%%%-------------------------------------------------------------------
%%% File    : drink_json_api.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2008-2010 Dan Willemsen
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module (drink_json_api).

-export ([request/3]).
-export ([machine_stat/2]).

-include ("user.hrl").
-include ("drink_mnesia.hrl").

args(_A, []) ->
    [];
args(A, [H|T]) ->
    case lists:keyfind(H, 1, A) of
        false -> {error, {arg_missing, H}};
        {H, V} -> [V] ++ args(A, T)
    end.

request(U, currentuser, _) ->
    userref_to_struct(U);

request(U, drop, A) ->
    case args(A, [machine, slot]) of
        [Machine, Slot] ->
            case user_auth:drop(U, list_to_atom(Machine), Slot) of
                ok ->
                    ok(true);
                {error, permission_denied} ->
                    error(permission_denied);
                {error, slot_empty} ->
                    error(slot_empty);
                {error, machine_down} ->
                    error(machine_down);
                {error, Reason} ->
                    error(Reason);
                _Else ->
                    error(unknown)
            end;
        {error, {arg_missing, _}} ->
            error(missing_arg)
    end;

request(U, logs, A) ->
    case args(A, [offset, limit]) of
        [Offset, Limit] when Offset >= 0, Limit =< 100 ->
            case drink_mnesia:get_logs(U, Offset, Limit) of
                {ok, Data} ->
                    ok(format_logs(Offset, Limit, Data));
                {error, Reason} ->
                    error(Reason)
            end;
        {error, {arg_mising, _}} ->
            error(arg_missing);
        _ ->
            error(invalid_args)
    end;

request(U, machines, _) ->
    ok({struct, machines(user_auth:can_admin(U), drink_machines_sup:machines())});

request(U, moduser, A) ->
    case args(A, [username, attr, value, reason]) of
        [UserName, Attr, Value, ModReason] ->
            case user_auth:admin(U, UserName) of
                {ok, User} ->
                    mod_user(User, list_to_atom(Attr), Value, ModReason);
                {error, permission_denied} ->
                    error(permission_denied);
                {error, invalid_user} ->
                    error(invalid_user);
                {error, Reason} ->
                    error(Reason);
                _Else ->
                    error(unknown)
            end;
        _ ->
            error(invalid_args)
    end;

request(U, setslot, A) ->
    case args(A, [machine, slot, name, price, available, disabled]) of
        [Machine, Slot, Name, Price, Avail, Disabled] when is_boolean(Disabled) ->
            case drink_machine:set_slot_info(U, #slot{
                machine = list_to_atom(Machine),
                num = Slot,
                name = Name,
                price = Price,
                avail = Avail,
                disabled = Disabled
            }) of
                ok ->
                    ok({struct, machines(user_auth:can_admin(U), drink_machines_sup:machines())});
                {error, permission_denied} ->
                    error(permission_denied);
                {error, Reason} ->
                    error(Reason)
            end;
        _ ->
            error(invalid_args)
    end;

request(_U, temperatures, A) ->
    case args(A, [from, length]) of
        [From, Limit] when From >= 0, Limit >= 0, Limit =< 100000 ->
            FromSecs = From + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            FromDate = calendar:gregorian_seconds_to_datetime(FromSecs),
            case drink_mnesia:get_temps(FromDate, Limit) of
                {ok, Data} ->
                    ok(format_temps(From, Limit, Data));
                {error, Reason} ->
                    error(Reason)
            end;
        _ ->
            error(invalid_args)
    end;

request(U, userinfo, A) ->
    case args(A, [user]) of
        [UserName] ->
            case user_auth:user(U, UserName) of
                {ok, User} ->
                    Ret = userref_to_struct(User),
                    user_auth:delete_ref(User),
                    Ret;
                _Else ->
                    error(invalid_user)
            end;
        _Else ->
            error(invalid_args)
    end;

request(U, addmachine, A) ->
    case user_auth:can_admin(U) of
        true ->
            case args(A, [machine, name, password, public_ip, available_sensor, machine_ip, allow_connect, admin_only]) of
                [MachineAtom, MachineName, MachinePassword, MachinePublicIP, MachineAvailableSensor,
                 MachineIP, MachineAllowConnect, MachineAdminOnly] ->
                    case drink_machines_sup:add(#machine{machine = MachineAtom,
                                                         password = MachinePassword,
                                                         name = MachineName,
                                                         public_ip = MachinePublicIP,
                                                         available_sensor = MachineAvailableSensor,
                                                         machine_ip = MachineIP,
                                                         allow_connect = MachineAllowConnect,
                                                         admin_only = MachineAdminOnly}) of
                        ok -> ok(true);
                        _  -> error(unknown_error)
                    end;
                _ -> error(invalid_args)
            end;
        false ->
            error(permission_denied)
    end;

request(_, _, _) ->
    error(unknown_command).

ok(Data) ->
    {ok, Data}.

error(Reason) ->
    {error, Reason}.

machines(_Admin, []) ->
    [];
machines(Admin, [M|Machines]) when is_boolean(Admin) ->
    [{M, machine_stat(Admin, M)}] ++ machines(Admin, Machines).

machine_stat(false, Machine) ->
    case drink_machine:slots(Machine) of
        {ok, Slots} ->
            {struct, [
                {machineid, atom_to_list(Machine)},
                {name, machine_attr(Machine, name, atom_to_list(Machine))},
                {connected, drink_machine:is_alive(Machine)},
                {temperature, machine_attr(Machine, temperature, 0)},
                {slots, {struct, slots(Slots)}}
            ]};
        _Else ->
            false
    end;
machine_stat(true, Machine) ->
    case drink_machine:slots(Machine) of
        {ok, Slots} ->
            {struct, [
                {machineid, atom_to_list(Machine)},
                {name, machine_attr(Machine, name, atom_to_list(Machine))},
                {connected, drink_machine:is_alive(Machine)},
                {temperature, machine_attr(Machine, temperature, 0)},
                {password, machine_attr(Machine, password, "")},
                {public_ip, ip_to_list(machine_attr(Machine, public_ip, ""))},
                {available_sensor, machine_attr(Machine, available_sensor, false)},
                {machine_ip, ip_to_list(machine_attr(Machine, machine_ip, ""))},
                {allow_connect, machine_attr(Machine, allow_connect, false)},
                {admin_only, machine_attr(Machine, admin_only, false)},
                {slots, {struct, slots(Slots)}}
            ]};
        _Else ->
            false
    end.

machine_attr(Machine, Attr, Default) ->
    case drink_machine:Attr(Machine) of
        {ok, Name} ->
            Name;
        _Else ->
            Default
    end.

slots([]) ->
    [];
slots([S|Slots]) ->
    [{integer_to_list(S#slot.num),slot_info(S)}] ++ slots(Slots).

slot_info(Slot) ->
    {struct, [
        {name, Slot#slot.name},
        {price, Slot#slot.price},
        {available, Slot#slot.avail},
        {disabled, Slot#slot.disabled}
    ]}.

ip_to_list(false) ->
    false;
ip_to_list({A,B,C,D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A,B,C,D])).

userref_to_struct(UserRef) ->
    case user_auth:user_info(UserRef) of
        {ok, UserInfo} ->
            ok({struct, [
                {username, UserInfo#user.username},
                {credits, UserInfo#user.credits},
                {admin, UserInfo#user.admin},
                {ibuttons, {array, UserInfo#user.ibuttons}}
            ]});
        {error, _Reason} ->
            error(invalid_user)
    end.

mod_user(UserRef, admin, "true", ModReason) -> mod_user(UserRef, admin, true, ModReason);
mod_user(UserRef, admin, "false", ModReason) -> mod_user(UserRef, admin, false, ModReason);
mod_user(UserRef, admin, Value, _) when is_atom(Value) ->
    case user_auth:set_admin(UserRef, Value) of
        ok ->
            userref_to_struct(UserRef);
        {error, Reason} ->
            error(Reason)
    end;
mod_user(UserRef, modcredits, Value, ModReason) when is_list(Value) ->
    case string:to_integer(Value) of
        {error, _Reason} ->
            error(invalid_args);
        {IntValue, _Rest} ->
            mod_user(UserRef, modcredits, IntValue, ModReason)
    end;
mod_user(UserRef, modcredits, Value, ModReason) when is_integer(Value) ->
    case user_auth:mod_credits(UserRef, Value, list_to_atom(ModReason)) of
        ok ->
            userref_to_struct(UserRef);
        {error, Reason} ->
            error(Reason)
    end;
mod_user(UserRef, delibutton, Value, _ModReason) ->
    case user_auth:del_ibutton(UserRef, Value) of
        ok ->
            userref_to_struct(UserRef);
        {error, Reason} ->
            error(Reason)
    end;
mod_user(UserRef, addibutton, Value, _ModReason) ->
    case user_auth:add_ibutton(UserRef, Value) of
        ok ->
            userref_to_struct(UserRef);
        {error, Reason} ->
            error(Reason)
    end;
mod_user(_, _, _, _) ->
    error(invalid_args).

format_logs(Start, Length, Data) ->
    {struct, [{start, Start}, {length, Length}, {lines, {array, lists:map(fun format_log/1, Data)}}]}.

format_log(Line = #money_log{}) ->
    {struct, [
        {type, "money"},
        {time, calendar:datetime_to_gregorian_seconds(Line#money_log.time) - 
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}})},
        {username, Line#money_log.username},
        {admin, Line#money_log.admin},
        {amount, Line#money_log.amount},
        {direction, atom_to_list(Line#money_log.direction)},
        {reason, atom_to_list(Line#money_log.reason)}
    ]};
format_log(Line = #drop_log{}) ->
    {struct, [
        {type, "drop"}, 
        {machine, atom_to_list(Line#drop_log.machine)},
        {slot, Line#drop_log.slot},
        {username, Line#drop_log.username},
        {time, calendar:datetime_to_gregorian_seconds(Line#drop_log.time) - 
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}})},
        {status, Line#drop_log.status}
    ]}.

format_temps(Start, Length, Data) -> % TODO: don't hardcode bigdrink and littledrink
    {struct, [{start, Start}, {length, Length}, {machines, {struct, [
        {bigdrink, {array, lists:map(fun format_temp/1,
            lists:filter(fun(E) -> E#temperature.machine =:= bigdrink end, Data))}},
        {littledrink, {array, lists:map(fun format_temp/1,
            lists:filter(fun(E) -> E#temperature.machine =:= littledrink end, Data))}}]}}]}.

format_temp(Temp = #temperature{}) ->
    {array, [calendar:datetime_to_gregorian_seconds(Temp#temperature.time) - 
     calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}), Temp#temperature.temperature]}.
