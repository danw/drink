%%%-------------------------------------------------------------------
%%% File    : drink_web.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2008 Dan Willemsen
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

-module (drink_web).

-export ([out/1]).

-include ("yaws_api.hrl").
-include ("user.hrl").
-include ("drink_mnesia.hrl").

out(A) ->
    User = case lists:keymember(webauth_keytab, 1, A#arg.opaque) of
        true ->
            case authmod_webauth:auth(A, undefined) of
                {true, {WebAuthUser, _, _}} -> {ok, WebAuthUser, []};
                _ -> error
            end;
        false ->
            case yaws_api:find_cookie_val("drink_user", (A#arg.headers)#headers.cookie) of
                [] ->
                    case {A#arg.appmoddata, yaws_api:queryvar(A, "user")} of
                        {"setusercookie", {ok, CookieUser}} ->
                            Cookie = yaws_api:setcookie("drink_user", CookieUser, "/"),
                            {ok, CookieUser, [Cookie]};
                        _ -> error
                    end;
                CookieUser -> {ok, CookieUser, []}
            end
    end,
    case User of
        {ok, UserName, Headers} ->
            case user_auth:auth({webauth, UserName}) of
                {ok, UserRef} ->
                    case request(A, UserRef, (A#arg.req)#http_request.method, list_to_atom(A#arg.appmoddata)) of
                        {websocket, OwnerPid, SocketMode} ->
                            {websocket, OwnerPid, SocketMode};
                        Ret ->
                            user_auth:delete_ref(UserRef),
                            Headers ++ Ret
                    end;
                _ -> Headers ++ error(login_failed)
            end;
        _ -> error(login_failed)
    end.

request(_, U, 'GET', currentuser) ->
    userref_to_struct(U);
request(_, _, _, currentuser) -> error(wrong_method);

request(A, U, 'POST', drop) ->
    case {yaws_api:postvar(A, "machine"), yaws_api:postvar(A, "slot")} of
        {{ok, Machine}, {ok, Slot}} ->
            case string:to_integer(Slot) of
                {error, _Reason} ->
                    error(invalid_args);
                {SlotNum, _} ->
                    case user_auth:drop(U, list_to_atom(Machine), SlotNum) of
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
                    end
            end;
        _ ->
            error(invalid_args)
    end;
request(_, _, _, drop) -> error(wrong_method);

request(_, U, 'GET', events) ->
    {ok, Pid, SocketMode} = drink_web_events:register(U, [temperature, drop]),
    {websocket, Pid, SocketMode};
request(_, _, _, events) -> error(wrong_method);

request(A, U, 'GET', logs) ->
    case {yaws_api:queryvar(A, "offset"), yaws_api:queryvar(A, "limit")} of
        {{ok, OffsetStr}, {ok, LimitStr}} ->
            case {string:to_integer(OffsetStr), string:to_integer(LimitStr)} of
                {{error, _Reason}, _} ->
                    error(invalid_args);
                {_, {error, _Reason}} ->
                    error(invalid_args);
                {{Offset, _Rest}, {Limit, _Rest}} when Offset >= 0, Limit =< 100 ->
                    case drink_mnesia:get_logs(U, Offset, Limit) of
                        {ok, Data} ->
                            ok(format_logs(Offset, Limit, Data));
                        {error, Reason} ->
                            error(Reason)
                    end;
                _ ->
                    error(invalid_args)
            end;
        _ ->
            error(invalid_args)
    end;
request(_, _, _, logs) -> error(wrong_method);

request(_, U, 'GET', machines) ->
    ok({struct, machines(user_auth:can_admin(U), drink_machines_sup:machines())});
request(_, _, _, machines) -> error(wrong_method);

request(A, U, 'POST', moduser) ->
    case {yaws_api:postvar(A, "username"),
          yaws_api:postvar(A, "attr"),
          yaws_api:postvar(A, "value"),
          yaws_api:postvar(A, "reason")} of
        {{ok, UserName}, {ok, Attr}, {ok, Value}, ModReason} ->
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
request(_, _, _, moduser) -> error(wrong_method);

request(A, U, 'POST', setslot) ->
    case {yaws_api:postvar(A, "machine"), 
          yaws_api:postvar(A, "slot"),
          yaws_api:postvar(A, "name"),
          yaws_api:postvar(A, "price"),
          yaws_api:postvar(A, "available"),
          yaws_api:postvar(A, "disabled")} of
        {{ok, Machine}, {ok, SlotStr}, {ok, Name}, {ok, PriceStr}, {ok, AvailStr}, {ok, DisabledStr}} ->
            case {string:to_integer(SlotStr),
                  string:to_integer(PriceStr),
                  string:to_integer(AvailStr),
                  list_to_atom(DisabledStr)} of
                {{error, _}, _, _, _} ->
                    error(invalid_args);
                {_, {error, _}, _, _} ->
                    error(invalid_args);
                {_, _, {error, _}, _} ->
                    error(invalid_args);
                {{Slot, _}, {Price, _}, {Avail, _}, Disabled} when is_boolean(Disabled) ->
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
        _ ->
            error(invalid_args)
    end;
request(_, _, _, setslot) -> error(wrong_method);

request(A, _, 'GET', temperatures) ->
    case {yaws_api:queryvar(A, "from"), yaws_api:queryvar(A, "length")} of
        {{ok, FromStr}, {ok, LengthStr}} ->
            case {string:to_integer(FromStr), string:to_integer(LengthStr)} of
                {{error, _Reason}, _} ->
                    error(invalid_args);
                {_, {error, _Reason}} ->
                    error(invalid_args);
                {{From, _Rest}, {Limit, _Rest}} when From >= 0, Limit >= 0, Limit =< 100000 ->
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
        _ ->
            error(invalid_args)
    end;
request(_, _, _, temperatures) -> error(wrong_method);

request(A, U, 'GET', userinfo) ->
    case yaws_api:queryvar(A, "user") of
        {ok, UserName} ->
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
request(_, _, _, userinfo) -> error(wrong_method);

request(A, U, 'POST', addmachine) ->
    case user_auth:can_admin(U) of
	true ->
	    case {postvar(A, atom, "machine"),
		  postvar(A, string, "name"),
		  postvar(A, atom, "password"),
		  postvar(A, ip, "public_ip"),
		  postvar(A, atom, "available_sensor"),
		  postvar(A, ip, "machine_ip"),
		  postvar(A, atom, "allow_connect"),
		  postvar(A, atom, "admin_only")} of
		{{ok, MachineAtom},
		 {ok, MachineName},
		 {ok, MachinePassword},
		 {ok, MachinePublicIP},
		 {ok, MachineAvailableSensor},
		 {ok, MachineIP},
		 {ok, MachineAllowConnect},
		 {ok, MachineAdminOnly}} ->
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
request(_, _, _, addmachine) -> error(wrong_method);

request(_, _, 'GET', _) ->
    error(unknown_path);
request(_, _, 'POST', _) ->
    error(unknown_path);
request(_, _, _, _) ->
    error(wrong_method).

ok(Data) ->
    [{content, "application/json", json:encode({struct, [{status, "ok"}, {data, Data}]})}].

error(Reason) when is_atom(Reason) ->
    error(atom_to_list(Reason));
error(Reason) ->
    [{content, "application/json", json:encode({struct, [{status, "error"}, {reason, Reason}]})}].

postvar(A, ip, Name) ->
    case yaws_api:postvar(A, Name) of
	{ok, IP} -> inet:getaddr(IP, inet);
	E -> E
    end;
postvar(A, atom, Name) ->
    case yaws_api:postvar(A, Name) of
	{ok, Atom} -> {ok, list_to_atom(Atom)};
	E -> E
    end;
postvar(A, _, Name) -> yaws_api:postvar(A, Name).

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
                {temperature, machine_attr(Machine, temperature, false)},
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
                {temperature, machine_attr(Machine, temperature, false)},
                {password, machine_attr(Machine, password, false)},
                {public_ip, ip_to_list(machine_attr(Machine, public_ip, false))},
                {available_sensor, machine_attr(Machine, available_sensor, false)},
                {machine_ip, ip_to_list(machine_attr(Machine, machine_ip, false))},
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
            atom_to_list(Machine)
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
mod_user(UserRef, modcredits, Value, {ok, ModReason}) when is_integer(Value) ->
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
