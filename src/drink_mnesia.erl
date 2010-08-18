%%%-------------------------------------------------------------------
%%% File    : drink_mnesia.erl
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

-module (drink_mnesia).
-export ([initialize/0]).
-export ([log_drop/1, log_money/1, log_temperature/1]).
-export ([get_logs/2, get_logs/3, get_temps/2]).

-include ("drink_mnesia.hrl").
-include ("user.hrl").

initialize() ->
    case filelib:is_file("mnesia_data/schema.DAT") of
        false -> mnesia:create_schema([node()]);
        true ->  ok
    end,
    ok = mnesia:start(),
    case mnesia:create_table(machine, [
        {disc_copies, [node()]},
        {ram_copies, []},
        {record_name, machine},
        {index, [password]},
        {attributes, record_info(fields, machine)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        E -> error_logger:error_msg("Got mnesia error: ~p~n", [E])
    end,
    case mnesia:create_table(slot, [
        {type, bag},
        {disc_copies, [node()]},
        {ram_copies, []},
        {record_name, slot},
        {index, [num]},
        {attributes, record_info(fields, slot)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Er -> error_logger:error_msg("Got mnesia error: ~p~n", [Er])
    end,
    case mnesia:create_table(temperature, [
        {type, bag},
        {disc_only_copies, [node()]},
        {ram_copies, []},
        {record_name, temperature},
        {index, [time]},
        {attributes, record_info(fields, temperature)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Err -> error_logger:error_msg("Got mnesia error: ~p~n", [Err])
    end,
    case mnesia:create_table(money_log, [
        {type, bag},
        {disc_only_copies, [node()]},
        {ram_copies, []},
        {record_name, money_log},
        {index, []},
        {attributes, record_info(fields, money_log)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Erro -> error_logger:error_msg("Got mnesia error: ~p~n", [Erro])
    end,
    case mnesia:create_table(drop_log, [
        {type, bag},
        {disc_only_copies, [node()]},
        {ram_copies, []},
        {record_name, drop_log},
        {index, [time, username, slot, status]},
        {attributes, record_info(fields, drop_log)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Error -> error_logger:error_msg("Got mnesia error: ~p~n", [Error])
    end.

mysql_init() ->
    mysql:prepare(log_temperature, <<"INSERT INTO temperature_log VALUES (?, ?, ?)">>),
    mysql:prepare(log_money, <<"INSERT INTO money_log VALUES (?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(log_drop, <<"INSERT INTO drop_log VALUES (?, ?, ?, ?, ?)">>),
    mysql:prepare(get_logs, <<"SELECT * FROM (
        SELECT \"money\" as type, m.time as time, m.username, m.admin, m.direction, m.reason, m.amount FROM money_log as m
        union
        SELECT \"drop\" as type, d.time as time, d.username, d.machine, d.slot, d.status, 0 FROM drop_log as d) AS log
        ORDER BY log.time DESC LIMIT ?, ?">>),
    mysql:prepare(get_logs_user, <<"SELECT * FROM (
        SELECT \"money\" as type, m.time as time, m.username as u, m.admin, m.direction, m.reason, m.amount
            FROM money_log as m
        union
        SELECT \"drop\" as type, d.time as time, d.username as u, d.machine, d.slot, d.status, 0
            FROM drop_log as d) AS log
        WHERE u = ? ORDER BY log.time DESC LIMIT ?, ?">>),
    mysql:prepare(get_temps, <<"SELECT * FROM temperature_log WHERE time > ? AND time < ? ORDER BY time ASC">>).

log_drop(Drop) ->
    Status = io_lib:format("~w", [Drop#drop_log.status]),
    case catch mysql:execute(drink_log, log_drop, [
                                Drop#drop_log.machine,
                                Drop#drop_log.slot,
                                Drop#drop_log.username,
                                Drop#drop_log.time,
                                Status]) of
        {error, {no_such_statement, log_drop}} ->
            mysql_init(),
            log_drop(Drop);
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok;
        _E ->
            case mnesia:transaction(fun() -> mnesia:write(Drop) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Drop Log Error! ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

log_money(Money) ->
    Admin = case Money#money_log.admin of
        nil ->
            null;
        Else ->
            Else
    end,
    case catch mysql:execute(drink_log, log_money, [
                                Money#money_log.time,
                                Money#money_log.username,
                                Admin,
                                Money#money_log.amount,
                                Money#money_log.direction,
                                Money#money_log.reason]) of
        {error, {no_such_statement, log_money}} ->
            mysql_init(),
            log_money(Money);
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok;
        _E ->
            case mnesia:transaction(fun() -> mnesia:write(Money) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Money Log Error! ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

log_temperature(Temperature) ->
    case catch mysql:execute(drink_log, log_temperature, [
                                Temperature#temperature.machine,
                                Temperature#temperature.time,
                                Temperature#temperature.temperature]) of
        {error, {no_such_statement, log_temperature}} ->
            mysql_init(),
            log_temperature(Temperature);
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok;
        _E ->
            case mnesia:transaction(fun() -> mnesia:write(Temperature) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Temperature Log Error! ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

get_logs(UserRef, Index, Count) when is_reference(UserRef), is_integer(Index), is_integer(Count) ->
    case {user_auth:can_admin(UserRef), user_auth:user_info(UserRef)} of
        {false, {ok, UserInfo = #user{}}} ->
            case catch mysql:execute(drink_log, get_logs_user, [UserInfo#user.username, Index, Count]) of
                {error, {no_such_statement, get_logs_user}} ->
                    mysql_init(),
                    get_logs(UserRef, Index, Count);
                {error, _MySqlRes} ->
                    {error, mysql};
                {data, MySqlRes} ->
                    {ok, lists:map(fun format_log/1, mysql:get_result_rows(MySqlRes))}
            end;
        {false, {error, Reason}} ->
            {error, Reason};
        {true, _} ->
            get_logs(Index, Count)
    end.

get_logs(Index, Count) when is_integer(Index), is_integer(Count) ->
    case catch mysql:execute(drink_log, get_logs, [Index, Count]) of
        {error, {no_such_statement, get_logs}} ->
            mysql_init(),
            get_logs(Index, Count);
        {error, _MySqlRes} ->
            {error, mysql};
        {data, MySqlRes} ->
            {ok, lists:map(fun format_log/1, mysql:get_result_rows(MySqlRes))}
    end.

get_temps(Since, Seconds) when is_tuple(Since), is_integer(Seconds) ->
    Until = calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds(Since)),
    case catch mysql:execute(drink_log, get_temps, [Since, Until]) of
        {error, {no_such_statement, get_temps}} ->
            mysql_init(),
            get_temps(Since, Seconds);
        {error, _MySqlRes} ->
            {error, mysql};
        {data, MySqlRes} ->
            {ok, lists:map(fun format_temp/1, mysql:get_result_rows(MySqlRes))}
    end.

format_log([<<"money">>, {datetime, Time}, User, Admin, Direction, Reason, Amount]) ->
    AdminUser = case Admin of
        undefined ->
            null;
        Else ->
            binary_to_list(Else)
    end,
    #money_log{
        time = Time,
        username = binary_to_list(User),
        admin = AdminUser,
        direction = list_to_atom(binary_to_list(Direction)),
        reason = list_to_atom(binary_to_list(Reason)),
        amount = Amount
    };
format_log([<<"drop">>, {datetime, Time}, User, Machine, Slot, Status, 0]) ->
    #drop_log{
        time = Time,
        username = binary_to_list(User),
        machine = list_to_atom(binary_to_list(Machine)),
        slot = binary_to_list(Slot),
        status = binary_to_list(Status)
    }.

format_temp([Machine, {datetime, Time}, Temp]) ->
    #temperature{
        machine = list_to_atom(binary_to_list(Machine)),
        time = Time,
        temperature = Temp
    }.
