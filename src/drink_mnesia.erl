-module (drink_mnesia).
-export ([initialize/0, upgrade/0]).
-export ([log_drop/1, log_money/1, log_temperature/1]).

-include ("drink_mnesia.hrl").

initialize() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(machine, [
		{disc_copies, [node()]},
		{ram_copies, []},
		{record_name, machine},
		{index, [password]},
		{attributes, record_info(fields, machine)}]),
	mnesia:create_table(slot, [
		{type, bag},
		{disc_copies, [node()]},
		{ram_copies, []},
		{record_name, slot},
		{index, [num]},
		{attributes, record_info(fields, slot)}]),
	mnesia:create_table(temperature, [
		{type, bag},
		{disc_only_copies, [node()]},
		{ram_copies, []},
		{record_name, temperature},
		{index, [time]},
		{attributes, record_info(fields, temperature)}]),
	mnesia:create_table(money_log, [
		{type, bag},
		{disc_only_copies, [node()]},
		{ram_copies, []},
		{record_name, money_log},
		{index, [time, username, admin]},
		{attributes, record_info(fields, money_log)}]),
	mnesia:create_table(drop_log, [
		{type, bag},
		{disc_only_copies, [node()]},
		{ram_copies, []},
		{record_name, drop_log},
		{index, [time, username, slot, status]},
		{attributes, record_info(fields, drop_log)}]).

mysql_init() ->
    mysql:prepare(log_temperature, <<"INSERT INTO temperature_log VALUES (?, ?, ?)">>),
    mysql:prepare(log_money, <<"INSERT INTO money_log VALUES (?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(log_drop, <<"INSERT INTO drop_log VALUES (?, ?, ?, ?, ?)">>).

log_drop(Drop) ->
    Status = io_lib:format("~w", [Drop#drop_log.status]),
    case mysql:execute(drink_log, log_drop, [
                                Drop#drop_log.machine,
                                Drop#drop_log.slot,
                                Drop#drop_log.username,
                                Drop#drop_log.time,
                                Status]) of
        {error, {no_such_statement, log_drop}} ->
            mysql_init(),
            log_drop(Drop);
        {error, _MySqlRes} ->
            case mnesia:transaction(fun() -> mnesia:write(Drop) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Drop Log Error! ~p~n", Reason),
                    {error, Reason}
            end;
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok
    end.

log_money(Money) ->
    case mysql:execute(drink_log, log_money, [
                                Money#money_log.time,
                                Money#money_log.username,
                                Money#money_log.admin,
                                Money#money_log.amount,
                                Money#money_log.direction,
                                Money#money_log.reason]) of
        {error, {no_such_statement, log_money}} ->
            mysql_init(),
            log_money(Money);
        {error, _MySqlRes} ->
            case mnesia:transaction(fun() -> mnesia:write(Money) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Money Log Error! ~p~n", Reason),
                    {error, Reason}
            end;
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok
    end.

log_temperature(Temperature) ->
    case mysql:execute(drink_log, log_temperature, [
                                Temperature#temperature.machine,
                                Temperature#temperature.time,
                                Temperature#temperature.temperature]) of
        {error, {no_such_statement, log_temperature}} ->
            mysql_init(),
            log_temperature(Temperature);
        {error, _MySqlRes} ->
            case mnesia:transaction(fun() -> mnesia:write(Temperature) end) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Temperature Log Error! ~p~n", Reason),
                    {error, Reason}
            end;
        {updated, _MySqlRes} ->
            % todo: check if there are records in mnesia to move to mysql
            ok
    end.

upgrade() ->
	%{atomic, ok} = mnesia:transform_table(machine, fun upgrade_machines/1, record_info(fields, machine), machine),
	%{atomic, ok} = mnesia:transform_table(slot, fun upgrade_slots/1, record_info(fields, slot), slot),
	%{atomic, ok} = mnesia:transform_table(temperature, fun upgrade_temperatures/1, record_info(fields, slot), slot).
	ok.

% upgrade_machines(X) ->
% 	X.
% 
% upgrade_slots(X) ->
% 	X.
% 
% upgrade_temperatures(X) ->
% 	X.