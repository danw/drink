-module (drink_mnesia).
-export ([initialize/0]).
-export ([log_drop/1, log_money/1, log_temperature/1]).
-export ([get_logs/2]).

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
    mysql:prepare(log_drop, <<"INSERT INTO drop_log VALUES (?, ?, ?, ?, ?)">>),
    mysql:prepare(get_logs, <<"SELECT * FROM (
    SELECT \"money\" as type, m.time as time, m.username, m.admin, m.direction, m.reason, m.amount FROM money_log as m
    union
    SELECT \"drop\" as type, d.time as time, d.username, d.machine, d.slot, d.status, 0 FROM drop_log as d) AS log
    ORDER BY log.time DESC LIMIT ?, ?">>).

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

get_logs(Index, Count) when is_integer(Index), is_integer(Count) ->
    case mysql:execute(drink_log, get_logs, [Index, Count]) of
        {error, {no_such_statement, get_logs}} ->
            mysql_init(),
            get_logs(Index, Count);
        {error, _MySqlRes} ->
            {error, mysql};
        {data, MySqlRes} ->
            {ok, lists:map(fun format_log/1, mysql:get_result_rows(MySqlRes))}
    end.

format_log([<<"money">>, {datetime, Time}, User, Admin, Direction, Reason, Amount]) ->
    #money_log{
        time = Time,
        username = binary_to_list(User),
        admin = binary_to_list(Admin),
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