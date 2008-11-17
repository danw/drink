-module (drink_mnesia).
-export ([initialize/0, upgrade/0]).

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