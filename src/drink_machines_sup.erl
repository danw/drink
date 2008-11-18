-module (drink_machines_sup).
-behaviour (supervisor).

-export ([start_link/0, init/1]).
-export ([machine_connected/2, machines/0, is_machine/1, is_machine_alive/1]).

-include ("drink_mnesia.hrl").
-include ("qlc.hrl").

start_link () ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init ([]) ->
	{ok, {{simple_one_for_one, 3, 10}, % One for one restart, dies completely after 3 times in 10 seconds
		  [{machine,
			{drink_machine, start_link, []},
			temporary,
			brutal_kill,
			worker,
			[drink_machine]}]
		}}.

machine_connected (MachineId, CommPid) ->
	supervisor:start_child(?MODULE, [MachineId, CommPid]).

machine_names([{_,Pid,_,_}|T]) ->
	{registered_name, Name} = process_info(Pid, registered_name),
	lists:append([Name], machine_names(T));
machine_names([]) ->
	[].

machines () ->
	machine_names(supervisor:which_children(?MODULE)).

is_machine (MachineId) ->
	case mnesia:transaction(fun() -> mnesia:read({machine, MachineId}) end) of
		{atomic, [_MachineRec]} ->
			true;
		{atomic, []} ->
			false;
		{aborted, _Reason} ->
			false
	end.

is_machine_alive (MachineId) ->
	case whereis(MachineId) of
		undefined ->
			false;
		Pid ->
			is_process_alive(Pid)
	end.