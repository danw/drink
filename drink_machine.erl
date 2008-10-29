-module (drink_machine).

-export ([start_link/2]).
-export ([init/3]).
-export ([system_continue/3, system_terminate/4]).
-export ([got_response/2]).
-export ([drop/2]).

-record (dmstate, {
			machineid,
			commpid,
			temperature}).

start_link (MachineId, CommPid) ->
	proc_lib:start_link(?MODULE, init, [self(), MachineId, CommPid]).

init (Parent, MachineId, CommPid) ->
	Name = list_to_atom(lists:append(["drink_machine_", atom_to_list(MachineId)])),
	register(Name, self()),			% Register as drink_machine_(id)
	Debug = sys:debug_options([]),
	proc_lib:init_ack(Parent, {ok, self()}),
	loop({idle, #dmstate{machineid=MachineId,commpid=CommPid}}, Parent, Debug).

loop ({idle, State}, Parent, Debug) ->
	#dmstate{commpid=CommPid} = State,
	receive
		{got_response, CommPid, {temperature, _DateTime, Temperature}} ->
			io:format("Got temperature: ~p~n", [Temperature]),
			loop({idle, State#dmstate{temperature=Temperature}}, Parent, Debug);
		{got_response, CommPid, drop_ack} ->
			error_logger:error_msg("Drop Ack Received while not dropping!"),
			loop({idle, State}, Parent, Debug);
		{got_response, CommPid, drop_nack} ->
			error_logger:error_msg("Drop Nack Received while not dropping!"),
			loop({idle, State}, Parent, Debug);
		{got_response, CommPid, Response} ->
			io:format("Unknown response: ~p~n", [Response]),
			loop({idle, State}, Parent, Debug);
		{timeout, _Tref} ->
			loop({idle, State}, Parent, Debug);
		{api, drop, RetPid, DropRef, Slot} ->
			case timer:send_after(timer:seconds(30), {timeout, DropRef}) of
				{ok, Timer} ->
					drink_machine_comm:send_command(CommPid, {drop, Slot}),
					loop({dropping, RetPid, Timer, DropRef, State}, Parent, Debug);
				{error, Reason} ->
					RetPid ! {drop_result, DropRef, error, timer_err},
					error_logger:error_msg("Timer couldn't be created for ~p drop(~p)", 
										   [State#dmstate.machineid,Reason]),
					loop({idle, State}, Parent, Debug)
			end;
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, {idle, State})
	end;

loop ({dropping, DropPid, Timer, DropRef, State}, Parent, Debug) ->
	#dmstate{commpid=CommPid} = State,
	receive
		{got_response, CommPid, drop_ack} ->
			timer:cancel(Timer),
			DropPid ! {drop_result, DropRef, ok},
			io:format("Got drop ack~n"),
			loop({idle, State}, Parent, Debug);
		{got_response, CommPid, drop_nack} ->
			timer:cancel(Timer),
			DropPid ! {drop_result, DropRef, error, drop_nack},
			error_logger:error_msg("Drop Nack received from ~p~n", [State#dmstate.machineid]),
			loop({idle, State}, Parent, Debug);
		{timeout, DropRef} ->
			DropPid ! {drop_result, DropRef, error, timeout},
			error_logger:error_msg("Drop timed out on ~p~n", [State#dmstate.machineid]),
			loop({idle, State}, Parent, Debug);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug,
								  {dropping, DropPid, Timer, DropRef, State})
	end.

system_continue (Parent, Debug, State) ->
	loop(State, Parent, Debug).

system_terminate (Reason, _Parent, _Debug, _State) ->
	exit(Reason).

% Callback from Drink Machine Comm
got_response (MachinePid, Response) ->
	MachinePid ! {got_response, self(), Response}.

% API
drop (MachinePid, Slot) when is_integer(Slot) ->
	Ref = make_ref(),
	MachinePid ! {api, drop, self(), Ref, Slot},
	receive
		{drop_result, Ref, ok} ->
			ok;
		{drop_result, Ref, error, Reason} ->
			{error, Reason}
	end.