-module (drink_machine).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_response/2]).
-export ([drop/2]).

-record (dmstate, {
			machineid,
			commpid,
			temperature,
			droppid,
			dropref,
			timerref}).

start_link (MachineId, CommPid) ->
	gen_server:start_link({local, list_to_atom(lists:append("drink_machine_", atom_to_list(MachineId)))},
							?MODULE, {MachineId, CommPid}, []).

init ({MachineId, CommPid}) ->
	{ok, {idle, #dmstate{machineid=MachineId,commpid=CommPid}}}.

terminate (_Reason, _State) ->
	ok.

code_change (_OldVsn, State, _Extra) when is_tuple(State) ->
	{ok, State}.

handle_cast ({got_response, CommPid, Response}, {idle, State}) ->
	case State#dmstate.commpid of
		CommPid ->
			case Response of
				drop_ack ->
					error_logger:error_msg("Drop Ack Received while not dropping!"),
					{noreply, {idle, State}};
				drop_nack ->
					error_logger:error_msg("Drop Nack Received while not dropping!"),
					{noreply, {idle, State}};
				{temperature, _DateTime, Temperature} ->
					io:format("Got temperature: ~p~n", [Temperature]),
					{noreply, {idle, State#dmstate{temperature=Temperature}}};
				Response ->
					io:format("Unknown response: ~p~n", [Response]),
					{noreply, {idle, State}}
			end;
		_Else ->
			{noreply, {idle, State}}
	end;
handle_cast ({got_response, CommPid, Response}, {dropping, State}) ->
	case State#dmstate.commpid of
		CommPid ->
			case Response of
				drop_ack ->
					timer:cancel(State#dmstate.timerref),
					gen_server:reply(State#dmstate.droppid, {ok}),
					io:format("Got a drop ack~n"),
					{noreply, {idle, State}};
				drop_nack ->
					timer:cancel(State#dmstate.timerref),
					gen_server:reply(State#dmstate.droppid, {error, drop_nack}),
					io:format("Got a drop nack~n"),
					{noreply, {idle, State}}
			end;
		_Else ->
			{noreply, {dropping, State}}
	end;
handle_cast (_Request, State) ->
	{noreply, State}.

handle_call ({drop, Slot}, {DropPid, DropRef}, {idle, State}) ->
	case timer:send_after(timer:seconds(30), {timeout, DropRef}) of
		{ok, Timer} ->
			drink_machine_comm:send_command(State#dmstate.commpid, {drop, Slot}),
			{noreply, {dropping, State#dmstate{timerref=Timer,dropref=DropRef,droppid={DropPid, DropRef}}}};
		{error, Reason} ->
			error_logger:error_msg("Timer couldn't be created for ~p drop(~p)",
									[State#dmstate.machineid,Reason]),
			{reply, {error, timer_err}, {idle, State}}
	end;
handle_call (_Request, _From, State) ->
	{reply, {error, unknown}, State}.

handle_info ({timeout, _DropRef},				{idle, State}) ->
	{noreply, {idle, State}};
handle_info ({timeout, DropRef},				{dropping, State}) ->
	case State#dmstate.dropref of
		DropRef ->
			timer:cancel(State#dmstate.timerref),
			error_logger:error_msg("Drop timed out on ~p~n", [State#dmstate.machineid]),
			gen_server:reply(State#dmstate.droppid, {error, timeout})
	end,
	{noreply, {idle, State}}.

% Callback from Drink Machine Comm
got_response (MachinePid, Response) ->
	gen_server:cast(MachinePid, {got_response, self(), Response}).

% API
drop (MachinePid, Slot) when is_integer(Slot) ->
	gen_server:call(MachinePid, {drop, Slot}, infinity).