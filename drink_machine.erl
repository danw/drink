-module (drink_machine).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_response/2]).
-export ([slots/1, slot_name/2, slot_available/2, slot_price/2, drop/2]).

-include ("drink_config.hrl").

-record (dmstate, {
			machineid,
			commpid,
			temperature,
			droppid,
			dropref,
			timerref,
			slot_table}).

start_link (MachineId, CommPid) ->
	gen_server:start_link({local, list_to_atom(lists:append("drink_machine_", atom_to_list(MachineId)))},
							?MODULE, {MachineId, CommPid}, []).

init ({MachineId, CommPid}) ->
	{ok, MachineTable} = dets:open_file(machine_table, [{auto_save, 10000}]),
	case dets:lookup(MachineTable, MachineId) of
		[MachineRecord] ->
			{MachineId, _Password, Slots} = MachineRecord,
			SlotTable = ets:new(slot_table, [set, private, {keypos, 2}]),
			{ok, {idle, #dmstate{machineid=MachineId,commpid=CommPid,slot_table=SlotTable}}};
		[] ->
			{stop, {error, invalid_machine}}
	end.

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
				{slot_status, _Status} ->
					% Update SlotTable
					{noreply, {idle, State}};
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
handle_call ({slot_name, Slot}, _From, {idle, State}) ->
	case ets:lookup(State#dmstate.slot_table,Slot) of
		[SlotInfo] ->
			#slot_info{name=Name} = SlotInfo,
			{reply, {ok, Name}, State};
		[] ->
			{reply, {error, invalid_slot}, State}
	end;
handle_call ({slot_available, Slot}, _From, {idle, State}) ->
	case ets:lookup(State#dmstate.slot_table,Slot) of
		[SlotInfo] ->
			#slot_info{avail=Avail} = SlotInfo,
			{reply, {ok, Avail}, State};
		[] ->
			{reply, {error, invalid_slot}, State}
	end;
handle_call ({slot_price, Slot}, _From, {idle, State}) ->
	case ets:lookup(State#dmstate.slot_table,Slot) of
		[SlotInfo] ->
			#slot_info{price=Price} = SlotInfo,
			{reply, {ok, Price}, State};
		[] ->
			{reply, {error, invalid_slot}, State}
	end;
handle_call ({slots}, _From, {idle, State}) ->
	{reply, ets:tab2list(State#dmstate.slot_table), State};
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

slot_name (MachinePid, Slot) when is_integer(Slot) ->
	gen_server:call(MachinePid, {slot_name, Slot}).

slot_available (MachinePid, Slot) when is_integer(Slot) ->
	gen_server:call(MachinePid, {slot_available, Slot}).

slot_price (MachinePid, Slot) when is_integer(Slot) ->
	gen_server:call(MachinePid, {slot_price, Slot}).

slots (MachinePid) ->
	gen_server:call(MachinePid, {slots}).