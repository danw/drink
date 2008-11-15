-module (drink_machine).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_response/2]).
-export ([slots/1, slot_name/2, slot_available/2, slot_price/2, drop/2]).

-include ("drink_mnesia.hrl").
-include ("qlc.hrl").

-record (dmstate, {
			machineid,
			commpid,
			droppid,
			dropref,
			dropslotnum,
			timerref,
			record}).

start_link (MachineId, CommPid) ->
	gen_server:start_link({local, list_to_atom(lists:append("drink_machine_", atom_to_list(MachineId)))},
							?MODULE, {MachineId, CommPid}, []).

init ({MachineId, CommPid}) ->
	case mnesia:transaction(fun() -> mnesia:read({machine, MachineId}) end) of
		{atomic, [MachineRecord]} ->
			State = #dmstate{
				machineid = MachineId,
				commpid = CommPid,
				record = MachineRecord
			},
			{ok, {idle, State}};
		{atomic, []} ->
			{stop, {error, invalid_machine}};
		{aborted, Reason} ->
			{stop, {error, Reason}}
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
					T = #temperature{
						machine = State#dmstate.machineid, 
						time = erlang:universaltime(),
						temperature = Temperature
					},
					case mnesia:transaction(fun() -> mnesia:write(T) end) of
						{aborted, Reason} ->
							error_logger:error_msg("mnesia Temperature Write failed: ~p", Reason);
						{atomic, ok} ->
							ok
					end,
					{noreply, {idle, State}};
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
					decrement_slot_avail(State#dmstate.dropslotnum, State),
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
	case get_slot_by_num(Slot, State) of
		{ok, SlotInfo} when SlotInfo#slot.avail > 0 ->
			case timer:send_after(timer:seconds(30), {timeout, DropRef}) of
				{ok, Timer} ->
					drink_machine_comm:send_command(State#dmstate.commpid, {drop, Slot}),
					{noreply, {dropping, State#dmstate{timerref=Timer,dropref=DropRef,droppid={DropPid, DropRef},dropslotnum=Slot}}};
				{error, Reason} ->
					error_logger:error_msg("Timer couldn't be created for ~p drop(~p)",
											[State#dmstate.machineid,Reason]),
					{reply, {error, timer_err}, {idle, State}}
			end;
		{ok, _SlotInfo} ->
			{reply, {error, not_available}, {idle, State}};
		{error, Reason} ->
			{reply, {error, Reason}, {idle, State}}
	end;
handle_call ({slot_name, Slot}, _From, {idle, State}) ->
	case get_slot_by_num(Slot, State) of
		{ok, SlotInfo} ->
			{reply, {ok, SlotInfo#slot.name}, {idle, State}};
		{error, Reason} ->
			{reply, {error, Reason}, {idle, State}}
	end;
handle_call ({slot_available, Slot}, _From, {idle, State}) ->
	case get_slot_by_num(Slot, State) of
		{ok, SlotInfo} ->
			{reply, {ok, SlotInfo#slot.avail}, {idle, State}};
		{error, Reason} ->
			{reply, {error, Reason}, {idle, State}}
	end;
handle_call ({slot_price, Slot}, _From, {idle, State}) ->
	case get_slot_by_num(Slot, State) of
		{ok, SlotInfo} ->
			{reply, {ok, SlotInfo#slot.price}, {idle, State}};
		{error, Reason} ->
			{reply, {error, Reason}, {idle, State}}
	end;
handle_call ({slots}, _From, {idle, State}) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid ]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, List} ->
			{reply, {ok, List}, {idle, State}};
		{aborted, Reason} ->
			{reply, {error, Reason}, {idle, State}}
	end;
handle_call (_Request, _From, State) ->
	{reply, {error, unknown}, State}.

handle_info ({timeout, _DropRef}, {idle, State}) ->
	{noreply, {idle, State}};
handle_info ({timeout, DropRef}, {dropping, State}) ->
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

% Internal Helpers
get_slot_by_num(Num, State) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid, X#slot.num =:= Num ]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, [Slot]} ->
			{ok, Slot};
		{atomic, []} ->
			{error, invalid_slot};
		{aborted, Reason} ->
			{error, Reason}
	end.

decrement_slot_avail(SlotNum, State) ->
	Qslot = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid, X#slot.num =:= SlotNum ]),
	F = fun() ->
		[Slot] = qlc:eval(Qslot),
		NewSlotInfo = Slot#slot{avail = Slot#slot.avail - 1},
		mnesia:write(NewSlotInfo)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.