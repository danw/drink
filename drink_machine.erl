-module (drink_machine).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_response/2]).
-export ([slots/1, drop/2, temperature/1, slot_info/2]).

-include ("drink_mnesia.hrl").
-include ("qlc.hrl").

-record (dmstate, {
			machineid,
			commpid,
			droppid,
			dropref,
			dropslotnum,
			timerref,
			record,
			latest_temp = nil}).

start_link (MachineId, CommPid) ->
	gen_server:start_link({local, MachineId}, ?MODULE, {MachineId, CommPid}, []).

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

handle_cast (_Request, State) ->
	{noreply, State}.

handle_call ({drop, Slot}, {DropPid, DropRef}, {idle, State}) ->
	case get_slot_by_num(Slot, State) of
		{ok, SlotInfo} when SlotInfo#slot.avail > 0 ->
			case timer:send_after(timer:seconds(30), {timeout, DropRef}) of
				{ok, Timer} ->
					CommPid = State#dmstate.commpid,
					drink_machine_comm:send_command(CommPid, {drop, Slot}),
					receive
						{timeout, DropRef} ->
							timer:cancel(Timer),
							error_logger:error_msg("Drop timed out on ~p~n", [State#dmstate.machineid]),
							{reply, {error, timeout}, {idle, State}};
						{got_response, CommPid, drop_ack} ->
							timer:cancel(Timer),
							decrement_slot_avail(Slot, State),
							{reply, {ok}, {idle, State}};
						{got_response, CommPid, drop_nack} ->
							timer:cancel(Timer),
							{reply, {error, drop_nack}, {idle, State}}
					end;
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
handle_call ({slots}, _From, {S, State}) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid ]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, List} ->
			{reply, {ok, List}, {S, State}};
		{aborted, Reason} ->
			{reply, {error, Reason}, {S, State}}
	end;
handle_call ({slot_info, SlotNum}, _From, {S, State}) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid, X#slot.num =:= SlotNum]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, [SlotInfo]} ->
			{reply, {ok, SlotInfo}, {S, State}};
		{atomic, _List} ->
			{reply, {error, invalid_slot}, {S, State}};
		{aborted, Reason} ->
			{reply, {error, Reason}, {S, State}}
	end;
handle_call ({temp}, _From, {S, State}) ->
	case State#dmstate.latest_temp of
		nil ->
			{reply, {error, no_temp}, {S, State}};
		Temp ->
			{reply, {ok, Temp}, {S, State}}
	end;
handle_call (_Request, _From, State) ->
	{reply, {error, unknown}, State}.

handle_info ({got_response, CommPid, Response}, {idle, State}) ->
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
					{noreply, {idle, State#dmstate{latest_temp = Temperature}}};
				{slot_status, Status} ->
					update_slot_status(Status, State),
					{noreply, {idle, State}};
				Response ->
					io:format("Unknown response: ~p~n", [Response]),
					{noreply, {idle, State}}
			end;
		_Else ->
			{noreply, {idle, State}}
	end;
handle_info ({timeout, _DropRef}, {idle, State}) ->
	{noreply, {idle, State}};
handle_info (_, State) ->
	{noreply, {idle, State}}.

% Callback from Drink Machine Comm
got_response (MachinePid, Response) ->
	MachinePid ! {got_response, self(), Response}.

% API Utility Functions
safe_gen_call(MachinePid, Args, Timeout) ->
	case catch gen_server:call(MachinePid, Args, Timeout) of
		{'EXIT', {noproc, _}} ->
			case drink_machines_sup:is_machine(MachinePid) of
				true ->
					{error, machine_down};
				false ->
					{error, invalid_machine}
			end;
		Out ->
			Out
	end.
safe_gen_call(MachinePid, Args) ->
	case catch gen_server:call(MachinePid, Args) of
		{'EXIT', {noproc, _}} ->
			case drink_machines_sup:is_machine(MachinePid) of
				true ->
					{error, machine_down};
				false ->
					{error, invalid_machine}
			end;
		Out ->
			Out
	end.

% API
drop (MachinePid, Slot) when is_integer(Slot) ->
	safe_gen_call(MachinePid, {drop, Slot}, infinity).

slots (MachinePid) ->
	safe_gen_call(MachinePid, {slots}).

slot_info (MachinePid, Slot) when is_integer(Slot) ->
	safe_gen_call(MachinePid, {slot_info, Slot}).

temperature (MachinePid) ->
	safe_gen_call(MachinePid, {temp}).

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
		mnesia:delete_object(Slot),
		mnesia:write(NewSlotInfo)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

update_slot_status(Status, State) ->
	case mnesia:transaction(fun() ->
		mnesia:write_lock_table(slot),
		update_slot_status_mnesia(State#dmstate.machineid, Status) 
	end) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			error_logger:error_msg("Got error updating slot status: ~p", [Reason]),
			{error, Reason}
	end.

update_slot_status_mnesia(_Machine, []) ->
	ok;
update_slot_status_mnesia(Machine, [{Slot, Status} | T]) ->
	[SlotInfo] = qlc:eval(qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= Machine, X#slot.num =:= Slot ])),
	case Status of
		1 ->
			case SlotInfo#slot.avail of
				0 ->
					NewSlotInfo = SlotInfo#slot{avail = 1},
					mnesia:delete_object(SlotInfo),
					mnesia:write(NewSlotInfo);
				_X ->
					ok
			end;
		0 ->
			case SlotInfo#slot.avail of
				0 ->
					ok;
				_X ->
					NewSlotInfo = SlotInfo#slot{avail = 0},
					mnesia:delete_object(SlotInfo),
					mnesia:write(NewSlotInfo)
			end
	end,
	update_slot_status_mnesia(Machine, T).