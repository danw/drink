%%%-------------------------------------------------------------------
%%% File    : drink_machine.erl
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

-module (drink_machine).
-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_response/2, got_machine_comm/1]).
-export ([slots/1, drop/2, temperature/1, slot_info/2, is_alive/1, set_slot_info/2]).

-include ("drink_mnesia.hrl").
-include_lib ("stdlib/include/qlc.hrl").

-record (dmstate, {
			machineid,
			commpid = nil,
			record,
			latest_temp = nil}).

start_link (MachineId) ->
	gen_server:start_link({local, MachineId}, ?MODULE, MachineId, []).

init (MachineId) ->
	case mnesia:transaction(fun() -> mnesia:read({machine, MachineId}) end) of
		{atomic, [MachineRecord]} ->
			State = #dmstate{
				machineid = MachineId,
				record = MachineRecord
			},
			{ok, State};
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

handle_call ({drop, Slot}, _From, State) ->
	case {State#dmstate.commpid, get_slot_by_num(Slot, State)} of
	    {nil, _} ->
	        {reply, {error, machine_down}, State};
		{CommPid, {ok, SlotInfo}} when SlotInfo#slot.avail > 0 ->
			Ref = make_ref(),
			case timer:send_after(timer:seconds(30), {timeout, Ref}) of
				{ok, Timer} ->
					drink_machine_comm:send_command(CommPid, {drop, Slot}),
					receive
						{timeout, Ref} ->
							timer:cancel(Timer),
							error_logger:error_msg("Drop timed out on ~p~n", [State#dmstate.machineid]),
							{reply, {error, timeout}, State};
						{got_response, CommPid, drop_ack} ->
							timer:cancel(Timer),
							decrement_slot_avail(Slot, State),
							{reply, {ok}, State};
						{got_response, CommPid, drop_nack} ->
							timer:cancel(Timer),
							error_logger:error_msg("Drop nack'd on ~p~n", [State#dmstate.machineid]),
							{reply, {error, drop_nack}, State}
					end;
				{error, Reason} ->
					error_logger:error_msg("Timer couldn't be created for ~p drop(~p)",
											[State#dmstate.machineid,Reason]),
					{reply, {error, timer_err}, State}
			end;
		{_, {ok, _SlotInfo}} ->
			{reply, {error, not_available}, State};
		{_, {error, Reason}} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({slots}, _From, State) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid ]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, List} ->
			{reply, {ok, lists:sort(List)}, State};
		{aborted, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({slot_info, SlotNum}, _From, State) ->
	Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid, X#slot.num =:= SlotNum]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, [SlotInfo]} ->
			{reply, {ok, SlotInfo}, State};
		{atomic, _List} ->
			{reply, {error, invalid_slot}, State};
		{aborted, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call ({set_slot_info, SlotInfo}, _From, State) ->
    Q = qlc:q([ X || X <- mnesia:table(slot), X#slot.machine =:= State#dmstate.machineid, X#slot.num =:= SlotInfo#slot.num]),
    case mnesia:transaction(fun() ->
        case qlc:eval(Q) of
            [Slot] ->
                mnesia:delete_object(Slot),
                mnesia:write(SlotInfo);
            _ ->
                mnesia:abort(invalid_slot)
        end
    end) of
        {atomic, _} ->
            {reply, ok, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call ({temp}, _From, State) ->
	case State#dmstate.latest_temp of
		nil ->
			{reply, {error, no_temp}, State};
		Temp ->
			{reply, {ok, Temp}, State}
	end;
handle_call ({got_comm, CommPid}, _From, State) ->
    link(CommPid),
    {reply, {ok, self()}, State#dmstate{commpid = CommPid}};
handle_call ({is_alive}, _From, State = #dmstate{commpid = nil}) ->
    {reply, false, State};
handle_call ({is_alive}, _From, State) ->
    {reply, true, State};
handle_call (_Request, _From, State) ->
	{reply, {error, unknown}, State}.

handle_info ({got_response, CommPid, Response}, State = #dmstate{commpid = CommPid}) ->
	case Response of
		drop_ack ->
			error_logger:error_msg("Drop Ack Received while not dropping!"),
			{noreply, State};
		drop_nack ->
			error_logger:error_msg("Drop Nack Received while not dropping!"),
			{noreply, State};
		{temperature, DateTime, Temperature} ->
			T = #temperature{
				machine = State#dmstate.machineid, 
				time = DateTime,
				temperature = Temperature
			},
			drink_mnesia:log_temperature(T),
			{noreply, State#dmstate{latest_temp = Temperature}};
		{slot_status, Status} ->
			update_slot_status(Status, State),
			{noreply, State};
		Response ->
			io:format("Unknown response: ~p~n", [Response]),
			{noreply, State}
	end;
handle_info ({'EXIT', CommPid, _Reason}, State = #dmstate{commpid = CommPid}) ->
    {noreply, State#dmstate{commpid = nil}};
handle_info ({timeout, _DropRef}, State) ->
	{noreply, State};
handle_info (_, State) ->
	{noreply, State}.

% Callback from Drink Machine Comm
got_machine_comm (MachinePid) ->
    safe_gen_call(MachinePid, {got_comm, self()}).

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

set_slot_info (UserRef, SlotInfo = #slot{machine = MachinePid, num = SlotNum, name = Name, price = Price, avail = Avail}) 
        when is_reference(UserRef), is_atom(MachinePid), is_integer(SlotNum), is_list(Name), is_integer(Price), is_integer(Avail),
        Price >= 0, Avail >= 0, SlotNum >= 0 ->
    case user_auth:can_admin(UserRef) of
        true ->
            safe_gen_call(MachinePid, {set_slot_info, SlotInfo});
        false ->
            {error, permission_denied}
    end.

is_alive (Machine) when is_atom(Machine) ->
    case safe_gen_call(Machine, {is_alive}) of
        {error, _} ->
            false;
        Out ->
            Out
    end.

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

update_slot_status(Status, State = #dmstate{record = MachineInfo}) ->
    case MachineInfo#machine.available_sensor of
        true ->
        	case mnesia:transaction(fun() ->
        		mnesia:write_lock_table(slot),
        		update_slot_status_mnesia(State#dmstate.machineid, Status) 
        	end) of
        		{atomic, _} ->
        			ok;
        		{aborted, Reason} ->
        			error_logger:error_msg("Got error updating slot status: ~p", [Reason]),
        			{error, Reason}
        	end;
        false ->
            ok
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