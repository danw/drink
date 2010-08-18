%%%-------------------------------------------------------------------
%%% File    : drink_web_events.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2008-2010 Dan Willemsen
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

-module (drink_web_events).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([register/2, trigger/2]).

-record (web_event_state, {table}).
-record (event_listener, {worker, userref, types}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init ([]) ->
    process_flag(trap_exit, true),
    Table = ets:new(web_event_listeners, [set, private, {keypos, 2}]),
    timer:send_interval(timer:seconds(20), ping),
    {ok, #web_event_state{table = Table}}.

handle_call({register, UserRef, Types}, _, State) ->
    case drink_web_event_worker:start_link(UserRef) of
        {ok, Pid} ->
            Listener = #event_listener{worker = Pid, userref = UserRef, types = Types},
            ets:insert(State#web_event_state.table, Listener),
            {reply, {ok, Pid, true}, State};
        _ ->
            error_logger:error_msg("Error starting drink_web_event_worker"),
            {reply, {error, web_worker}, State}
    end;
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({event, Type, Data}, State) ->
    trigger_event(Type, Data, State),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(ping, State) ->
    trigger_event(ping, ok, State),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        tcp_closed -> ok;
        discard -> ok;
        E ->
            error_logger:error_msg("Web event worker died: ~p~n", [E]),
            case ets:lookup(State#web_event_state.table, Pid) of
                [Worker] -> user_auth:delete_ref(Worker#event_listener.userref);
                _ -> ok
            end
    end,
    ets:delete(State#web_event_state.table, Pid),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%

register(UserRef, Type) when is_atom(Type) ->
    ?MODULE:register(UserRef, [Type]);
register(UserRef, Types) when is_list(Types) ->
    gen_server:call(?MODULE, {register, UserRef, Types}).

trigger(Type, Data) ->
    gen_server:cast(?MODULE, {event, Type, Data}).

%%%%%%%%%%%%%%%%%%%%
% Internal Helpers %
%%%%%%%%%%%%%%%%%%%%

trigger_event(Type, Data, State) ->
    Msg = encode_event(Type, Data),
    ets:foldl(fun(Listener, _) ->
        drink_web_event_worker:send_msg(Listener#event_listener.worker, Msg)
    end, ok, State#web_event_state.table).

encode_event_data(machine, Machine) ->
    drink_json_api:machine_stat(false, Machine);
encode_event_data(temperature, _Data) ->
    {struct, [{machine, "bigdrink"}, {temperature, 42.3}]};
encode_event_data(_, _) ->
    nil.

encode_event(Type, Data) ->
    case encode_event_data(Type, Data) of
        nil -> json:encode({struct, [{event, atom_to_list(Type)}]});
        JData -> json:encode({struct, [{event, atom_to_list(Type)}, {data, JData}]})
    end.
