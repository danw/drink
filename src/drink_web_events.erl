%%%-------------------------------------------------------------------
%%% File    : drink_web_events.erl
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

-module (drink_web_events).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([register/1, trigger/2]).

-record (web_event_state, {table}).
-record (event_listener, {yawspid, types}).

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

handle_call({register, Types}, {Pid, _PidRef}, State) ->
    Listener = #event_listener{yawspid = Pid, types = Types},
    ets:insert(State#web_event_state.table, Listener),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({event, Type, Data}, State) ->
    trigger_event(Type, Data, State),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    ets:delete(State#web_event_state.table, Pid),
    io:format("~n~nWeb Process Died ~w~n~n", [Pid]),
    {noreply, State};
handle_info(ping, State) ->
    trigger_event(ping, ok, State),
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

register(Type) when is_atom(Type) ->
    register([Type]);
register(Types) when is_list(Types) ->
    gen_server:call(?MODULE, {register, Types}).

trigger(Type, Data) ->
    ?MODULE ! {event, Type, Data}.

%%%%%%%%%%%%%%%%%%%%
% Internal Helpers %
%%%%%%%%%%%%%%%%%%%%

trigger_event(Type, Data, State) ->
    Msg = encode_message(Type, Data),
    ets:foldl(fun(Listener, _) ->
        yaws_api:stream_chunk_deliver(Listener#event_listener.yawspid, Msg),
        ok
    end, ok, State#web_event_state.table).

encode_message(temperature, _Data) ->
    drink_web:encode_json_chunk({struct, [{event, "temperature"}, {data, {struct, [{machine, "bigdrink"}, {temperature, 42.3}]}}]});
encode_message(ping, _Data) ->
    drink_web:encode_json_chunk({struct, [{event, "ping"}]}).