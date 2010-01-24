%%%-------------------------------------------------------------------
%%% File    : drink_web_event_worker.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%%
%%% edrink, Copyright (C) 2010 Dan Willemsen
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

-module (drink_web_event_worker).
-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([send_msg/2]).

-record (worker, {socket, userref}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link (UserRef) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, UserRef, []).

init (UserRef) ->
    {ok, #worker{userref = UserRef, socket = nil}}.

handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_msg, Msg}, State = #worker{socket = Socket}) when Socket =/= nil ->
    % TODO: handle errors from yaws?
    yaws_api:websocket_send(Socket, Msg),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({ok, WebSocket}, State) ->
    % TODO: Check for already existing socket?
    yaws_api:websocket_send(WebSocket, json:encode({struct, [{event, "hello"}]})),
    {noreply, State#worker{socket = WebSocket}};
handle_info({tcp, WebSocket, DataFrame}, State) ->
    Data = yaws_api:websocket_unframe_data(DataFrame),
    io:format("Got data from Websocket: ~p~n", [Data]),
    {noreply, State};
handle_info(discard, State) ->
    {stop, discard, State};
handle_info({tcp_closed, WebSocket}, State = #worker{socket = WebSocket}) ->
    {stop, tcp_closed, State};
handle_info({tcp_closed, _WebSocket}, State) ->
    error_logger:error_msg("Got tcp_closed for a socket we don't own!~n"),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    user_auth:delete_ref(State#worker.userref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%

send_msg(Ref, Msg) ->
    gen_server:cast(Ref, {send_msg, Msg}).
