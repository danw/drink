%%%-------------------------------------------------------------------
%%% File    : drink_event_permissions.erl
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

-module (drink_event_permissions).
-behaviour (dw_events_permissions).

-export ([can_register/2, filter_event/4]).

-include ("user.hrl").
-include ("drink_mnesia.hrl").
-include_lib ("drink_log/include/drink_log.hrl").

% Allow everyone to register
can_register (drink, _ClientInfo) -> ok.

% Allow verified processes full access
filter_event (drink, _FromPid, {registered, _}, Event) ->
    %error_logger:error_msg("Registered PID OK~n"),
    {ok, Event};
filter_event (drink, _FromPid, UserRef, Event) ->
    %error_logger:error_msg("In filter_event with UserRef~n"),
    case user_auth:can_admin_noblock(UserRef) of
        true -> filter_event_admin(drink, UserRef, Event);
        false -> filter_event_user(drink, UserRef, Event)
    end.

filter_event_admin(drink, UserRef, Event) ->
    %error_logger:error_msg("admin OK~n"),
    {ok, Event}.

filter_event_user(drink, UserRef, Event = #money_log{ username = Username }) ->
    case user_auth:user_info_noblock(UserRef) of
        {ok, #user{ username = Username }} ->
            %error_logger:error_msg("for user ~p OK~n", [Username]),
            {ok, Event};
        _ ->
            %error_logger:error_msg("not for user FAIL~n"),
            false
    end;
filter_event_user(drink, UserRef, {user_changed, Username, Changes}) ->
    case user_auth:user_info_noblock(UserRef) of
        {ok, #user{ username = Username }} ->
            {ok, {user_changed, Username, filter_user_changes(Changes)}};
        _ ->
            false
    end;
% TODO: this only comes with the machineatom, not the full struct
%filter_event_user(drink, _, {machine_added, Machine = #machine{ admin_only = true }}) -> false;
filter_event_user(drink, _, {machine_modified, Machine = #machine{ admin_only = true }}) -> false;
% TODO: We don't have full info here... and can't get it, because it's gone
%filter_event_user(drink, _, {machine_deleted, Machine = #machine{ admin_only = true }}) -> false;
filter_event_user(drink, _, {slot_modified, Machine = #machine{ admin_only = true }, _SlotInfo}) -> false;
% TODO: filter temps for admin_only machines?
filter_event_user(drink, _, Event) ->
    %error_logger:error_msg("default user OK~n"),
    {ok, Event}.

filter_user_changes([]) -> [];
% Users can't see their own ibuttons
filter_user_changes([{del_ibutton,_}|T]) -> [{del_ibutton, ""}] ++ filter_user_changes(T);
filter_user_changes([{add_ibutton,_}|T]) -> [{add_ibutton, ""}] ++ filter_user_changes(T);
filter_user_changes([Other|T]) -> [Other] ++ filter_user_changes(T).
