%%%-------------------------------------------------------------------
%%% File    : user_auth_mnesia.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : Mnesia connector for user_auth.erl
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

-module (user_auth_mnesia).

-export ([init/0]).
% User auth interface
-export ([get_user/2]).
-export ([set_credits/1, set_admin/1, add_ibutton/2, del_ibutton/2]).
% Debug interface
-export ([add_user/1]).

-include ("user.hrl").

init() -> 
    case mnesia:create_table(user, [
	{disc_copies, [node()]},
	{ram_copies, []},
	{record_name, user},
	{index, [ibuttons]},
	{attributes, record_info(fields, user)}]) of
	    {atomic, ok} -> ok;
	    {aborted, {already_exists, user}} -> ok;
	    E -> 
		error_logger:error_msg("Got mnesia error: ~p~n", [E]),
		error
    end.

% User Auth Interfaces
get_user(username, Username) ->
    case mnesia:transaction(fun() -> mnesia:read(user, Username) end) of
	{atomic, [UserInfo]} ->
	    {ok, UserInfo};
	{atomic, []} ->
	    {error, not_found};
	{atomic, _} ->
	    {error, unknown};
	_ ->
	    {error, mnesia}
    end;
get_user(ibutton, Ibutton) ->
    {error, not_implemented}.

set_credits(UserInfo) ->
    F = fun() ->
        case mnesia:read(user, UserInfo#user.username) of
            [User] -> mnesia:write(User#user{credits = UserInfo#user.credits});
            E -> E
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        E -> {error, E}
    end.

set_admin(UserInfo) ->
    F = fun() ->
        case mnesia:read(user, UserInfo#user.username) of
            [User] -> mnesia:write(User#user{admin = UserInfo#user.admin});
            E -> E
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        E -> {error, E}
    end.

add_ibutton(Username, IButton) ->
    {error, not_implemented}.

del_ibutton(Username, IButton) ->
    {error, not_implemented}.

% Debug Methods
add_user(UserInfo) ->
    mnesia:transaction(fun() -> mnesia:write(UserInfo) end).

