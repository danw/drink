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

-export ([can_register/2, filter_event/3]).

% Allow everyone to register
can_register (drink, _ClientInfo) -> true.

% Allow verified processes full access
filter_event (drink, {registered, _}, Event) -> {ok, Event}.
% TODO: filter the rest of the events by user auth
filter_event (drink, _, Event) -> {ok, Event}.
