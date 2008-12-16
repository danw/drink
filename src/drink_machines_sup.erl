%%%-------------------------------------------------------------------
%%% File    : drink_machines_sup.erl
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

-module (drink_machines_sup).
-behaviour (supervisor).

-export ([start_link/0, init/1]).
-export ([machine_connected/2, machines/0, is_machine/1, is_machine_alive/1]).

-include ("drink_mnesia.hrl").
-include_lib ("stdlib/include/qlc.hrl").

start_link () ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init ([]) ->
	{ok, {{simple_one_for_one, 3, 10}, % One for one restart, dies completely after 3 times in 10 seconds
		  [{machine,
			{drink_machine, start_link, []},
			temporary,
			brutal_kill,
			worker,
			[drink_machine]}]
		}}.

machine_connected (MachineId, CommPid) ->
	supervisor:start_child(?MODULE, [MachineId, CommPid]).

machine_names([{_,Pid,_,_}|T]) ->
	{registered_name, Name} = process_info(Pid, registered_name),
	lists:append([Name], machine_names(T));
machine_names([]) ->
	[].

machines () ->
	machine_names(supervisor:which_children(?MODULE)).

is_machine (MachineId) ->
	case mnesia:transaction(fun() -> mnesia:read({machine, MachineId}) end) of
		{atomic, [_MachineRec]} ->
			true;
		{atomic, []} ->
			false;
		{aborted, _Reason} ->
			false
	end.

is_machine_alive (MachineId) ->
	case whereis(MachineId) of
		undefined ->
			false;
		Pid ->
			is_process_alive(Pid)
	end.