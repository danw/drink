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
-export ([machines/0, is_machine/1]).

-include ("drink_mnesia.hrl").
-include_lib ("stdlib/include/qlc.hrl").

start_link () ->
	case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
	    {ok, Pid} ->
		MachineSpecs = case mnesia:transaction(fun() -> mnesia:all_keys(machine) end) of
		    {atomic, Machines} ->
			lists:foreach(fun(machine) -> supervisor:start_child(?MODULE, machine) end, Machines);
	    	    _ ->
			error_logger:error_msg("Failed to read machine table from mnesia! Not creating drink machine instances!~n")
		end,
		{ok, Pid};
	    Error ->
		Error
	end.

init ([]) ->
	{ok, {{simple_one_for_one, 3, 10}, % Simple one for one restart, dies completely after 3 times in 10 seconds
		  [{drink_machine,
			{drink_machine, start_link, []},
			permanent,
			100,
			worker,
			[drink_machine]}]}}.

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
