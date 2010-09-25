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
-export ([machines/0, is_machine/1, add/1, mod/1, del/1]).

-include ("drink_mnesia.hrl").
-include_lib ("stdlib/include/qlc.hrl").

start_link () ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} ->
            case mnesia:transaction(fun() -> mnesia:all_keys(machine) end) of
                {atomic, Machines} ->
                    lists:foreach(fun(Machine) ->
                        case supervisor:start_child(?MODULE, [Machine]) of
                            {error, E} -> error_logger:error_msg("Error starting machine ~p: ~p~n", [Machine, E]);
                            {ok, _} -> ok
                        end
                    end, Machines);
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

add(Machine = #machine{}) ->
    F = fun() ->
        mnesia:write(Machine)
    end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            case supervisor:start_child(?MODULE, [Machine#machine.machine]) of
                {error, E} ->
                    error_logger:error_msg("Error starting machine ~p: ~p~n", [Machine#machine.machine, E]),
                    {error, E};
                {ok, _} ->
                    dw_events:send(drink, {machine_added, Machine}),
                    ok
            end;
        _ ->
            {error, mnesia}
    end.

mod(Machine) ->
    case drink_machine:modify_machine(Machine) of
        {ok, OldMachine} ->
            case mnesia:transaction(fun() -> mnesia:write(Machine) end) of
                {atomic, ok} ->
                    dw_events:send(drink, {machine_modified, OldMachine, Machine}),
                    ok;
                Reason ->
                    % TODO: Revert the drink_machine instance?
                    error_logger:error_msg("drink_machines_sup:mod(mnesia) -> ~p~n", [Reason]),
                    {error, mnesia}
            end;
        Reason ->
            error_logger:error_msg("drink_machines_sup:mod -> ~p~n", [Reason]),
            {error, modification_failed}
    end.

del(Machine) ->
    case drink_machine:get_info(Machine) of
        {ok, OldInfo} ->
            supervisor:terminate_child(?MODULE, [Machine]),
            supervisor:delete_child(?MODULE, [Machine]),
            case mnesia:transaction(fun() -> mnesia:delete({machine, Machine}) end) of
                {atomic, ok} ->
                    dw_events:send(drink, {machine_deleted, OldInfo}),
                    ok;
                Reason ->
                    error_logger:error_msg("drink_machines_sup:del(mnesia) -> ~p~n", [Reason]),
                    {error, mnesia}
            end;
        Reason ->
            error_logger:error_msg("drink_machines_sup:del -> ~p~n", [Reason]),
            {error, termination_failed}
    end.

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
