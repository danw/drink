%%%-------------------------------------------------------------------
%%% File    : drink_mnesia.erl
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

-module (drink_mnesia).
-export ([initialize/0]).

-include ("drink_mnesia.hrl").

initialize() ->
    case filelib:is_file("mnesia_data/schema.DAT") of
        false -> mnesia:create_schema([node()]);
        true ->  ok
    end,
    ok = mnesia:start(),
    case mnesia:create_table(machine, [
        {disc_copies, [node()]},
        {ram_copies, []},
        {record_name, machine},
        {index, [password]},
        {attributes, record_info(fields, machine)}]) of
        {atomic, ok} -> init_test_machines(), ok;
        {aborted, {already_exists, _}} -> ok;
        E -> error_logger:error_msg("Got mnesia error: ~p~n", [E])
    end,
    case mnesia:create_table(slot, [
        {type, bag},
        {disc_copies, [node()]},
        {ram_copies, []},
        {record_name, slot},
        {index, [num]},
        {attributes, record_info(fields, slot)}]) of
        {atomic, ok} -> init_test_slots(), ok;
        {aborted, {already_exists, _}} -> ok;
        Er -> error_logger:error_msg("Got mnesia error: ~p~n", [Er])
    end.

init_test_machines() ->
    add_row(#machine{ machine=test,
                      password="password",
                      name="Test Machine",
                      public_ip={192,168,1,2},
                      machine_ip={192,168,1,2},
                      available_sensor=true,
                      allow_connect=true,
                      admin_only=true }).

init_test_slots() ->
    add_row(#slot{ machine=test, num=0, name="Dr Pepper", price=35, avail=1, disabled=false }),
    add_row(#slot{ machine=test, num=1, name="Disabled", price=35, avail=1, disabled=true }),
    add_row(#slot{ machine=test, num=2, name="Sprite", price=1000, avail=20, disabled=false }).

add_row(R) -> mnesia:transaction(fun() -> mnesia:write(R) end).
