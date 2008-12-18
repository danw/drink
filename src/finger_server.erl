%%%-------------------------------------------------------------------
%%% File    : finger_server.erl
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

-module (finger_server).

-export ([start_link/0]).
-export ([init/1]).

-include ("drink_mnesia.hrl").
-include ("user.hrl").
-include_lib ("stdlib/include/qlc.hrl").

-record (finger_state, {
			socket,
			machine = nil}).

start_link () ->
	spawn_link(?MODULE, init, [self()]).

init (_Parent) ->
	loop(waiting_for_socket, #finger_state{}).

loop (waiting_for_socket, State) ->
	receive
		{socket, Socket} ->
			inet:setopts(Socket, [{active, once}]),
			{ok, {Address, _Port}} = inet:sockname(Socket),
			Q = qlc:q([ X#machine.machine || X <- mnesia:table(machine), X#machine.public_ip =:= Address ]),
			Machine = case mnesia:transaction(fun() -> qlc:eval(Q) end) of
				{atomic, [M]} ->
					M;
				{atomic, []} ->
				    % TODO: replace with drink_machine_sup:default_machine
					bigdrink;
				{aborted, _Reason} ->
					bigdrink
			end,
			loop(normal, State#finger_state{socket=Socket,machine=Machine});
		_Else ->
			loop(waiting_for_socket, State)
	end;

loop (normal, State) ->
	Socket = State#finger_state.socket,
	receive
		{tcp, Socket, Data} ->
		    % TODO: Make sure it's a full line and only a full line
		    Line = binary_to_list(Data) -- "\r\n",
			NewState = process_line(State, Line),
			inet:setopts(Socket, [{active, once}]),
			loop(normal, NewState);
		{tcp_closed, Socket} ->
            % error_logger:error_msg("TCP Socket Closed"),
			exit(tcp_closed);
		{tcp_error, Socket, Reason} ->
			error_logger:error_msg("TCP Socket Error: ~p", [Reason]),
			exit(Reason);
		_Else ->
			loop(normal, State)
	end.

header(Machine) ->
    io_lib:format("Welcome to ~s~n~n SLOT  ITEM             COST  # LEFT~n------------------------------------~n",[atom_to_list(Machine)]).

footer() ->
    io_lib:format("------------------------------------~n~n", []).

machine_state(Machine) ->
    {ok, Slots} = drink_machine:slots(Machine),
    format_slots(Slots).

avail_format(0) ->
    "  Out";
avail_format(1) ->
    "Available";
avail_format(Num) ->
    io_lib:format("Available (~b)", [Num]).

format_slots([]) ->
    [];
format_slots([Slot|Slots]) ->
    io_lib:format("~s  ~s ~s ~s~n", [string:right(integer_to_list(Slot#slot.num), 4),
                                      string:left(Slot#slot.name, 15),
                                      string:right(integer_to_list(Slot#slot.price), 6),
                                      avail_format(Slot#slot.avail)]) ++ format_slots(Slots).

process_line(State, Line) ->
    Machine = State#finger_state.machine,
    Res = case lists:member($@, Line) of
        true ->
            io_lib:format("Finger forwarding service denied~n",[]);
        false ->
            case drink_machines_sup:is_machine(Machine) of
                true ->
                    case drink_machine:is_alive(Machine) of
                        true ->
                            % TODO: interpret finger protocol
                            header(Machine) ++ machine_state(Machine) ++ footer();
                        false ->
                            io_lib:format("Machine is down~n", [])
                    end;
                false ->
                    io_lib:format("Machine not found~n", [])
            end
    end,
    gen_tcp:send(State#finger_state.socket, Res),
    exit(normal).