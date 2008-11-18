-module (drink_machine_comm).

-export ([start_link/0]).
-export ([init/1]).
-export ([send_command/2]).

-include ("drink_mnesia.hrl").
-include ("qlc.hrl").
-record (dmcomm_state, {
			socket,
			machine}).

start_link () ->
	spawn_link(?MODULE, init, [self()]).

init (_Parent) ->
	loop(waiting_for_socket, #dmcomm_state{}).

loop (waiting_for_socket, State) ->
	receive
		{socket, Socket} ->
			inet:setopts(Socket, [{active, once}]),
			loop(waiting_for_auth, State#dmcomm_state{socket=Socket});
		_Else ->
			loop(waiting_for_socket, State)
	end;

loop (waiting_for_auth, State) ->
	#dmcomm_state{socket=Socket} = State,
	receive
		{tcp, Socket, Data} ->
			{ok, Remote} = inet:peername(Socket),
			case machine_lookup(Remote,binary_to_list(Data) -- "\r\n") of
				{ok, MachineId} ->	% Got a valid machine
					send(machine_ack, Socket),
					inet:setopts(Socket, [{active, once}]),
					case drink_machines_sup:machine_connected(MachineId, self()) of
						{error, Reason} ->
							error_logger:error_msg("Failure starting machine: ~p", [Reason]),
							exit(Reason);
						{ok, Pid} ->
							link(Pid),
							loop(normal_op, State#dmcomm_state{machine=Pid})
					end;
				{error, Reason} ->	% Invalid auth token
					send(machine_nack, Socket),
					error_logger:error_msg("Bad machine password(~p) ~p", [Reason, Data]),
					exit(Reason)
			end;
		{tcp_closed, Socket} ->
			error_logger:error_msg("TCP Socket Closed"),
			exit(tcp_closed); 		% At this point, if an error occurs, just exit
		{tcp_error, Socket, Reason} ->
			error_logger:error_msg("TCP Socket Error: ~p", [Reason]),
			exit(Reason);
		_Else ->
			loop(waiting_for_auth, State)
	end;

loop (normal_op, State) ->
	#dmcomm_state{socket=Socket,machine=Machine} = State,
	receive
		{'EXIT', Machine, Reason} ->
			error_logger:error_msg("Machine Exited: ~p", [Reason]),
			exit(Reason);
		{send, Machine, Command} ->
			send(Command, Socket),
			loop(normal_op, State);
		{tcp, Socket, Data} ->
			case receive_response(Data, State) of
				{ok, Response} ->
					drink_machine:got_response(State#dmcomm_state.machine, Response);
				{error, Reason} ->
					error_logger:error_msg("Got bad response from machine: ~p (~p)~n", [Reason, Data])
			end,
			inet:setopts(Socket, [{active, once}]),
			loop(normal_op, State);
		{tcp_closed, Socket} ->			% Notify Drink Machine Object???
			error_logger:error_msg("TCP Socket Closed"),
			exit(tcp_closed);
		{tcp_error, Socket, Reason} ->
			error_logger:error_msg("TCP Socket Error: ~p", [Reason]),
			exit(Reason);
		_Else ->
			loop(normal_op, State)
	end.

% External API Call
send_command(MachineComm, Command) ->
	MachineComm ! {send, self(), Command}.

% Looking up a machine - Address, Password
machine_lookup(From, Pass) when is_list(Pass) ->
	machine_lookup(From, list_to_atom(Pass));
machine_lookup({_Address, _Port}, Pass) when is_atom(Pass) ->
	Q = qlc:q([ X#machine.machine || X <- mnesia:table(machine), X#machine.password =:= Pass ]),
	case mnesia:transaction(fun() -> qlc:eval(Q) end) of
		{atomic, [MachineId]} ->
			{ok, MachineId};
		{atomic, []} ->
			{error, badpass};
		{aborted, Reason} ->
			{error, Reason}
	end.

% Send Command
send(machine_ack, Socket) ->
	gen_tcp:send(Socket, "1\n");
send(machine_nack, Socket) ->
	gen_tcp:send(Socket, "2\n");
send({drop,Slot}, Socket) when is_integer(Slot) ->
	gen_tcp:send(Socket, lists:append([[$3], integer_to_list(Slot), "\n"]));
send(slot_check, Socket) ->
	gen_tcp:send(Socket, "6\n").

remove_line_ending(Str) when is_binary(Str) ->
	remove_line_ending(binary_to_list(Str));
remove_line_ending(Str) when is_list(Str) ->
	Str1 = string:strip(Str, right, 10),
	string:strip(Str1, right, 13).

convert_status_list([]) ->
	[];
convert_status_list([H|Tail]) ->
	[SlotI, SlotStatusI] = string:tokens(H, " "),
	{Slot, _} = string:to_integer(SlotI),
	{SlotStatus, _} = string:to_integer(SlotStatusI),
	lists:append([{Slot, SlotStatus}], convert_status_list(Tail)).

% Received Command
receive_response(<<$4, _/binary>>, _State) ->
	{ok, drop_ack};
receive_response(<<$5, _/binary>>, _State) ->
	{ok, drop_nack};
receive_response(<<$7, StatusBin/binary>>, _State) ->
	StatusList = string:tokens(remove_line_ending(StatusBin), "`"),
	{ok, {slot_status, convert_status_list(StatusList)}};
receive_response(<<$8, Remain/binary>>, _State) ->
	case string:to_float(binary_to_list(Remain)) of
		{error, Reason} ->
			error_logger:error_msg("Failed to convert temperature: ~p", [Reason]),
			{error, Reason};
		{Temperature, _Rest} ->
			{ok, {temperature, erlang:universaltime(), Temperature}}
	end;
receive_response(_Data, _State) ->
	{error, unknown}.