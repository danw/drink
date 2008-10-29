-module (drink_machine_listener).
-include ("drink_config.hrl").

-export ([start_link/0]).
-export ([init/1]).
-export ([system_continue/3, system_terminate/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main socket listener process that acts as an OTP behavior in the supervisor tree %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
	proc_lib:start_link(?MODULE, init, [self()]).

init (Parent) ->
	register(?MODULE, self()),
	Debug = sys:debug_options([]),
	case gen_tcp:listen(?LISTEN_PORT, [binary, {packet, line}, {reuseaddr, true}, {active, false}, {keepalive, true}, {send_timeout, 10}]) of
		{ok, ListenSocket} ->
			process_flag(trap_exit, true),
			proc_lib:init_ack(Parent, {ok, self()}),
			watch_listen_socket(ListenSocket),
			loop(ListenSocket, Parent, Debug);
		{error, Reason} ->
			exit(Reason)
	end.

loop(ListenSocket, Parent, Debug) ->
	receive
		{got_connection, Socket} ->
			case inet:peername(Socket) of
				{ok, Remote} ->
					io:format("Got connection from ~w~n", [Remote]);
				{error, Reason} ->
					io:format("Failed for some reason: ~p~n", [Reason])
			end,
			socket_watcher(Socket, {drink_machine_comm, start_link, []}),
			loop(ListenSocket, Parent, Debug);
		{got_socket_error, Reason} ->
			io:format("Got socket error ~w~n", [Reason]),
			exit(Reason);
		{'EXIT', _Pid, Reason} ->
			io:format("Exited for reason: ~w~n", [Reason]),
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, ListenSocket)
	end.

system_continue (Parent, Debug, State) ->
	loop(State, Parent, Debug).

system_terminate (Reason, _Parent, _Debug, _State) ->
	exit(Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process that Loops on gen_tcp:accept %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
watch_accept_socket (Pid, ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			case gen_tcp:controlling_process(Socket, Pid) of
				ok ->
					Pid ! {got_connection, Socket};
				{error, Reason} ->
					error_logger:error_msg("Error transfering ownership: ~p~n", [Reason]),
					gen_tcp:close(Socket)
			end,
			watch_accept_socket(Pid, ListenSocket);
		{error, Reason} ->
			Pid ! {got_socket_error, Reason}
	end.

watch_listen_socket (ListenSocket) ->
	Pid = self(),
	spawn_link(fun() -> watch_accept_socket(Pid, ListenSocket) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process that Closes socket when process exits %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
socket_watcher_impl ({Module, Fun, Args}) ->
	process_flag(trap_exit, true),	% Trap the child process' exit
	receive							% Wait for the socket to be assigned to us
		{socket, Socket} ->
			Pid = apply(Module, Fun, Args),		% Start child process
			case gen_tcp:controlling_process(Socket, Pid) of
				ok -> 				% Pass the Socket to the child if we can
					Pid ! {socket, Socket},
					socket_watcher_impl(Socket, Pid);
				{error, Reason} ->	% Otherwise close the socket and kill the child
					error_logger:error_msg("Error transfering ownership: ~p~n", [Reason]),
					gen_tcp:close(Socket),
					exit(Pid, kill)
			end
	end.

socket_watcher_impl (Socket, ProcessId) ->
%	io:format("Waiting on ~p to exit so we can close ~p~n", [ProcessId, Socket]),
	receive
		{'EXIT', ProcessId, _Reason} ->
			gen_tcp:close(Socket)
	end.
%	io:format("Closed ~p~n", [Socket]).

socket_watcher (Socket, Mfa) ->
	Pid = spawn(fun() -> % De-link the watcher from us so we don't get the exit message
			socket_watcher_impl(Mfa)
		  end),
	case gen_tcp:controlling_process(Socket, Pid) of
		ok ->				% Successfully pass of the Socket to the watcher
			Pid ! {socket, Socket};
		{error, Reason} -> % Or close the socket and kill the watcher
			error_logger:error_msg("Error transfering ownership: ~p~n", [Reason]),
			exit(Pid, kill),
			gen_tcp:close(Socket)
	end.
