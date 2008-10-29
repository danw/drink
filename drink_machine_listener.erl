-module (drink_machine_listener).
-behaviour (gen_server).

-include ("drink_config.hrl").

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([got_connection/1, got_socket_error/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main socket listener process that acts as an OTP behavior in the supervisor tree %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init ([]) ->
	case gen_tcp:listen(?LISTEN_PORT, [binary, {packet, line}, {reuseaddr, true}, {active, false}, {keepalive, true}, {send_timeout, 10}]) of
		{ok, ListenSocket} ->
			process_flag(trap_exit, true),
			watch_listen_socket(ListenSocket),
			{ok, null};
		{error, Reason} ->
			{stop, Reason}
	end.

terminate (_Reason, _State) ->
	ok.

code_change (_OldVsn, State, _Extra) when is_tuple(State) ->
	{ok, State}.

handle_call (_Request, _From, State) ->
	{noreply, State}.

handle_info ({'EXIT', _Pid, Reason}, State) ->
	error_logger:error_msg("Exited for reason: ~w~n", [Reason]),
	{stop, Reason, State}.

handle_cast ({got_connection, Socket}, State) ->
	socket_watcher(Socket, {drink_machine_comm, start_link, []}),
	{noreply, State};
handle_cast ({got_socket_error, Reason}, State) ->
	error_logger:error_msg("Got socket error ~w~n", [Reason]),
	{stop, Reason, State}.

got_connection (Socket) ->
	gen_server:cast(?MODULE, {got_connection, Socket}).

got_socket_error (Reason) ->
	gen_server:cast(?MODULE, {got_socket_error, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process that Loops on gen_tcp:accept %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
watch_accept_socket (Pid, ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			case gen_tcp:controlling_process(Socket, Pid) of
				ok ->
					?MODULE:got_connection(Socket);
				{error, Reason} ->
					error_logger:error_msg("Error transfering ownership: ~p~n", [Reason]),
					gen_tcp:close(Socket)
			end,
			watch_accept_socket(Pid, ListenSocket);
		{error, Reason} ->
			?MODULE:got_socket_error(Reason)
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
	receive
		{'EXIT', ProcessId, _Reason} ->
			gen_tcp:close(Socket)
	end.

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
