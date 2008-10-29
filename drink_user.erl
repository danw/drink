-module (drink_user).
-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([is_admin/1]).

-include ("user.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link (User) when is_tuple(User) ->
	#user{username=Username} = User,
	gen_server:start_link({local, lists:append([atom_to_list(?MODULE), "_", Username])},
						?MODULE, User, []).

init (User) when is_tuple(User) ->
	process_flag(trap_exit, true),
	{ok, User}.

handle_call (is_admin, _From, State) when is_tuple(State) ->
	case State#user.admin of
		true ->
			{reply, {ok, true}, State};
		_Else ->
			{reply, {ok, false}, State}
	end;

handle_call (_Request, _From, State) when is_tuple(State) ->
	{reply, {error, unknown_call}, State}.

handle_cast (_Request, State) when is_tuple(State) ->
	{noreply, State}.

handle_info (Info, State) when is_tuple(State) ->
	case Info of
		{'EXIT', _From, _Reason} ->
			error_logger:error_msg("User got EXIT message"),
			ok;
		_Wtf ->
			ok
	end,
	{noreply, State}.

terminate (_Reason, _State) ->
	ok.

code_change (_OldVsn, State, _Extra) when is_tuple(State) ->
	{ok, State}.

%%%%%%%%%%%%%%%%
% External API %
%%%%%%%%%%%%%%%%
is_admin (UserPid) when is_pid(UserPid) ->
	case gen_server:call(UserPid, is_admin) of
		{ok, true} ->
			true;
		_Else ->
			false
	end.
