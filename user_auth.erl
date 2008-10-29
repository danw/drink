-module (user_auth).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([auth/2, admin/2]).

-include ("user.hrl").

-record (uastate, {ldapconn}).

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init ([]) ->
	process_flag(trap_exit, true),
	{ok, #uastate{}}.

handle_call ({auth, Username, Password}, _From, State) when is_tuple(State), is_list(Username), is_list(Password) ->
	Admin = string:equal(Username, "drinksvr"),
	case string:equal(Password, "magicpass") of
		true ->
			% TODO: Singleton this!
			User = users_sup:add_user(#user{username=Username,admin=Admin}),
			{reply, User, State};
		false ->
			{reply, {error, badpass}, State}
	end;

handle_call (_Request, _From, State) when is_tuple(State) ->
	{reply, {error, unknown_call}, State}.

handle_cast (_Request, State) when is_tuple(State) ->
	{noreply, State}.

handle_info (Info, State) when is_tuple(State) ->
	case Info of
		{'EXIT', _From, _Reason} ->
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
auth(Username, Password) when is_list(Username), is_list(Password) ->
	gen_server:call(?MODULE, {auth, Username, Password}).

admin(User, Username) when is_pid(User), is_list(Username) ->
	gen_server:call(?MODULE, {admin, User, Username}).