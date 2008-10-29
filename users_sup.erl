-module (users_sup).
-behaviour (supervisor).

-export ([start_link/0, init/1]).
-export ([add_user/1]).

start_link () ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init ([]) ->
	{ok, {{simple_one_for_one, 3, 10}, % One for one restart, dies completely after 3 times in 10 seconds
		  [{user,
			{drink_user, start_link, []},
			temporary,
			brutal_kill,
			worker,
			[drink_user]}]
		}}.

add_user(User) when is_tuple(User) ->
	Pid = self(),
	case whereis(user_auth) of
		Pid ->
			supervisor:start_child(?MODULE, [User]);
		_Else ->
			{error, permission_denied}
	end.
