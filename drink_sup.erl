-module (drink_sup).
-behaviour (supervisor).

-export ([start/0, start_link/1, init/1]).

start () ->
	start_link([]).

start_link (Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init ([]) ->
	{ok, {{one_for_one, 3, 10},  % One for one restart, shutdown after 3 restarts within 10 seconds
		  [{machine_listener,    % Our first child, the drink_machine_listener
			{drink_machine_listener, start_link, []},
			permanent,			 % Always restart
			100,				 % Allow 10 seconds for it to shutdown
			worker,				 % It isn't a supervisor
			[drink_machine_listener]},
			
		   {machines,			% The Supervisor for connected machines
			{drink_machines_sup, start_link, []},
			permanent,			% Always restart the supervisor
			infinity,			% Wait forever for the supervisor
			supervisor,
			[drink_machines_sup]}, % Uses the drink_machines_sup Module
			
		   {user_auth,			% The User Authenticator
		    {user_auth, start_link, []},
		    permanent,			% Always restart
		    100,				% Allow 100 seconds for it to shutdown
		    worker,				% Not a supervisor
		    [user_auth]},		% Uses the user_auth Module
		  ]
		}}.