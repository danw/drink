-module (drink_sup).
-behaviour (supervisor).

-include ("drink_config.hrl").

-export ([start/0, start_link/1, init/1]).

start () ->
	start_link([]).

start_link (Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init ([]) ->
	{ok, {{one_for_one, 10, 3},  % One for one restart, shutdown after 10 restarts within 3 seconds
		  [{machine_listener,    % Our first child, the drink_machine_listener
			{gen_listener, start_link, [?MACHINE_LISTEN_PORT, {drink_machine_comm, start_link, []}]},
			permanent,			 % Always restart
			100,				 % Allow 10 seconds for it to shutdown
			worker,				 % It isn't a supervisor
			[gen_listener]},
			
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

		   {eldap_user,
                    {eldap, start_link, ["user"]},
                    permanent,
                    100,
                    worker,
                    [eldap]},
		
		   {sunday_server_listener,
		    {gen_listener, start_link, [?SUNDAY_SERVER_PORT, {sunday_server, start_link, []}]},
		    permanent,
		    100,
		    worker,
		    [gen_listener]},

           {pam_auth,
            {epam, start_link, []},
            permanent,
            100,
            worker,
            [epam]}
		   
            %          {web_server,
            % {drink_web, start_link, []},
            % permanent,
            % 100,
            % worker,
            % [drink_web]}
		  ]
		}}.
