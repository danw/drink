-module (drink_sup).
-behaviour (supervisor).

-include ("drink_config.hrl").

-export ([start/0, start_link/1, init/1]).

start () ->
	start_link([]).

start_link (Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init ([]) ->
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(drink), "dbpass")),
    LogDBPassword = binary_to_list(Bin),
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
		    
		   {finger_server_listener,
		    {gen_listener, start_link, [?FINGER_SERVER_PORT, {finger_server, start_link, []}]},
		    permanent,
		    100,
		    worker,
		    [gen_listener]},

           {pam_auth,
            {epam, start_link, []},
            permanent,
            100,
            worker,
            [epam]},
            
           {mysql_conn,
            {mysql, start_link, [drink_log, ?LOG_DB_SERVER, undefined, 
                                 ?LOG_DB_USERNAME, LogDBPassword, ?LOG_DB_DATABASE, fun mysql_log/4]},
            permanent,
            100,
            worker,
            [mysql]},
            
           {drink_web_events,
            {drink_web_events, start_link, []},
            permanent,
            100,
            worker,
            [drink_web_events]}
		   
            %          {web_server,
            % {drink_web, start_link, []},
            % permanent,
            % 100,
            % worker,
            % [drink_web]}
		  ]
		}}.

mysql_log(_Module, _Line, _Level, _Fun) ->
    ok.
