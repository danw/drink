%%%-------------------------------------------------------------------
%%% File    : drink_sup.erl
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

-module (drink_sup).
-behaviour (supervisor).

-export ([start/0, start_link/1, init/1]).

start () ->
	start_link([]).

start_link (Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init ([]) ->
	DBPassFile = filename:join(code:priv_dir(drink), "dbpass"),
	case filelib:is_file(DBPassFile) of
	    true ->
		{ok, LogDBServer} = application:get_env(log_db_server),
		{ok, LogDBUser} = application:get_env(log_db_user),
		{ok, LogDBDatabase} = application:get_env(log_db_database),
		{ok, Bin} = file:read_file(DBPassFile),
		LogDBPassword = binary_to_list(Bin),
		LogDBSpec = [{mysql_conn,
		    {mysql, start_link, [drink_log, LogDBServer, undefined, 
		     LogDBUser, LogDBPassword, LogDBDatabase, fun mysql_log/4]},
		    permanent,
		    100,
		    worker,
		    [mysql]}];
	    _ ->
		error_logger:error_msg("Warning: Log DB Password missing, skipping log db!~n"),
		LogDBSpec = []
	end,

	{ok, {{one_for_one, 10, 3},  % One for one restart, shutdown after 10 restarts within 3 seconds
		  [{machine_listener,    % Our first child, the drink_machine_listener
			{gen_listener, start_link, [drink_app:get_port(machine_listen_port), {drink_machine_comm, start_link, []}]},
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
		    {gen_listener, start_link, [drink_app:get_port(sunday_server_port), {sunday_server, start_link, []}]},
		    permanent,
		    100,
		    worker,
		    [gen_listener]},
		    
		   {finger_server_listener,
		    {gen_listener, start_link, [drink_app:get_port(finger_server_port), {finger_server, start_link, []}]},
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
            
           {drink_web_events,
            {drink_web_events, start_link, []},
            permanent,
            100,
            worker,
            [drink_web_events]}
		  ] ++ LogDBSpec
		}}.

% Logging function for the mysql module
mysql_log(_Module, _Line, _Level, _Fun) ->
    ok.
