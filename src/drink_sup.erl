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
    case {user_auth_provider(), logger_provider()} of
        {{ok, UserAuthChildren}, {ok, LoggerChildren}} ->
            {ok, {{one_for_one, 10, 3},  % One for one restart, shutdown after 10 restarts within 3 seconds
                common_children() ++ LoggerChildren ++ UserAuthChildren}};
        {_,_} ->
        {error, cant_start}
    end.

common_children() ->
    [{machine_listener,     % Our first child, the drink_machine_listener
      {dw_gen_listener, start_link, [drink_app:get_port(machine_listen_port), {drink_machine_comm, start_link, []}]},
      permanent,            % Always restart
      100,                  % Allow 10 seconds for it to shutdown
      worker,               % It isn't a supervisor
      [dw_gen_listener]},

     {machines,             % The Supervisor for connected machines
      {drink_machines_sup, start_link, []},
      permanent,            % Always restart the supervisor
      infinity,             % Wait forever for the supervisor
      supervisor,
      [drink_machines_sup]}, % Uses the drink_machines_sup Module

     {pam_auth,
      {epam, start_link, []},
      permanent,
      100,
      worker,
      [epam]}].

user_auth_provider() ->
    case application:get_env(user_auth_provider) of
        {ok, Mod} ->
            {ok, user_auth_provider_entry(Mod)};
        Else ->
            error_logger:error_msg("Error: Unknown user_auth_provider: ~p~n", [Else]),
            {error, unknown_user_auth_provider}
    end.

user_auth_provider_entry(Provider) ->
    [{user_auth,
      {user_auth, start_link, [Provider]},
      permanent,
      100,
      worker,
      [user_auth]}].

logger_provider() ->
    case application:get_env(use_log_db) of
        {ok, true} ->
            DBPassFile = filename:join(code:priv_dir(drink), "dbpass"),
            case filelib:is_file(DBPassFile) of
                true ->
                    case {application:get_env(log_db_server), application:get_env(log_db_user), application:get_env(log_db_database), file:read_file(DBPassFile)} of
                        {{ok, LogDBServer}, {ok, LogDBUser}, {ok, LogDBDatabase}, {ok, Bin}} ->
                            LogDBPassword = binary_to_list(Bin),
                            {ok, [{mysql_conn,
                                   {mysql, start_link, [drink_log, LogDBServer, undefined, LogDBUser, LogDBPassword, LogDBDatabase, fun mysql_log/4]},
                                   permanent,
                                   100,
                                   worker,
                                   [mysql]}]};
                        {A, B, C, _} ->
                            error_logger:error_msg("Error: Log DB misconfigured: '~p' '~p' '~p'~n", [A,B,C]),
                            {error, log_db_args}
                    end;
                _ ->
                    error_logger:error_msg("Error: Log DB Password missing, and use_log_db = true~n"),
                    {error, log_db_password_missing}
            end;
        {ok, false} ->
            {ok, []};
        Else ->
            error_logger:error_msg("Error: Unknown use_log_db: ~p~n", [Else]),
            {error, unknown_use_log_db}
    end.

% Logging function for the mysql module
mysql_log(_Module, _Line, _Level, _Fun) ->
    ok.
