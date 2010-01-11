%%%-------------------------------------------------------------------
%%% File    : drink_app.erl
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

-module (drink_app).
-behaviour (application).

-export ([start/2, stop/1]).
-export ([get_port/1]).

-include ("yaws.hrl").

start(_Type, StartArgs) ->
	case mnesia:start() of
		{error, _Reason} ->
			drink_mnesia:initialize();
		ok ->
			ok
	end,
	case os:getenv("USER") of
	    "root" ->
		application:set_env(drink, portoffset, 0);
	    _ ->
		% We won't have access to restricted ports :-/
		ok
	end,
	yaws_conf(),
	drink_sup:start_link(StartArgs).

stop(_State) ->
	ok.

get_port(Name) ->
	case application:get_env(Name) of
	    {ok, Port} when Port =< 1024 ->
		{ok, PortOffset} = application:get_env(portoffset),
		Port + PortOffset;
	    {ok, Port} ->
		Port;
	    _ ->
		error_logger:error_msg("Unknown port ~p~n", [Name]),
		nil
	end.

yaws_conf() ->    
	{ok, WebLog} = application:get_env(weblogs),
	{ok, Docroot} = application:get_env(docroot),
	{ok, ServerName} = application:get_env(servername),
	GC0 = yaws_config:make_default_gconf(false, "drinkweb"),
	GC = GC0#gconf{
	    logdir = WebLog,
	    flags = yaws:flag(GC0#gconf.flags, ?GC_COPY_ERRLOG, false),
	    cache_refresh_secs = 0
	},
	SSLKey = filename:join(code:priv_dir(drink), "key.pem"),
	SSLCert = filename:join(code:priv_dir(drink), "cert.pem"),
	SSLCA = filename:join(code:priv_dir(drink), "cshca.crt"),
	case {filelib:is_file(SSLKey), filelib:is_file(SSLCert), filelib:is_file(SSLCA)} of
	    {true, true, true} ->
		SC = #sconf{
		    port = get_port(web_port),
		    servername = "drink.csh.rit.edu",
		    listen = {0,0,0,0},
		    docroot = "",
		    allowed_scripts = [],
		    appmods = [{"/", ssl_redirect}]},
		Ssl = #ssl{
		    keyfile = SSLKey,
		    certfile = SSLCert,
		    cacertfile = SSLCA},
		SCssl = #sconf{
		    port = get_port(secure_web_port),
		    servername = "drink.csh.rit.edu",
		    listen = {0,0,0,0},
		    docroot = Docroot,
		    allowed_scripts = [yaws],
		    ssl = Ssl,
		    appmods = [{"/drink", drink_web}],
		    authdirs = [{"/", #auth{dir = "/", mod = authmod_webauth}}],
		    start_mod = authmod_webauth,
		    opaque = [
		        {webauth_keytab, "FILE:" ++ filename:join(code:priv_dir(drink), "webauth.keytab")},
		        {webauth_sslca, filename:join(code:priv_dir(drink), "cshca.crt")},
		        {webauth_login_url, "https://webauth.csh.rit.edu/login/"},
		        {webauth_kdc_url, "https://webauth.csh.rit.edu/webkdc-service/"},
		        {webauth_kdc_princ, "service/webkdc@CSH.RIT.EDU"}
		    ]},
		yaws_api:setconf(GC, [[SC], [SCssl]]);
	    _ ->
		error_logger:error_msg("Warning: SSL keys not found, operating without SSL~n"),
		SC = #sconf{
		    port = get_port(web_port),
		    servername = ServerName,
		    listen = {0, 0, 0, 0},
		    docroot = "",
		    allowed_scripts = [],
		    appmods = [{"/drink", drink_web}]},
		yaws_api:setconf(GC, [[SC]])
	end.
