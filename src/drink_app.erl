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

-include ("yaws.hrl").

start(_Type, StartArgs) ->
	case mnesia:start() of
		{error, _Reason} ->
			drink_mnesia:initialize();
		ok ->
			ok
	end,
	yaws_conf(),
	drink_sup:start_link(StartArgs).

stop(_State) ->
	ok.

yaws_conf() ->    
	{ok, WebLog} = application:get_env(drink, weblogs),
	{ok, Docroot} = application:get_env(drink, docroot),
	GC0 = yaws_config:make_default_gconf(false, "drinkweb"),
	GC = GC0#gconf{
	    logdir = WebLog,
	    flags = yaws:flag(GC0#gconf.flags, ?GC_COPY_ERRLOG, false),
	    cache_refresh_secs = 0
	},
	SC = #sconf{
	    port = 80,
	    servername = "drink.csh.rit.edu",
	    listen = {0,0,0,0},
	    docroot = "",
	    allowed_scripts = [],
	    appmods = [{"/", ssl_redirect}]},
	Ssl = #ssl{
	    keyfile = filename:join(code:priv_dir(drink), "key.pem"),
	    certfile = filename:join(code:priv_dir(drink), "cert.pem"),
	    cacertfile = filename:join(code:priv_dir(drink), "cshca.crt")},
	SCssl = #sconf{
	    port = 443,
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
	yaws_api:setconf(GC, [[SC], [SCssl]]).