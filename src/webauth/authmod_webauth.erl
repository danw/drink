%%%-------------------------------------------------------------------
%%% File    : authmod_webauth.erl
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

-module (authmod_webauth).
-behaviour (gen_server).

-export ([start/1, stop/0, auth/2, out/1]).
-export ([init/1, terminate/2, code_change/3, handle_call/3, handle_info/2, handle_cast/2]).

-export ([token_attr_split/1, token_descape/1]).

-include ("yaws.hrl").
-include ("yaws_api.hrl").

-record (webauth_key, {name, type = aes, data, creation, valid_after, expires, token}).
-record (was_state, {port, keytab, sslca, login_url, kdc_url, kdc_princ, service_token, keys = []}).

-define (SERVER, ?MODULE).
-define (SUPERVISOR, yaws_sup).

-define(ENABLE_DEBUG, yes).

-ifdef(ENABLE_DEBUG).
-define(INFO, io:format).
-define(DEBUG, io:format).
-else.
-define(INFO, ignore).
-define(DEBUG, ignore).
ignore(_) -> ok.
ignore(_,_) -> ok.
-endif.

-define(WARNING, io:format).
-define(ERROR, io:format).

-define (WEBAUTH_COOKIE, "webauth_at").
-define (WEBAUTH_RETURN_ARG, "WEBAUTHR=").

% Yaws API
start(Sconf) when is_record(Sconf, sconf) ->
    ?WARNING("Starting authmod_webauth~n"),
    ChildSpec = {?SERVER,
                 {gen_server, start_link, [{local, ?SERVER}, ?MODULE, Sconf, []]},
                 permanent,
                 1000,
                 worker,
                 [?MODULE]},
    supervisor:start_child(?SUPERVISOR, ChildSpec);
start(_) ->
    ?ERROR("Didn't get Sconf!!!~n").

stop() ->
    supervisor:terminate_child(?SUPERVISOR, ?SERVER),
    supervisor:delete_child(?SUPERVISOR, ?SERVER).

auth(Arg, Auth) when is_record(Arg, arg), is_record(Auth, auth) ->
    gen_server:call(?SERVER, {auth, Arg, Auth}).

out(Arg) when is_record(Arg, arg) -> % Strip webauth cookies, redirect to webkdc for login
    gen_server:call(?SERVER, {handle, Arg}).

% gen_server callbacks
init(Sconf) when is_record(Sconf, sconf) ->
    {value, {_, Keytab}} = lists:keysearch(webauth_keytab, 1, Sconf#sconf.opaque),
    {value, {_, Sslca}} = lists:keysearch(webauth_sslca, 1, Sconf#sconf.opaque),
    {value, {_, LoginUrl}} = lists:keysearch(webauth_login_url, 1, Sconf#sconf.opaque),
    {value, {_, KdcUrl}} = lists:keysearch(webauth_kdc_url, 1, Sconf#sconf.opaque),
    {value, {_, KdcPrinc}} = lists:keysearch(webauth_kdc_princ, 1, Sconf#sconf.opaque),
    
    % Start the necessary apps
    application:start(ssl),
    % ssl app needs seeding
    %ssl:seed(crypto:rand_bytes(32)),
    
    % Start the port
    
    PortName = filename:join(code:priv_dir(drink), "ewebauth"),
    Port = open_port({spawn, PortName}, [{packet, 2}, binary, exit_status]),
    
    AppKey = #webauth_key{name = app, data = crypto:rand_bytes(16)},
    
    {ok, #was_state{
        port = Port,
        keytab = Keytab,
        sslca = Sslca,
        login_url = LoginUrl,
        kdc_url = KdcUrl,
        kdc_princ = KdcPrinc,
        keys = [AppKey]
    }}.

terminate(_Reason, State) when is_record(State, was_state) ->
    case State#was_state.port of
        nil ->
            ok;
        Port ->
            port_close(State#was_state.port)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({auth, Arg, _Auth}, _From, State) ->
    % TODO: just fail for xmlhttprequests
    case yaws_api:find_cookie_val(?WEBAUTH_COOKIE, Arg) of
        [] -> % No cookie, forward to webkdc - TODO: catch response from webkdc
            {reply, {appmod, authmod_webauth}, State};
        CookieStr -> % Found cookie, check validity, insert user record
            case webauth_cookie_valid(CookieStr, State) of
                {ok, User} ->
                    io:format("got user: ~p~n", [User]),
                    {reply, {true, {User, undefined, undefined}}, State};
                Else ->
                    error_logger:error_msg(":( bad cookie: ~p~n", [Else]),
                    {reply, {appmod, authmod_webauth}, State}
            end
    end;
handle_call({handle, Arg}, _From, State) ->
    % error_logger:error_msg("Got WEBAUTHR: ~p / ~p ~n", [yaws_api:getvar(Arg, "WEBAUTHR"), yaws_api:queryvar(Arg, "WEBAUTHR")]),
    % error_logger:error_msg("Args: ~p ~n", [Arg#arg.querydata]),
    case Arg#arg.querydata of
        ?WEBAUTH_RETURN_ARG ++ RetTokenPlus -> % Handle KDC response
            % Remove the trailing ;
            RetToken = lists:sublist(RetTokenPlus, length(RetTokenPlus) - 1),
            case catch webkdc_request_reply(RetToken, State) of
                {ok, {User, _, _} = Attrs, NewState} ->
                    AppToken = webauth_cookie_create(Attrs, NewState),
                    Cookie = yaws_api:setcookie(?WEBAUTH_COOKIE, AppToken, "/", "", "", on),
                    {reply, [Cookie, {html, "Welcome, " ++ User}], NewState};
                {error, Reason, NewState} ->
                    {reply, [{html, "Permission Denied: " ++ io_lib:format("~p", [Reason])}], NewState};
                E ->
                    {reply, [{html, "Permission Denied: " ++ io_lib:format("~p", [E])}], State}
            end;
        _ ->
            % TODO: strip webauth cookies?
            {ok, Url, NewState} = webkdc_request_url(Arg, State),
            {reply, {redirect, Url}, NewState}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info({Port, {data, _Data}}, State = #was_state{port = Port}) ->
    ?INFO("Got some extra port data when not listening"),
    {noreply, State};
handle_info({Port, {exit_status, _}}, State = #was_state{port = Port}) ->
    {stop, port_died, State#was_state{port = undefined}};
handle_info(_Request, State) ->
    {noreply, State}.

% Private calls
app_key(State) ->
    AppKeys = lists:filter(fun(Key) ->
            (Key#webauth_key.name =:= app)
        end, State#was_state.keys),
    case AppKeys of
        [] ->
            {error, no_app_key};
        Keys ->
            {ok, hd(Keys)}
    end.

session_key(State) ->
    Now = calendar:universal_time(),
    SessionKeys = lists:filter(fun(Key) -> 
            ((Key#webauth_key.name =:= session) and (Now > Key#webauth_key.valid_after) and (Now < Key#webauth_key.expires))
        end, State#was_state.keys),
    case SessionKeys of
        [] ->
            request_new_session_key(State);
        Keys ->
            {ok, hd(Keys), State}
    end.

session_key_request(State = #was_state{port = Port, keytab = Keytab, kdc_princ = KdcPrinc}) ->
    Data = term_to_binary({session_key_req, nil, {Keytab, KdcPrinc}}),
    port_command(Port, Data),
    receive
        {Port, {data, D}} ->
            {session_key_req, nil, Res} = binary_to_term(D),
            case Res of
                {ok, Req} ->
                    Req;
                {error, Reason} ->
                    throw(Reason);
                _Else ->
                    throw(unknown)
            end;
        {Port, {exit_status, _}} ->
            throw(port_died)
    after 30000 ->
        throw(timeout)
    end.

request_new_session_key(State) ->
    Cred = session_key_request(State),
    Credential = base64:encode(Cred),
    Body = <<"<getTokensRequest><requesterCredential type=\"krb5\">", Credential/binary, "</requesterCredential><tokens><token type=\"service\" /></tokens></getTokensRequest>">>,
    {ok, Result} = http:request(
        post,
        {State#was_state.kdc_url, [], "text/xml", Body},
        [{timeout, 30000}, {ssl, [
            {verify, 2},
            {cacertfile, State#was_state.sslca}
        ]}],
        [{body_format, binary}]),
    case Result of
        {{_, 200, _}, Headers, ResBody} ->
            case erlsom:simple_form(ResBody) of
                {ok, {"getTokensResponse", [], [{"tokens", [], [{"token", [], Data}]}]}, []} ->
                    Creation = calendar:universal_time(),
                    Expires = request_key_expires(Data),
                    Token = request_key_data("tokenData", Data),
                    Key = base64:decode(request_key_data("sessionKey", Data)),
                    SessionKey = #webauth_key{name = session, data = Key, creation = Creation, expires = Expires, valid_after = Creation, token = Token},
                    {ok, SessionKey, State#was_state{keys = State#was_state.keys ++ [SessionKey]}};
                Else ->
                    {error, Else, State}
            end;
        _Else ->
            {error, kdc_error, State}
    end.

request_key_data(Type, Data) ->
    {value, {Type, [], [Ret]}} = lists:keysearch(Type, 1, Data),
    Ret.

request_key_expires(Data) ->
    case string:to_integer(request_key_data("expires", Data)) of
        {error, _Reason} ->
            calendar:universal_time();
        {Expires, _Rest} ->
            calendar:gregorian_seconds_to_datetime(
                Expires + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}))
    end.

webauth_cookie_valid(CookieStr, State) ->
    {ok, AppKey} = app_key(State),
    case catch token_decode(CookieStr, AppKey) of
        Attrs when is_list(Attrs) ->
            case {lists:keysearch(t, 1, Attrs), lists:keysearch(et, 1, Attrs), lists:keysearch(s, 1, Attrs)} of
                {{value, {t, "app"}}, {value, {et, ExpireTime}}, {value, {s, User}}} ->
                    Now = calendar:universal_time(),
                    if
                        ExpireTime > Now ->
                            {ok, User};
                        true ->
                            {error, expired}
                    end;
                Else ->
                    {error, Else}
            end;
        Else ->
            {error, Else}
    end.

webauth_cookie_create({User, Created, Expires}, State) ->
    Attrs = [{t, app}, {et, Expires}, {ct, Created}, {s, User}],
    {ok, AppKey} = app_key(State),
    token_encode(Attrs, AppKey).

webkdc_request_url(Arg, State) ->
    {ok, SessionKey, NewState} = session_key(State),
    {ok, State#was_state.login_url ++ 
        "?RT=" ++ binary_to_list(webkdc_request_token(Arg, SessionKey, NewState)) ++ 
        ";ST=" ++ SessionKey#webauth_key.token, 
            NewState}.

webkdc_request_token(Arg, SessionKey, State) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    token_encode([
        {t, req},
        {ct, <<Now:32/big>>},
        {ru, "https://drink.csh.rit.edu/docs/sunday.html"},
        {rtt, id},
        {sa, webkdc}
            ], SessionKey).

webkdc_request_reply(Token, State) ->
    {ok, SessionKey, NewState} = session_key(State),
    Attrs = token_decode(Token, SessionKey),
    case {lists:keysearch(t, 1, Attrs), lists:keysearch(s, 1, Attrs), lists:keysearch(ct, 1, Attrs), lists:keysearch(et, 1, Attrs)} of
        {{value, {t, "id"}},
         {value, {s, User}},
         {value, {ct, CreationTime}},
         {value, {et, Expires}}} ->
            {ok, {User, CreationTime, Expires}, NewState};
        Else ->
            {error, Else, NewState}
    end.

token_encode(Attrs, Key) ->
    Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Random = crypto:rand_bytes(16),
    AttrString = list_to_binary(lists:flatten(lists:map(fun({Name, Value}) -> [atom_to_list(Name), "=", token_escape(Value), ";"] end, Attrs))),
    Padding = token_padding(size(AttrString) + 20), % 20 for the HMAC
    Ending = <<AttrString/binary, Padding/binary>>,
    HMAC = crypto:sha_mac(Key#webauth_key.data, Ending),
    Payload = <<Random/binary, HMAC/binary, Ending/binary>>,
    Encrypted = crypto:aes_cbc_128_encrypt(Key#webauth_key.data, <<0:128>>, Payload),
    base64:encode(<<Time:32/big, Encrypted/binary>>).

token_escape(Str) when is_list(Str) ->
    lists:flatten(lists:map(fun(Chr) ->
            case Chr of
                $; ->
                    [$;,$;];
                Else ->
                    Else
            end
        end, Str));
token_escape(Bin) when is_binary(Bin) ->
    token_escape(binary_to_list(Bin));
token_escape(Atom) when is_atom(Atom) ->
    token_escape(atom_to_list(Atom));
token_escape({{_,_,_}, {_,_,_}} = Time) ->
    DateTime = calendar:datetime_to_gregorian_seconds(Time) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    token_escape(<<DateTime:32/big>>);
token_escape(Str) -> Str.

token_descape([]) -> [];
token_descape([$;, $;]) -> [$;];
token_descape([$; | [$; | Rest]]) ->
    [$;] ++ token_descape(Rest);
token_descape([A | Rest]) ->
    [A] ++ token_descape(Rest).

token_padding(Bin) when is_binary(Bin) ->
    token_padding(size(Bin));
token_padding(Size) when is_integer(Size) ->
    Len = 16 - (Size rem 16),
    list_to_binary(lists:duplicate(Len, Len)).

token_decode(Str, Key) when is_list(Str) ->
    token_decode(base64:decode(Str), Key);
token_decode(<<Time:32/big, Encrypted/binary>>, Key) ->
    <<_Random:16/binary, HMAC:20/binary, Ending/binary>> = crypto:aes_cbc_128_decrypt(Key#webauth_key.data, <<0:128>>, Encrypted),
    HMAC = crypto:sha_mac(Key#webauth_key.data, Ending),
    PaddingPos = size(Ending) - 1,
    <<_:PaddingPos/binary, PaddingLen:8>> = Ending,
    AttrLen = size(Ending) - PaddingLen,
    <<Attrs:AttrLen/binary, _/binary>> = Ending,
    token_attr_split(binary_to_list(Attrs)).

token_attr_split([]) -> [];
token_attr_split(Attrs) ->
    NameEnd = string:chr(Attrs, $=),
    Name = list_to_atom(lists:sublist(Attrs, 1, NameEnd - 1)),
    ValueEnd = token_attr_split_escaped(Attrs, NameEnd + 1),
    Value = lists:sublist(Attrs, NameEnd + 1, ValueEnd - NameEnd - 2),
    [{Name, token_value_format(Name, token_descape(Value))}] ++ token_attr_split(lists:sublist(Attrs, ValueEnd, length(Attrs))).

token_attr_split_escaped(Str, StartAt) ->
    List = lists:sublist(Str, StartAt, length(Str)),
    Found = string:chr(List, $;),
    if
        length(List) >= (Found + 1) ->
            case lists:nth(Found + 1, List) of
                $; ->
                    token_attr_split_escaped(Str, StartAt + Found + 1);
                _ ->
                    Found + StartAt
            end;
        true ->
            Found + StartAt
    end.

token_value_format(ct, V) -> token_value_format(binary_date, list_to_binary(V));
token_value_format(et, V) -> token_value_format(binary_date, list_to_binary(V));
token_value_format(binary_date, <<Time:32/big>>) ->
    calendar:gregorian_seconds_to_datetime(Time + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}));
token_value_format(_, Value) -> Value.