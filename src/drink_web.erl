-module (drink_web).

-export ([out/1]).

-include ("yaws_api.hrl").
-include ("user.hrl").

-record (ses, {user=nil}).

out(A) ->
    {ok, Cookie, Session, SessionCookie} = session_start(A),
    RetJson = case (A#arg.req)#http_request.method of
        'POST' ->
            case A#arg.appmoddata of
                "login" ->
                    case {yaws_api:postvar(A, "username"), yaws_api:postvar(A, "password")} of
                        {{ok, UserName}, {ok, Pass}} ->
                            case user_auth:auth(UserName, Pass) of
                                {ok, User} ->
                                    yaws_api:replace_cookie_session(Cookie, Session#ses{user=User}),
                                    userref_to_struct(User);
                                _Else ->
                                    error(bad_auth)
                            end;
                        _Else ->
                            error(invalid_args)
                    end;
                "logout" ->
                    error(wrong_method);
                "userinfo" ->
                    error(wrong_method);
                "currentuser" ->
                    error(wrong_method);
                _Else ->
                    error(unknown_path)
            end;
        'GET' ->
            case A#arg.appmoddata of
                "userinfo" ->
                    case yaws_api:queryvar(A, "user") of
                        {ok, UserName} ->
                            case user_auth:user(Session#ses.user, UserName) of
                                {ok, User} ->
                                    Ret = userref_to_struct(User),
                                    user_auth:delete_ref(User),
                                    Ret;
                                _Else ->
                                    error(invalid_user)
                            end;
                        _Else ->
                            error(invalid_args)
                    end;
                "currentuser" ->
                    case Session#ses.user of
                        nil ->
                            error(not_logged_in);
                        User ->
                            userref_to_struct(User)
                    end;
                "logout" ->
                    yaws_api:replace_cookie_session(Cookie, #ses{}),
                    ok(true);
                "login" ->
                    error(wrong_method);
                _Else ->
                    error(unknown_path)
            end;
        _Else ->
            error(wrong_method)
    end,
    % change to application/json when done testing
    [SessionCookie, {content, "text/plain", json:encode(RetJson)}, break].

ok(Data) ->
    {struct, [{status, "ok"}, {data, Data}]}.

error(Reason) when is_atom(Reason) ->
    error(atom_to_list(Reason));
error(Reason) ->
    {struct, [{status, "error"}, {reason, Reason}]}.

userref_to_struct(UserRef) ->
    case user_auth:user_info(UserRef) of
        {ok, UserInfo} ->
            ok({struct, [
                {username, UserInfo#user.username},
                {credits, UserInfo#user.credits},
                {admin, UserInfo#user.admin},
                {ibuttons, {array, UserInfo#user.ibuttons}}
            ]});
        {error, _Reason} ->
            error(invalid_user)
    end.

session_start(A) ->
    H = A#arg.headers,
    case yaws_api:find_cookie_val("ssid", H#headers.cookie) of
        Val when Val /= [] ->
            case yaws_api:cookieval_to_opaque(Val) of
                {ok, Sess} ->
                    {ok, Val, Sess, ok};
                _Else ->
                    error_logger:error_msg("No session, creating new cookie"),
                    Session = #ses{},
                    Cookie = yaws_api:new_cookie_session(Session),
                    {ok, Cookie, Session, yaws_api:setcookie("ssid", Cookie, "/")}
            end;
        [] ->
            error_logger:error_msg("No cookie, creating new session"),
            Session = #ses{},
            Cookie = yaws_api:new_cookie_session(Session),
            {ok, Cookie, Session, yaws_api:setcookie("ssid", Cookie, "/")}
    end.