-module (drink_web).

-export ([out/1]).

-include ("yaws_api.hrl").
-include ("user.hrl").

-record (ses, {user=nil}).

out(A) ->
    {ok, Cookie, Session, SessionCookie} = session_start(A),
    RetJson = case list_to_binary(A#arg.appmoddata) of
        <<"userinfo/", UserBin/binary>> ->
            UserName = binary_to_list(UserBin),
            {ok, User} = user_auth:user(UserName),
            Ret = userref_to_struct(User),
            user_auth:delete_ref(User),
            Ret;
        <<"login">> ->
            nil;
        <<"setuser/", UserBin/binary>> ->
            yaws_api:replace_cookie_session(Cookie, Session#ses{user = binary_to_list(UserBin)}),
            true;
        <<"user">> ->
            case Session#ses.user of
                nil -> false;
                User -> User
            end;
        Else ->
            {struct, [{error, "unknown_path"}, {path, binary_to_list(Else)}]}
    end,
    % change to application/json when done testing
    [SessionCookie, {content, "text/plain", json:encode(RetJson)}, break].

userref_to_struct(UserRef) ->
    case user_auth:user_info(UserRef) of
        {ok, UserInfo} ->
            {struct, [
                {username, UserInfo#user.username},
                {credits, UserInfo#user.credits},
                {admin, UserInfo#user.admin}
            ]};
        {error, _Reason} ->
            {struct, [{error, "invalid user"}]}
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