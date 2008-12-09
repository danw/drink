-module (drink_web).

-export ([out/1]).

-include ("yaws_api.hrl").
-include ("user.hrl").
-include ("drink_mnesia.hrl").

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
                "moduser" ->
                    case {Session#ses.user, yaws_api:postvar(A, "username"), yaws_api:postvar(A, "attr"), yaws_api:postvar(A, "value")} of
                        {nil, _, _, _} ->
                            error(permission_denied);
                        {AdminUser, {ok, UserName}, {ok, Attr}, {ok, Value}} ->
                            case user_auth:admin(AdminUser, UserName) of
                                {ok, User} ->
                                    mod_user(User, list_to_atom(Attr), Value);
                                {error, permission_denied} ->
                                    error(permission_denied);
                                {error, invalid_user} ->
                                    error(invalid_user);
                                {error, Reason} ->
                                    error(Reason);
                                _Else ->
                                    error(unknown)
                            end;
                        _ ->
                            error(invalid_args)
                    end;
                "machines" ->
                    error(wrong_method);
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
                "machines" ->
                    ok({struct, machines(drink_machines_sup:machines())});
                "logout" ->
                    yaws_api:replace_cookie_session(Cookie, #ses{}),
                    ok(true);
                "login" ->
                    error(wrong_method);
                "moduser" ->
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

machines([]) ->
    [];
machines([M|Machines]) ->
    [{M, machine_stat(M)}] ++ machines(Machines).

machine_stat(Machine) ->
    case {drink_machine:is_alive(Machine), drink_machine:temperature(Machine), drink_machine:slots(Machine)} of
        {true, {ok, Temperature}, {ok, Slots}} ->
            {struct, [
                {connected, true},
                {temperature, Temperature},
                {slots, {struct, slots(Slots)}}
            ]};
        {true, {error, no_temp}, {ok, Slots}} ->
            {struct, [
                {connected, true},
                {temperature, false},
                {slots, {struct, slots(Slots)}}
            ]};
        _else ->
            {struct, [
                {connected, false}
            ]}
    end.

slots([]) ->
    [];
slots([S|Slots]) ->
    [{integer_to_list(S#slot.num),slot_info(S)}] ++ slots(Slots).

slot_info(Slot) ->
    {struct, [
        {name, Slot#slot.name},
        {price, Slot#slot.price},
        {available, Slot#slot.avail}
    ]}.

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

mod_user(UserRef, admin, "true") -> mod_user(UserRef, admin, true);
mod_user(UserRef, admin, "false") -> mod_user(UserRef, admin, false);
mod_user(UserRef, admin, Value) when is_atom(Value) ->
    case user_auth:set_admin(UserRef, Value) of
        ok ->
            userref_to_struct(UserRef);
        {error, Reason} ->
            error(Reason)
    end;
mod_user(_UserRef, modcredits, _Value) -> error(not_implemented);
mod_user(_UserRef, delibutton, _Value) -> error(not_implemented);
mod_user(_UserRef, addibutton, _Value) -> error(not_implemented);
mod_user(_, _, _) ->
    error(invalid_args).

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