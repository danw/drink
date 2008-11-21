-module (ssl_redirect).

-export ([out/1]).

-include ("yaws_api.hrl").

out(Arg) ->
    Headers = Arg#arg.headers,
    Query = case Arg#arg.querydata of
        [] ->
            [];
        Data when is_list(Data) ->
            "?" ++ Data;
        _ ->
            []
    end,
    case Headers#headers.host of
        undefined ->
            {redirect, "https://erlang" ++ Arg#arg.server_path ++ Query};
        Host ->
            {redirect, "https://" ++ hd(string:tokens(Host, ":")) ++ Arg#arg.server_path ++ Query}
    end.