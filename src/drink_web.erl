-module (drink_web).

-export ([out/1]).

-include ("yaws_api.hrl").

out(_Arg) ->
    {html, "Drink!"}.