-module(middleware_example).
-export([exports/0, basic_auth/2]).

-include_lib("rooster.hrl").

basic_auth(Req, Resp) ->
    Auth = Req#request.authorization,
    Authorizated = rooster_basic_auth:is_authorized(Auth, {"admin", "admin"}),
    case Authorizated of
        true ->
            {next, Resp};
        _ ->
            {break, {403, {[{<<"reason">>, <<"Acess Forbidden">>}]}}}
    end. 


exports() ->
    [{'BEFORE', ".*", basic_auth}].
