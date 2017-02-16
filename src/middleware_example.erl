-module(middleware_example).
-export([exports/0,foo/2, basic_auth/2]).

-include_lib("rooster.hrl").

foo(_Req, _Resp) ->
    "something...".

basic_auth(Req, _Resp) ->
    Auth = Req#request.authorization,
    case Auth of
        "Basic" ++ Params ->
            Params;
        _ ->
            {permission_denied}
    end.

exports() ->
    [{'BEFORE', ".*", basic_auth}].
