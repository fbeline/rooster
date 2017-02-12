-module(middleware_example).
-export([exports/0,foo/2]).


foo(_Req, _Resp) ->
    "something...".


exports() ->
    [{'BEFORE', ".*", foo}].
