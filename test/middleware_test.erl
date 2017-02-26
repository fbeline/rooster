-module(middleware_test).

-include_lib("../include/rooster.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([some_route_before/2, some_route_after/2]).

match_before_test() ->
    Middlewares = exports(),
    Req = #request{path="lena/soderberg"},
    {_,Resp} = rooster_middleware:match('BEFORE', Req, Middlewares, undefined),
    ?assertEqual("hi", Resp).

match_after_test() ->
    Middlewares = exports(),
    Req = #request{path="lena/soderberg"},
    {_,Resp} = rooster_middleware:match('AFTER', Req, Middlewares, "hi,"),
    ?assertEqual("hi, my dear", Resp).

no_middleware_test() ->
    {next,Resp} = rooster_middleware:match('BEFORE', #request{}, [], undefined),
    ?assertEqual(undefined, Resp).

some_route_before(_Req, _Resp) ->
    {next,"hi"}.

some_route_after(_Req, Resp) ->
    {next, Resp ++ " my dear"}.

exports() ->
    [{?MODULE, 'BEFORE', ".*", some_route_before},
     {?MODULE, 'AFTER', ".*", some_route_after}].
