-module(dispatcher_test).
-include_lib("../src/rooster.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([route_function/1, route_function_pparam/1]).

compare_route_tokens_simple_test() ->
	RequestedRoute = ["products", "save"],
	Route = ["products", "save"],
	?assertEqual({true, []},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).


compare_route_tokens_path_param_test() ->
	RequestedRoute = ["products", "10", "load"],
	Route = ["products", ":id", "load"],
	?assertEqual({true, [{":id", "10"}]},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).


compare_route_tokens_fail1_test() ->
	RequestedRoute = ["products", "10", "load"],
	Route = ["products", ":id", "update"],
	?assertEqual({false, {}},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).

compare_route_tokens_fail2_test() ->
	RequestedRoute = ["productz"],
	Route = ["products"],
	?assertEqual({false, {}},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).

parse_route_sanity_test() ->
	Tokens = dispatcher:parse_route("products/test?id=10"),
	?assertEqual(["products", "test"], Tokens). 

parse_route_path_params_test() ->
	Tokens = dispatcher:parse_route("products/10/save/1"),
	?assertEqual(["products", "10", "save", "1"], Tokens). 


call_route_function_body_test() ->
    Body = "{\n\t\"user\": \"felipe 1234\"\n}",
    Req = #request{body=Body},
    Resp = dispatcher:call_route_function(Req, {?MODULE, route_function}, {}),
    ?assertEqual({[{<<"user">>, <<"felipe 1234">>}]}, Resp).

call_route_function_pparam_test() ->
    Params = [{param1, 10}, {param2, 20}],
    Req = #request{},
    Resp = dispatcher:call_route_function(Req, {?MODULE, route_function_pparam}, Params),
    ?assertEqual(Params, Resp).

route_function(Req) ->
    Req#request.body.

route_function_pparam(Req) ->
    Req#request.pathParams.
