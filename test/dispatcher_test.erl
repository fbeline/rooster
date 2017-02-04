-module(dispatcher_test).

-include_lib("eunit/include/eunit.hrl").

compare_route_tokens_simple_test() ->
	RequestedRoute = ["products", "save"],
	Route = ["products", "save"],
	?assertEqual({true, {path_params, []}},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).


compare_route_tokens_path_param_test() ->
	RequestedRoute = ["products", "10", "load"],
	Route = ["products", ":id", "load"],
	?assertEqual({true, {path_params, [{":id", "10"}]}},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).


compare_route_tokens_fail1_test() ->
	RequestedRoute = ["products", "10", "load"],
	Route = ["products", ":id", "update"],
	?assertEqual({false, []},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).

compare_route_tokens_fail2_test() ->
	RequestedRoute = ["productz"],
	Route = ["products"],
	?assertEqual({false, []},
		dispatcher:compare_route_tokens(Route, RequestedRoute, [])).

parse_route_sanity_test() ->
	Tokens = dispatcher:parse_route("products/test?id=10"),
	?assertEqual(["products", "test"], Tokens). 

parse_route_path_params_test() ->
	Tokens = dispatcher:parse_route("products/10/save/1"),
	?assertEqual(["products", "10", "save", "1"], Tokens). 
