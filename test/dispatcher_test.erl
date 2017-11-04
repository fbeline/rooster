-module(dispatcher_test).
-include_lib("eunit/include/eunit.hrl").

path_params_valid_test() ->
  RequestedRoute = ["products", "save"],
  Route = ["products", "save"],
  ?assertEqual({ok, #{}},
    rooster_dispatcher:path_params(Route, RequestedRoute)).

path_params_with_values_test() ->
  RequestedRoute = ["products", "10", "load"],
  Route = ["products", ":id", "load"],
  ?assertEqual({ok, #{id => "10"}},
    rooster_dispatcher:path_params(Route, RequestedRoute)).

path_params_invalid_test() ->
  RequestedRoute = ["productz"],
  Route = ["products"],
  ?assertEqual({not_match, #{}},
    rooster_dispatcher:path_params(Route, RequestedRoute)).

path_params_wrong_number_of_tokens_test() ->
  RequestedRoute = ["products", "10"],
  Route = ["products", ":id", "update"],
  ?assertEqual({not_match, #{}},
    rooster_dispatcher:path_params(Route, RequestedRoute)).

parse_route_sanity_test() ->
  Tokens = rooster_dispatcher:parse_route("products/test?id=10"),
  ?assertEqual(["products", "test"], Tokens).

parse_route_path_params_test() ->
  Tokens = rooster_dispatcher:parse_route("products/10/save/1"),
  ?assertEqual(["products", "10", "save", "1"], Tokens).
