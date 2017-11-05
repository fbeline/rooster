-module(routes_test).

-include_lib("eunit/include/eunit.hrl").

routes() ->
  [{"/account/:id", [auth],
    [{'GET', fun() -> acc end, [test]},
     {'POST', fun() -> new_acc end},
     {'GET', "/permissions", fun() -> permissions end}]}].

mixed_routes() ->
  routes() ++ [['GET', "/health", fun() -> health end]].

simple_test() ->
  ?assert(true).
