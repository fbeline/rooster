-module(routes_test).

-include_lib("eunit/include/eunit.hrl").

routes(Fn) ->
  [{"/account/:id", [auth],
    [{'GET', Fn , [test]},
     {'POST', Fn},
     {'GET', "/permissions", Fn}]}].

mixed_routes(Fn) ->
  routes(Fn) ++ [{'GET', "/health", Fn}].

complete_routes(Fn) ->
  {"/account/:id", [auth],
   [{'GET', "/foo", Fn, [test]},
    {'POST', "/bar", Fn, [test]}]}.

expected_routes(Fn) ->
  [{'GET', "/account/:id", Fn, [auth, test]},
   {'POST', "/account/:id", Fn, [auth]},
   {'GET', "/account/:id/permissions", Fn, [auth]}].

expected_mixed_routes(Fn) ->
  [{'GET', "/health", Fn, []}] ++ expected_routes(Fn).

expected_complete_routes(Fn) ->
  [{'GET', "/account/:id/foo", Fn, [auth, test]},
   {'POST', "/account/:id/bar", Fn, [auth, test]}].

generic_routes() ->
  [{'GET', "/account/:id", fn, []},
   {'GET', "/account/foo", fn, []},
   {'POST', "/account/save", fn, []},
   {'POST', "/account/:id/status", fn, []}].

expected_sorted_routes() ->
  [{'GET', "/account/foo", fn, []},
   {'POST', "/account/save", fn, []},
   {'GET', "/account/:id", fn, []},
   {'POST', "/account/:id/status", fn, []}].

start_test() ->
  Fn = fun() -> foo end,
  rooster_route:start_link(routes(Fn)),
  Routes = rooster_route:get(),
  gen_server:stop(rooster_route),
  ?assert(erlang:is_list(Routes)).

routes_sanity_test() ->
  Fn = fun() -> foo end,
  rooster_route:start_link(routes(Fn)),
  Routes = rooster_route:get(),
  gen_server:stop(rooster_route),
  ?assertEqual(expected_routes(Fn), Routes).

mixed_routes_test() ->
  Fn = fun() -> foo end,
  rooster_route:terminate(nil, nil),
  rooster_route:start_link(mixed_routes(Fn)),
  Routes = rooster_route:get(),
  ?assertEqual(expected_mixed_routes(Fn), Routes).

nested_test() ->
  Fn = fun() -> foo end,
  {Path, Middleware, Nested} = complete_routes(Fn),
  Routes = rooster_route:nested(Path, Middleware, Nested),
  ?assertEqual(expected_complete_routes(Fn), Routes).

sort_test() ->
  Sorted = rooster_route:sort(generic_routes()),
  ?assertEqual(expected_sorted_routes(), Sorted).
