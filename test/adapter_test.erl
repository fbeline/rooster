-module(adapter_test).

-include_lib("eunit/include/eunit.hrl").

config_adapter_test() ->
  Expected = #{ip          => {0, 0, 0, 0},
               port        => 8080,
               static_path => ["priv", "www"],
               ssl         => {ssl, false},
               ssl_opts    => {ssl_opts, []}},
  ?assertEqual(Expected, rooster_adapter:config(#{})).

state_adapter_test() ->
  Expected = #{routes     => [],
               middleware => []},
  ?assertEqual(Expected, rooster_adapter:state(#{})).

middleware_test() ->
  Result = rooster_adapter:middleware(#{name => m_test}),
  ?assertEqual(m_test, maps:get(name, Result)),
  ?assert(is_function(maps:get(enter, Result))),
  ?assert(is_function(maps:get(leave, Result))).

flatt_routes_test() ->
  Result = rooster_adapter:flatt_routes(#{routes => [[1], [2]]}),
  ?assertEqual(#{routes => [1, 2]}, Result).

add_base_middleware_test() ->
  Expected = #{routes     => [{'GET', "foo", foo, rooster_adapter:base_middleware()}],
               middleware => []},
  Input = #{routes     => [{'GET', "foo", foo}],
            middleware => []},
  ?assertEqual(Expected, rooster_adapter:add_base_middleware(Input)).

server_response_test() ->
  Result = rooster_adapter:server_response({200, #{}, [{"authorization","foo"}]}),
  ?assertEqual({200,
                [{"Content-type", "application/json"},
                 {"authorization","foo"}],
                <<"{}">>}, Result).

route_response_test() ->
  Result = rooster_adapter:route_response({200, #{}}),
  ?assertEqual({200, #{}, []}, Result).

route_response_with_header_test() ->
  Result = rooster_adapter:route_response({200, #{}, [{"foo", "bar"}]}),
  ?assertEqual({200, #{}, [{"foo", "bar"}]}, Result).

base_headers_test() ->
  ?assertEqual([{"Content-type", "application/json"}], rooster_adapter:base_headers()).

base_middleware_test() ->
  ?assertEqual([], rooster_adapter:base_middleware()).

nested_route_test() ->
  Fn = fun() -> 1 end,
  Nested = [{'GET', Fn, [test]},
            {'POST', Fn},
            {'GET', "/permissions", Fn},
            {'GET', "/health", Fn, [test]}],
  Expected = [{'GET', "", Fn, [test]},
              {'POST',"", Fn, []},
              {'GET', "/permissions", Fn, []},
              {'GET', "/health", Fn, [test]}],
  Result = rooster_adapter:nested_route(Nested),
  ?assertEqual(Expected, Result).
