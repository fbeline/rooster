-module(adapter_test).
-author("felipe").

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

base_headers_test() ->
  ?assertEqual(rooster_adapter:base_headers(), [{"Content-type", "application/json"}]).

add_headers_test() ->
  Expected = #{routes     => [{'GET', "foo", foo, [{"Content-type", "application/json"}]}],
               middleware => []},
  Input = #{routes     => [{'GET', "foo", foo}],
            middleware => []},
  ?assertEqual(Expected, rooster_adapter:add_headers(Input)).

server_response_test() ->
  Result = rooster_adapter:server_response({200, #{}, [{"authorization","foo"}]}),
  ?assertEqual({200,
                [{"authorization","foo"}],
                <<"{}">>}, Result).

route_response_test() ->
  Result = rooster_adapter:route_response({200, #{}}),
  ?assertEqual({200, #{}, []}, Result).

route_response_with_header_test() ->
  Result = rooster_adapter:route_response({200, #{}, [{"foo", "bar"}]}),
  ?assertEqual({200, #{}, [{"foo", "bar"}]}, Result).
