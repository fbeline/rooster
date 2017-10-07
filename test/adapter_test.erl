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
  Expected = #{routes       => [],
               middleware   => [],
               resp_headers => [],
               version      => "0.0.0"},
  ?assertEqual(Expected, rooster_adapter:state(#{})).

middleware_test() ->
  Result = rooster_adapter:middleware(#{name => m_test}),
  ?assertEqual(m_test, maps:get(name, Result)),
  ?assert(is_function(maps:get(enter, Result))),
  ?assert(is_function(maps:get(leave, Result))).

flatt_routes_test() ->
  Result = rooster_adapter:flatt_routes(#{routes => [[1], [2]]}),
  ?assertEqual(#{routes => [1, 2]}, Result).
