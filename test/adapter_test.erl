-module(adapter_test).
-author("felipe").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

config_adapter_test() ->
  Expected = #{ip          => {0, 0, 0, 0},
               port        => 8080,
               static_path => ["priv", "www"],
               ssl         => {ssl, false},
               ssl_opts    => {ssl_opts, []}},
  ?assertEqual(Expected, rooster_adapter:config(#{})).

state_adapter_test() ->
  Expected = #{routes       => [],
               resp_headers => [],
               version      => "0.0.0"},
  ?assertEqual(Expected, rooster_adapter:state(#{})).
