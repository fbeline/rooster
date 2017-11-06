-module(state_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  rooster_state:start_link(#{}),
  State = gen_server:call(rooster_state, get_state),
  rooster_state:stop(),
  ?assertEqual(#{}, State).

set_test() ->
  rooster_state:start_link(#{}),
  gen_server:cast(rooster_state, {set_state, #{middleware => [foo]}}),
  State = gen_server:call(rooster_state, get_state),
  rooster_state:stop(),
  ?assertEqual(#{middleware => [foo]}, State).
