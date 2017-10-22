-module(state_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, Srv} = rooster_state:start_link(#{}),
  State = gen_server:call(Srv, get_state),
  ?assertEqual(#{middleware   => [],
                 routes       => []},
               State).
