-module(state_test).

-include_lib("eunit/include/eunit.hrl").

state() ->
  #{routes => [?MODULE]}.

simple_test() ->
  {ok, Srv} = rooster_state:start_link(state()),
  State = gen_server:call(Srv, get_state),
  ?assertEqual(#{middleware   => [],
                 routes       => [state_test]},
               State).

