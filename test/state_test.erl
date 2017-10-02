-module(state_test).

-include_lib("eunit/include/eunit.hrl").


state() ->
  #{routes => [?MODULE]}.

middleware_double() ->
  #{name  => mid01,
    leave => fun(_Req, Resp) -> Resp * 2 end}.

exports() ->
  [{'POST', "test00", fun(_, _) -> {200, #{}} end, [middleware_double()]},
   {'GET', "test01", fun(_, _) -> {200, #{}} end, []}].

simple_test() ->
  {ok, Srv} = rooster_state:start(),
  gen_server:cast(Srv, {set_state, state()}),
  State = gen_server:call(Srv, get_state),
  erlang:display(State),
  ?assertEqual(#{resp_headers => [],
                 routes       => [state_test],
                 version      => "0.0.0"},
               State).
