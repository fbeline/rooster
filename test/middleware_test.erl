-module(middleware_test).

-include_lib("../include/rooster.hrl").
-include_lib("eunit/include/eunit.hrl").


middleware() ->
  [#{name => foo, leave => fun(_, Resp) -> {null, Resp * 2} end},
   #{name => bar, enter => fun(Req, _) -> {Req * 3, null} end}].

server_start_test() ->
  {ok, Pid} = rooster_middleware:start_link(middleware()),
  State = gen_server:call(Pid, get_state),
  ?assertEqual(State, middleware()).

leave_test() ->
  {_, Resp} = rooster_middleware:leave({1, null}, [foo]),
  ?assertEqual(Resp, 2).

enter_test() ->
  {Req, _} = rooster_middleware:enter({1, null}, [bar]),
  ?assertEqual(Req, 3).
