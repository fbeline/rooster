-module(middleware_test).

-include_lib("../include/rooster.hrl").
-include_lib("eunit/include/eunit.hrl").


middleware() ->
  [#{name => foo, leave => fun({_, Resp}) -> {null, Resp * 2} end},
   #{name => bar, enter => fun({Req, _}) -> {Req * 3, null} end},
   #{name => baz, enter => fun({Req, Resp}) -> {Req * 2, Resp} end, leave => fun({Req, Resp}) -> {Req, Resp * 2} end}].

server_start_test() ->
  Middleware = middleware(),
  {ok, Pid} = rooster_middleware:start_link(Middleware),
  State = gen_server:call(Pid, get_state),
  Expected = lists:map(fun rooster_adapter:middleware/1, Middleware),
  ?assertEqual(Expected, State).

leave_test() ->
  rooster_middleware:start_link(middleware()),
  {_, Resp} = rooster_middleware:leave({null, 1}, [foo]),
  ?assertEqual(2, Resp).

enter_test() ->
  rooster_middleware:start_link(middleware()),
  {Req, _} = rooster_middleware:enter({1, null}, [bar]),
  ?assertEqual(3, Req).

multiple_middleware_enter_test() ->
  rooster_middleware:start_link(middleware()),
  {Req, _} = rooster_middleware:enter({1, null}, [baz, bar]),
  ?assertEqual(6, Req).

multiple_middleware_leave_test() ->
  rooster_middleware:start_link(middleware()),
  {_, Resp} = rooster_middleware:leave({null, 1}, [baz, foo]),
  ?assertEqual(4, Resp).
