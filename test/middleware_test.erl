-module(middleware_test).

-include_lib("eunit/include/eunit.hrl").

middleware() ->
  [#{name => foo, leave => fun(Resp) -> Resp * 2 end},
   #{name => bar, enter => fun(Req) -> Req * 3 end},
   #{name => baz, enter => fun(Req) -> Req * 2 end, leave => fun(Resp) -> Resp * 2 end}].

server_start_test() ->
  Middleware = middleware(),
  {ok, Pid} = rooster_middleware:start_link(Middleware),
  State = gen_server:call(Pid, get_state),
  Expected = lists:map(fun rooster_adapter:middleware/1, Middleware),
  ?assertEqual(Expected, State).

leave_test() ->
  rooster_middleware:start_link(middleware()),
  Resp = rooster_middleware:leave(1, [foo]),
  ?assertEqual(2, Resp).

enter_test() ->
  rooster_middleware:start_link(middleware()),
  Req = rooster_middleware:enter(1, [bar]),
  ?assertEqual(3, Req).

multiple_middleware_enter_test() ->
  rooster_middleware:start_link(middleware()),
  Req = rooster_middleware:enter(1, [baz, bar]),
  ?assertEqual(6, Req).

multiple_middleware_leave_test() ->
  rooster_middleware:start_link(middleware()),
  Resp = rooster_middleware:leave(1, [baz, foo]),
  ?assertEqual(4, Resp).
