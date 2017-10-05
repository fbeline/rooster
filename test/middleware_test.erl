-module(middleware_test).

-include_lib("../include/rooster.hrl").
-include_lib("eunit/include/eunit.hrl").


middleware() ->
  [#{name => foo, leave => fun({_, Resp}) -> {null, Resp * 2} end},
   #{name => bar, enter => fun({Req, _}) -> {Req * 3, null} end}].

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
