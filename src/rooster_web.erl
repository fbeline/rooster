-module(rooster_web).

-export([start/1, stop/0, loop/3]).

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun(Req) ->
           #{routes := Routes} = gen_server:call(rooster_state, get_state),
           ?MODULE:loop(Req, DocRoot, Routes)
         end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

loop(Req, _DocRoot, Routes) ->
  try
    Response = rooster_dispatcher:match_route(rooster_adapter:request(Req), Routes),
    Req:respond(rooster_adapter:server_response(Response))
  catch
    Type:What ->
      log_error(Type, What),
      Req:respond({500, [{"Content-Type", "application/json"}], request_fail_msg()})
  end.

request_fail_msg() ->
  rooster_json:encode(#{message => <<"Internal server error">>}).

log_error(Type, What) ->
  Report = ["web request failed",
            {type, Type},
            {what, What},
            {trace, erlang:get_stacktrace()}],
  error_logger:error_report(Report).

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

stop() ->
  mochiweb_http:stop(?MODULE).
