-module(rooster_web).

-export([start/1, stop/0, loop/3]).

%% @doc initializer function responsible for start mochiweb server
%%
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun(Req) ->
           #{routes := Routes} = gen_server:call(rooster_state, get_state),
           ?MODULE:loop(Req, DocRoot, Routes)
         end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

loop(Req, _DocRoot, Routes) ->
  try
    Response = analyze_request(create_request(Req), Routes),
    Req:respond(Response)
  catch
    Type:What ->
      log_error(Type, What),
      Req:respond({500, [{"Content-Type", "application/json"}], request_fail_msg()})
  end.

create_request(Req) ->
  "/" ++ Path = Req:get(path),
  #{path          => Path,
    method        => Req:get(method),
    headers       => Req:get(headers),
    body          => rooster_json:decode(Req:recv_body()),
    qs            => Req:parse_qs(),
    cookies       => Req:parse_cookie(),
    pathParams    => [],
    authorization => Req:get_header_value('Authorization')}.

analyze_request(Req, Routes) ->
  {ok, Pid} = rooster_srv:start(Routes),
  rooster_srv:analyze_route(Pid, Req).

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
