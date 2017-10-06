-module(rooster_web).

-export([start/1, stop/0, loop/4]).

%% @doc initializer function responsible for start mochiweb server
%%
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun(Req) ->
           #{routes := Routes, resp_headers := Rh} = gen_server:call(rooster_config, get_state),
           ?MODULE:loop(Req, DocRoot, Routes, Rh)
         end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).


%% @doc loop is executed once per request
%%
loop(Req, _DocRoot, Routes, RespHeaders) ->
  try
    Response = rooster:analyze_request(create_request(Req), Routes, RespHeaders),
    Req:respond(Response)
  catch
    Type:What ->
      log_error(Type, What),
      Req:respond({500, [{"Content-Type", "application/json"}], request_fail_msg()})
  end.

%% @doc build request
%%
create_request(Req) ->
  "/" ++ Path = Req:get(path),
  #{
    path          => Path,
    method        => Req:get(method),
    headers       => Req:get(headers),
    body          => rooster_json:decode(Req:recv_body()),
    qs            => Req:parse_qs(),
    cookies       => Req:parse_cookie(),
    pathParams    => [],
    authorization => Req:get_header_value('Authorization')}.

%% @doc request fail return
%%
request_fail_msg() ->
  rooster_json:encode(#{message => <<"Internal server error">>}).

%% @doc log stack trace if exception occurs
%%
log_error(Type, What) ->
  Report = ["web request failed",
            {type, Type},
            {what, What},
            {trace, erlang:get_stacktrace()}],
  error_logger:error_report(Report).

%% @doc get option
%%
get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% @doc stop mochiweb server
%%
stop() ->
  mochiweb_http:stop(?MODULE).
