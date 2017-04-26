-module(rooster_web).
-include_lib("rooster.hrl").

-export([start/1, stop/0, loop/5]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   {Routes, Middlewares, RespHeaders} = gen_server:call(rooster_config, get_state),
                   ?MODULE:loop(Req, DocRoot, Routes, Middlewares, RespHeaders) 
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot, Routes, Middlewares, RespHeaders) ->
    try
        Response = rooster:analyze_request(create_request(Req), Routes, Middlewares, RespHeaders),
        Req:respond(Response)
    catch
        Type:What ->
            log_error(Type, What),
            Req:respond({500, [{"Content-Type", "application/json"}], request_fail_msg()})
    end.

create_request(Req) ->
    "/" ++ Path = Req:get(path),
    #{
       path => Path,
       method => Req:get(method),
       headers => Req:get(headers),
       body => rooster_json:decode(Req:recv_body()),
       qs => Req:parse_qs(),
       cookies => Req:parse_cookie(),
       pathParams => [],
       authorization => Req:get_header_value('Authorization')}.

request_fail_msg() ->
    rooster_json:encode(#{message => <<"request failed, sorry">>}).

log_error(Type, What) ->
    Report = ["web request failed",
              {type, Type}, {what, What},
              {trace, erlang:get_stacktrace()}],
    error_logger:error_report(Report).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
