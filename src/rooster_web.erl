-module(rooster_web).
-include_lib("rooster.hrl").

-export([start/1, stop/0, loop/2]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    {Routes, Middlewares} = gen_server:call(rooster_config_srv, get_state),
    "/" ++ Path = Req:get(path),
    Request = #request{path=Path,
                       method=Req:get(method),
                       headers=Req:get(headers),
                       body=Req:recv_body(),
                       qs=Req:parse_qs(),
                       cookies=Req:parse_cookie(),
                       pathParams=[]},
    try
        Response = rooster:analyze_request(Request, Routes, Middlewares),
        Req:respond(Response)
    catch
        _Exception:_Reason ->
            rooster_logger:err([Path, " -- web request failed --"]),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
