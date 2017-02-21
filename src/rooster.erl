-module(rooster).
-include_lib("rooster.hrl").

-export([start/0, stop/0, analyze_request/3, start_server/3]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @doc Start the greeting server.
%%
-spec start() -> ok.

start() ->
    rooster_deps:ensure(),
    ensure_started(crypto),
    application:start(rooster).


%% @doc Stop the greeting server.
%%
-spec stop() -> ok.

stop() ->
    application:stop(rooster).


%% @doc analyze request
%%
-spec analyze_request(request(), [route()], [middleware()]) -> response().

analyze_request(Req, Routes, Middlewares) ->
    {ok, Pid} = rooster_srv:start({Routes, Middlewares}),
    gen_server:call(Pid, {analyze_route, Req}).

%% @doc start rooster server
%%
-spec start_server(integer(), [route()], [middleware()]) -> 'ignore' | {'error',_} | {'ok',pid()}.

start_server(Port, Routes, Middlewares) ->
    rooster_deps:ensure(),
    rooster_sup:start_link({Port, Routes, Middlewares}).
