-module(rooster).
-include_lib("rooster.hrl").

-export([start/0, stop/0, analyze_request/4, start_server/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @doc Start the rooster server.
%%
-spec start() -> ok.

start() ->
    rooster_deps:ensure(),
    ensure_started(crypto),
    application:start(rooster).


%% @doc Stop the rooster server.
%%
-spec stop() -> ok.

stop() ->
    application:stop(rooster).


%% @doc analyze request
%%
-spec analyze_request(request(), [route()], [middleware()], list()) -> response().

analyze_request(Req, Routes, Middlewares, Cors) ->
    {ok, Pid} = rooster_srv:start({Routes, Middlewares, Cors}),
    gen_server:call(Pid, {analyze_route, Req}).

%% @doc start rooster server
%%
-spec start_server(config()) -> 'ignore' | {'error',_} | {'ok',pid()}.

start_server(State) ->
    rooster_deps:ensure(),
    ensure_started(crypto),
    rooster_sup:start_link(State).
