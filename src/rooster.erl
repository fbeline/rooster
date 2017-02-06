-module(rooster).
-export([start/0, stop/0, analyze_request/1, start_server/2]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the greeting server.
start() ->
    rooster_deps:ensure(),
    ensure_started(crypto),
    application:start(rooster).


%% @spec stop() -> ok
%% @doc Stop the greeting server.
stop() ->
    application:stop(rooster).


%% @doc analyze request
%%
analyze_request(Req) ->
    gen_server:call(rooster_srv, {analyze_route, Req}).

%% @doc start rooster server
%%
start_server(Port, Routes) ->
    rooster_deps:ensure(),
    rooster_sup:start_link({Port, Routes}).
