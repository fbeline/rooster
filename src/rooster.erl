-module(rooster).
-include_lib("rooster.hrl").

-export([stop/0, analyze_request/4, start/1]).

%%  @doc ensure that application is started
%%
-spec ensure_started(atom()) -> ok.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

%% @doc start rooster server
%%
-spec start(config()) -> 'ignore' | {'error', _} | {'ok', pid()}.

start(State) ->
  ensure_started(crypto),
  rooster_holder:start(State),
  application:start(rooster).

%% @doc Stop the rooster server.
%%
-spec stop() -> ok.

stop() ->
  application:stop(rooster).


%% @doc analyze request
%%
-spec analyze_request(request(), [route()], [middleware()], list()) -> response().

analyze_request(Req, Routes, Middleware, Cors) ->
  {ok, Pid} = rooster_srv:start({Routes, Middleware, Cors}),
  rooster_srv:analyze_route(Pid, Req).

