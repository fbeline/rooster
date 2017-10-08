-module(rooster).
-include_lib("rooster.hrl").

-export([stop/0, start/2]).

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
-spec start(config(), state()) -> 'ignore' | {'error', _} | {'ok', pid()}.

start(SrvConf, State) ->
  ensure_started(crypto),
  rooster_holder:start([SrvConf, State]),
  application:start(rooster).

%% @doc Stop the rooster server.
%%
-spec stop() -> ok.

stop() ->
  application:stop(rooster).


