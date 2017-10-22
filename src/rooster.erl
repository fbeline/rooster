-module(rooster).

-export([stop/0, start/2]).

-type route() :: {atom(), string(), any(), list(map())}.
-type config() :: #{ip          => {integer(), integer(), integer(), integer()},
                    port        => integer(),
                    static_path => list(string()),
                    ssl         => any(),
                    ssl_opts    => any()}.

-type state() :: #{routes => list(route()),
                   middleware => list(map())}.

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

-spec start(config(), state()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start(SrvConf, State) ->
  ensure_started(crypto),
  rooster_holder:start([SrvConf, State]),
  application:start(rooster).

-spec stop() -> ok.
stop() ->
  application:stop(rooster).