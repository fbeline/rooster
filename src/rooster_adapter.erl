-module(rooster_adapter).
-include_lib("rooster.hrl").

-export([config/1, state/1]).

%% @doc create default behaviour of config
%%
-spec config(config()) -> config().

config(Conf) ->
  Default = #{ip          => {0, 0, 0, 0},
              port        => 8080,
              static_path => ["priv", "www"],
              ssl         => {ssl, false},
              ssl_opts    => {ssl_opts, []}},
  maps:merge(Default, Conf).


%% @doc create default behaviour of state
%%
-spec state(state()) -> state().

state(State) ->
  Default = #{routes       => [],
              resp_headers => [],
              version      => "0.0.0"},
  maps:merge(Default, State).
