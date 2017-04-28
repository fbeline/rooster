-module(rooster_util).
-include_lib("rooster.hrl").

-export([forge_config/1, forge_state/1]).

%% @doc create default behaviour of config
%%
-spec forge_config(config()) -> config().

forge_config(Conf) ->
    Default = #{ip => {0, 0, 0, 0},
        port => 8080,
        static_path => ["priv", "www"],
        ssl => {ssl, false},
        ssl_opts => {ssl_opts, []}},
    maps:merge(Default, Conf).


%% @doc create default behaviour of state
%%
-spec forge_state(state()) -> state().

forge_state(State) ->
    Default = #{routes => [],
        middleware => [],
        resp_headers => [],
        version => "0.0.0"},
    maps:merge(Default, State).
