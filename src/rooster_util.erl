-module(rooster_util).
-export([forge_config/1]).

forge_config(Conf) ->
    Default = #{ip => {0,0,0,0},
                port => 8080,
                static_path => ["priv", "www"],
                ssl => {ssl, false},
                ssl_opts => {ssl_opts, []}},
    maps:merge(Conf, Default).