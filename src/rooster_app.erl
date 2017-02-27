-module(rooster_app).

-include_lib("rooster.hrl").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for rooster.
start(_Type, _StartArgs) ->
    Options = #config{port=8080,
                      routes=[route_example],
                      resp_headers=[{"access-control-allow-methods", "*"},
                                    {"access-control-allow-headers", "*"},
                                    {"access-control-allow-origin", "*"}]},
    rooster_sup:start_link(Options).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for rooster.
stop(_State) ->
    ok.

