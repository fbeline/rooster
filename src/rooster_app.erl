-module(rooster_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for rooster.
start(_Type, _StartArgs) ->
    rooster_deps:ensure(),
    rooster_sup:start_link({8080, [route_example], [middleware_example]}).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for rooster.
stop(_State) ->
    ok.

