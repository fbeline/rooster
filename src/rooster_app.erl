-module(rooster_app).

-behaviour(application).
-export([start/2, stop/1]).


%% @doc application start callback for rooster.
%%
start(_Type, _StartArgs) ->
    Options = gen_server:call(rooster_holder, get_state),
    rooster_holder:stop(),
    rooster_sup:start_link(Options).

%% @doc application stop callback for rooster.
%%
stop(_State) ->
    ok.
