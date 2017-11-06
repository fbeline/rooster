-module(rooster_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  Options = rooster_state:get(),
  rooster_sup:start_link(Options).

stop(_State) ->
  ok.