-module(rooster_adapter).
-include_lib("rooster.hrl").

-export([config/1, state/1, middleware/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

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
              middleware   => [],
              resp_headers => []},
  flatt_routes(maps:merge(Default, State)).

flatt_routes(#{routes := Routes} = State)->
  State#{routes := lists:flatten(Routes)}.

middleware(Middleware) ->
  Default = #{name  => default,
              enter => fun(ReqResp) -> ReqResp end,
              leave => fun(ReqResp) -> ReqResp end},
  maps:merge(Default, Middleware).
