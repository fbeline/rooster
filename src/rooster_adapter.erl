-module(rooster_adapter).
-include_lib("rooster.hrl").

-export([config/1, state/1, middleware/1, route_response/1, server_response/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

config(Conf) ->
  Default = #{ip          => {0, 0, 0, 0},
              port        => 8080,
              static_path => ["priv", "www"],
              ssl         => {ssl, false},
              ssl_opts    => {ssl_opts, []}},
  maps:merge(Default, Conf).

state(State) ->
  Default = #{routes       => [],
              middleware   => []},
  flatt_routes(maps:merge(Default, State)).

flatt_routes(#{routes := Routes} = State)->
  State#{routes := lists:flatten(Routes)}.

middleware(Middleware) ->
  Default = #{name  => default,
              enter => fun(ReqResp) -> ReqResp end,
              leave => fun(ReqResp) -> ReqResp end},
  maps:merge(Default, Middleware).

route_response({Status, Response, Headers}) ->
  {Status, Response, Headers};
route_response({Status, Response}) ->
  {Status, Response, []}.

server_response({Status, Response, Header}) ->
  Headers = [{"Content-type", "application/json"}] ++ Header,
  {Status, Headers, rooster_json:encode(Response)}.
