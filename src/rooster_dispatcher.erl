-module(rooster_dispatcher).

-export([match_route/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

match_route(#{path := Path, method := Method} = Req) ->
  match_route(Path, Method, Req, rooster_route:get()).

match_route(_, _, _, []) -> {404, #{message => <<"Not found">>}};
match_route(RequestedRoute, Method, Req, [{Method, Route, Fn, Middleware} | T]) ->
  {ContextTokens, RequestTokens} = {parse_route(Route), parse_route(RequestedRoute)},
  case path_params(ContextTokens, RequestTokens) of
    {ok, Params} ->  handle_request(Req#{params := Params}, Fn, Middleware);
    {not_match, _} -> match_route(RequestedRoute, Method, Req, T)
  end.

handle_request(Request, Fn, Middleware) ->
  Res = rooster_middleware:enter(Request, Middleware),
  call_route(Res, Fn, Middleware).

call_route({break, Resp}, _, _) -> rooster_adapter:route_response(Resp);
call_route({ok, Req}, Fn, Middleware)->
  RouteResponse = rooster_adapter:route_response(Fn(Req)),
  {_, Response} = rooster_middleware:leave(RouteResponse, Middleware),
  Response.

parse_route(Route) ->
  [RouteWithoutQueryParams | _] = string:tokens(Route, "?"),
  RouteTokens = string:tokens(RouteWithoutQueryParams, "/"),
  RouteTokens.

path_params(ContextTokens, RequestedTokens) ->
  if erlang:length(ContextTokens) =:= erlang:length(RequestedTokens) -> path_params(ContextTokens, RequestedTokens, #{});
    true -> {not_match, #{}}
  end.

path_params([], [], Acc) -> {ok, Acc};
path_params([Token|T1], [Token|T2], Acc) -> path_params(T1, T2, Acc);
path_params([Context|T1], [Requested|T2], Acc) ->
  IsPathParam = string:str(Context, ":") =:= 1,
  if IsPathParam -> path_params(T1, T2, new_param(Acc, {Context, Requested}));
     true -> {not_match, #{}}
  end.

new_param(Params, {Key, Value}) ->
  Params#{erlang:list_to_atom(string:slice(Key, 1)) => Value}.
