-module(rooster_dispatcher).
-include_lib("rooster.hrl").

-export([match_route/2, compare_route_tokens/3, parse_route/1, call_route_function/3]).


%% @doc get matched route
%%
-spec match_route(request(), list(route())) -> response().

match_route(#{path := Path, method := Method} = Req, Routes) ->
  match_route(Path, Method, Req, Routes).

match_route(_, _, _Req, []) -> {404, []};
match_route(RequestedRoute, Method, Req, [{Method, Route, Function} | T]) ->
  RouteTokens = parse_route(Route),
  RequestedRouteTokens = parse_route(RequestedRoute),
  {IsValid, PathParams} = compare_route_tokens(RouteTokens, RequestedRouteTokens, []),
  if IsValid =:= true ->
    error_logger:info_msg("Route matched: " ++ lists:concat([Method, ":", RequestedRoute]), []),
    call_route_function(Req, Function, PathParams);
    true ->
      match_route(RequestedRoute, Method, Req, T)
  end;

match_route(Route, M1, Req, [_ | T]) -> match_route(Route, M1, Req, T).

%% @doc Call route function
%%
-spec call_route_function(request(), {module(), function()}, list()) -> response().

call_route_function(Req, Function, PathParams) ->
  NewRequest = create_request(Req, PathParams),
  apply(Function, [NewRequest, #{}]).

%% @doc create request with path params
%%
-spec create_request(request(), any()) -> request().

create_request(Req, PathParams) ->
  Req#{pathParams := PathParams}.

%% @doc Parse a route in tokens
%%
-spec parse_route(string()) -> [nonempty_string()].

parse_route(Route) ->
  [RouteWithoutQueryParams | _] = string:tokens(Route, "?"),
  RouteTokens = string:tokens(RouteWithoutQueryParams, "/"),
  RouteTokens.

%% @doc Compare routes and extract path parameters
%%
-spec compare_route_tokens(maybe_improper_list(), maybe_improper_list(), _) -> {false, {}}|{true, _}.

compare_route_tokens([], [], Acc) -> {true, Acc};
compare_route_tokens([], [_H | _T], _) -> {false, {}};
compare_route_tokens([_H | _T], [], _) -> {false, {}};
compare_route_tokens([H1 | T1], [H2 | T2], Acc) ->
  IsPathParam = string:str(H1, ":") =:= 1,
  SameToken = H1 =:= H2,
  if IsPathParam ->
    compare_route_tokens(T1, T2, Acc ++ [{H1, H2}]);
    SameToken ->
      compare_route_tokens(T1, T2, Acc);
    true ->
      {false, {}}
  end.
