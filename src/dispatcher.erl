-module(dispatcher).
-include_lib("rooster.hrl").

-export([match_route/2, compare_route_tokens/3, parse_route/1]).


%% @doc get matched route and call respective function
%%
match_route(Req, Routes) ->
    match_route(Req#request.path, Req#request.method, Req, Routes).

match_route(_,_,_Req, []) -> {404, []};
match_route(RequestedRoute, Method, Req, [{Module, Method, Route, Function}|T]) ->
    RouteTokens = parse_route(Route),
    RequestedRouteTokens = parse_route(RequestedRoute),
    {IsValid, PathParams} = compare_route_tokens(RouteTokens, RequestedRouteTokens, []),
    if IsValid =:= true ->
           io:format("~n~p: ~p", [Method, RequestedRoute]),
           case Method of
               'GET' ->
                   Params = [Req] ++ [PathParams],
                   apply(Module, Function, Params);
               _ -> 
                   Body = decode_data_from_request(Req),
                   apply(Module, Function, [Req] ++ [[Body] ++ [PathParams]])
           end;
       true ->
           match_route(Route, Method, Req, T)
    end;

match_route(Route, M1, Req, [_|T]) -> match_route(Route, M1, Req, T).

%% @doc Get payload and parse to erlang struct
%%
decode_data_from_request(Req) ->
    RecvBody = Req#request.body,
    Data = case RecvBody of
               undefined -> erlang:list_to_binary("{}");
               Bin -> Bin
           end,
    Struct = rooster_json:decode(Data),
    {body, Struct}.

%% @doc Parse a route in tokens
%%
parse_route(Route) ->
    [RouteWithoutQueryParams| _] = string:tokens(Route, "?"),
    RouteTokens = string:tokens(RouteWithoutQueryParams, "/"),
    RouteTokens.

%% @doc Compare routes and extract path parameters
%%
compare_route_tokens([], [], Acc) -> {true, {path_params, Acc}};
compare_route_tokens([], [_H|_T], _) -> {false, {}};
compare_route_tokens([_H|_T], [], _) -> {false, {}};
compare_route_tokens([H1|T1],[H2|T2], Acc) -> 
    IsPathParam = string:str(H1, ":") =:= 1,
    SameToken = H1 =:= H2,
    if IsPathParam ->
           compare_route_tokens(T1, T2, Acc ++ [{H1, H2}]);
       SameToken ->
           compare_route_tokens(T1, T2, Acc);
       true ->
           {false, {}}
    end.
