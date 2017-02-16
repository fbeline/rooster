-module(rooster_dispatcher).
-include_lib("rooster.hrl").

-export([match_route/3, compare_route_tokens/3, parse_route/1, call_route_function/4]).


%% @doc get matched route
%%
-spec match_route(request(), list(route()), list(middleware())) -> response().

match_route(Req, Routes, Middlewares) ->
    match_route(Req#request.path, Req#request.method, Req, Routes, Middlewares).


match_route(_,_,_Req, [],_) -> {404, []};
match_route(RequestedRoute, Method, Req, [{Module, Method, Route, Function}|T], Middlewares) ->
    RouteTokens = parse_route(Route),
    RequestedRouteTokens = parse_route(RequestedRoute),
    {IsValid, PathParams} = compare_route_tokens(RouteTokens, RequestedRouteTokens, []),
    if IsValid =:= true ->
           error_logger:info_msg("Route matched: " ++ lists:concat([Method,":",RequestedRoute]), []), 
           call_route_function(Req, {Module, Function}, PathParams, Middlewares);    
       true ->
           match_route(Route, Method, Req, T, Middlewares)
    end;

match_route(Route, M1, Req, [_|T], Middlewares) -> match_route(Route, M1, Req, T, Middlewares).

%% @doc Call route function
%%
-spec call_route_function(request(), {module(), atom()}, list(), list(middleware())) -> response().

call_route_function(Req, {Module, Function}, PathParams, Middlewares) ->
    NewRequest = #request{path=Req#request.path,
                          method=Req#request.method,
                          headers=Req#request.headers,
                          body= decode_data_from_request(Req),
                          qs=Req#request.qs,
                          cookies=Req#request.cookies,
                          pathParams=PathParams,
                          authorization=Req#request.authorization},

    RespBefore = rooster_middleware:match('BEFORE', NewRequest, Middlewares, undefined),
    Resp = apply(Module, Function, [NewRequest, RespBefore]),
    rooster_middleware:match('AFTER', NewRequest, Middlewares, Resp).

%% @doc Get payload and parse to erlang struct
%%
-spec decode_data_from_request(request()) -> any().

decode_data_from_request(Req) ->
    RecvBody = Req#request.body,
    Data = case RecvBody of
               undefined -> erlang:list_to_binary("{}");
               Bin -> Bin
           end,
    rooster_json:decode(Data).

%% @doc Parse a route in tokens
%%
-spec parse_route(string()) -> [nonempty_string()].

parse_route(Route) ->
    [RouteWithoutQueryParams| _] = string:tokens(Route, "?"),
    RouteTokens = string:tokens(RouteWithoutQueryParams, "/"),
    RouteTokens.

%% @doc Compare routes and extract path parameters
%%
-spec compare_route_tokens(maybe_improper_list(), maybe_improper_list(), _) -> {false, {}}|{true, _}.

compare_route_tokens([], [], Acc) -> {true, Acc};
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
