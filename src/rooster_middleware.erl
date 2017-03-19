-module(rooster_middleware).
-include_lib("rooster.hrl").

-export([match/4]).

%% @doc filter matched middlewares and execute it
%%
-spec match(atom(), request(), [middleware()], any()) -> any().

match(_, _, [], Resp) -> {next, Resp};
match(Type, Req, Middleware = [{_, Type, Regex, _}|_], Resp) ->
    Match = re:run(Req#request.path, Regex),
    middleware_regex_match(Match, Req, Middleware, Resp);

match(Type, Req, [_|T], Resp) ->
    match(Type, Req, T, Resp).

%% @doc execute middleware if regex matches
%%
-spec middleware_regex_match(atom(), request(), [middleware()], any()) -> any().

middleware_regex_match(nomatch, Req, [{_, Type, _, _}|T], Resp) ->
    match(Type, Req, T, Resp);

middleware_regex_match(_, Req, [{Module, Type, _, Function}|T], Resp) ->
    {Instruction, NewResp} = apply(Module, Function, [Req, Resp]),
    execute_next(Instruction, {Type, Req, T}, NewResp).

%% @doc execute next middleware or end the cycle
%%
-spec execute_next(atom(), {atom(), request(), [middleware()]}, any()) -> any().

execute_next(next, {Type, Req, Tail}, Resp) ->
    match(Type, Req, Tail, Resp);

execute_next(_, _, Resp) ->
    {break, Resp}.
