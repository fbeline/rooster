-module(rooster_middleware).
-include_lib("rooster.hrl").

-export([match/4]).

%% filter matched middlewares and execute it
%%
-spec match(atom(), request(), [middleware()], any()) -> any().

match(_, _, [], Resp) -> Resp;
match(Type, Req, [{Module, Type, Regex, Function}|T], Resp) ->
    Match = re:run(Req#request.path, Regex),
    if Match =/= nomatch ->
           NewResp = apply(Module, Function, [Req, Resp]),
           match(Type, Req, T, NewResp);
       true ->
           match(Type, Req, T, Resp)
    end;

match(Type, Req, [_|T], Resp) ->
    match(Type, Req, T, Resp).
