-module(rooster_middleware).
-include_lib("rooster.hrl").

-export([match/4]).

%% filter matched middlewares and execute it
%%
-spec match(atom(), request(), [middleware()], any()) -> any().

match(_, _, [], Resp) -> {next, Resp};
match(Type, Req, [{Module, Type, Regex, Function}|T], Resp) ->
    Match = re:run(Req#request.path, Regex),
    if Match =/= nomatch ->
           MiddlewareResp = apply(Module, Function, [Req, Resp]),
           case MiddlewareResp of
               {next, NewResp} ->
                   match(Type, Req, T, NewResp);
               {_, NewResp} ->
                   {break, NewResp}
           end;
       true ->
           match(Type, Req, T, Resp)
    end;

match(Type, Req, [_|T], Resp) ->
    match(Type, Req, T, Resp).
