-module(rooster_json).

-export([encode/1, decode/1]).

%% @doc erlang to json
%%
-spec encode(any()) -> string().

encode(Term) ->
    jiffy:encode(Term).

%% @doc json to erlang
%%
-spec decode(string()) -> any().

decode(Term) ->
    jiffy:decode(Term).
