-module(rooster_json).

-export([encode/1, decode/1]).

%% @doc erlang to json
%%
encode(Term) ->
	jiffy:encode(Term).

%% @doc json to erlang
%%
decode(Term) ->
	jiffy:decode(Term).
