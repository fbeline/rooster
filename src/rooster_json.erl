-module(rooster_json).

-export([encode/1, decode/1]).

%% erlang to json
%%
encode(Term) ->
	jiffy:encode(Term).

%% json to erlang
%%
decode(Term) ->
	jiffy:decode(Term).
