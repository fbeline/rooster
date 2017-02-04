-module(rooster_json).

-export([encode/1, decode/1]).


encode(Term) ->
	jiffy:encode(Term).

decode(Term) ->
	jiffy:decode(Term).
