-module(rooster_json).

-export([encode/1, decode/1]).

-spec encode(any()) -> string().
encode(Term) ->
  jsx:encode(Term).

-spec decode(string()) -> any().
decode(Term) ->
  Data = case Term of
           undefined -> erlang:list_to_binary("{}");
           Bin -> Bin
         end,
  jsx:decode(Data).