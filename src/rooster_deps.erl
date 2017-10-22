-module(rooster_deps).

-export([ensure/0, ensure/1]).
-export([get_base_dir/0, get_base_dir/1]).
-export([local_path/1, local_path/2]).
-export([deps_on_path/0, new_siblings/1]).

deps_on_path() ->
  F = fun(X, Acc) ->
    ProjDir = filename:dirname(X),
    case {filename:basename(X),
      filename:basename(filename:dirname(ProjDir))} of
      {"ebin", "deps"} ->
        [filename:basename(ProjDir) | Acc];
      _ ->
        Acc
    end
      end,
  ordsets:from_list(lists:foldl(F, [], code:get_path())).

new_siblings(Module) ->
  Existing = deps_on_path(),
  SiblingEbin = filelib:wildcard(local_path(["deps", "*", "ebin"], Module)),
  Siblings = [filename:dirname(X) || X <- SiblingEbin,
    ordsets:is_element(
      filename:basename(filename:dirname(X)),
      Existing) =:= false],
  lists:filter(fun filelib:is_dir/1,
    lists:append([[filename:join([X, "ebin"]),
      filename:join([X, "include"])] ||
      X <- Siblings])).

ensure(Module) ->
  code:add_paths(new_siblings(Module)),
  code:clash(),
  ok.

ensure() ->
  ensure(?MODULE).

get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

get_base_dir() ->
  get_base_dir(?MODULE).

local_path(Components, Module) ->
  filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
  local_path(Components, ?MODULE).