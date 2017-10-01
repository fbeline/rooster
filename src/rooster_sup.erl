-module(rooster_sup).
-behaviour(supervisor).

-export([start_link/1, upgrade/0]).
-export([init/1]).

%% @doc API for starting the supervisor.
%%
start_link(State) ->
  Conf = rooster_util:forge_config(State),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).

%% @doc Add processes if necessary.
%%
upgrade() ->
  {ok, {_, Specs}} = init([]),
  Old = sets:from_list(
    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun(Id, ok) ->
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id),
    ok
            end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @doc supervisor callback.
%%
init(#{port := Port} = Conf) ->
  Web = web_specs(rooster_web, Conf),
  RoosterConfig = rooster_config_specs(),
  rooster_deps:ensure(),
  io:format("~nrooster listening on port ~p~n", [Port]),
  {ok, {{one_for_one, 10, 10}, [RoosterConfig, Web]}}.

%% @doc generate mochiweb specs to be used by supervisor
%%
web_specs(Mod, #{ip := Ip, port := Port, static_path := Sp, ssl := Ssl, ssl_opts := Ssl_opts}) ->
  WebConfig = [{ip, Ip},
    {port, Port},
    {docroot, rooster_deps:local_path(Sp, ?MODULE)},
    Ssl, Ssl_opts],
  {Mod, {Mod, start, [WebConfig]}, permanent, 5000, worker, dynamic}.

%% @doc generate rooster_config specs to be used by supervisor
%%
rooster_config_specs() ->
  {rooster_config,
    {rooster_config, start, []},
    permanent, 5000, worker, []}.
