-module(rooster_sup).
-behaviour(supervisor).

-export([start_link/1, upgrade/0]).
-export([init/1]).

%% @doc API for starting the supervisor.
%%
start_link([SrvConf, State]) ->
  ISrvConf = rooster_adapter:config(SrvConf),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [ISrvConf, State]).

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
init([#{port := Port} = Conf, State]) ->
  Web = web_specs(rooster_web, Conf),
  State = state_specs(State),
  Middleware = middleware_specs(State),
  rooster_deps:ensure(),
  io:format("~nrooster listening on port ~p~n", [Port]),
  {ok, {{one_for_one, 10, 10}, [Middleware, State, Web]}}.

%% @doc generate mochiweb specs to be used by supervisor
%%
web_specs(Mod, #{ip := Ip, port := Port, static_path := Sp, ssl := Ssl, ssl_opts := Ssl_opts}) ->
  WebConfig = [{ip, Ip},
    {port, Port},
    {docroot, rooster_deps:local_path(Sp, ?MODULE)},
    Ssl, Ssl_opts],
  {Mod, {Mod, start, [WebConfig]}, permanent, 5000, worker, dynamic}.

%% @doc generate rooster_state specs to be used by supervisor
%%
state_specs(State) ->
  {rooster_state,
    {rooster_state, start_link, State},
    permanent, 5000, worker, []}.

%% @doc generate rooster_specs specs to be used by supervisor
%%
middleware_specs(#{middleware := Middleware}) ->
  {rooster_middleware,
    {rooster_middleware, start_link, Middleware},
    permanent, 5000, worker, []}.
