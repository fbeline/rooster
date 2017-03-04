-module(rooster_sup).
-include_lib("rooster.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/1, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link(State) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, State).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(State) ->
    Web = web_specs(rooster_web, State),
    RoosterConfig = register_rooster(),
    Strategy = {one_for_one, 10, 10},
    io:format("~nrooster listening on port ~p~n", [State#config.port]),
    {ok, {Strategy, [RoosterConfig, Web]}}.

%% @doc generate mochiweb specs to be used by supervisor
%%
web_specs(Mod, State) ->
    io:format("~n~p~n", [State]),
    WebConfig = [{ip, State#config.ip},
                 {port, State#config.port},
                 {docroot, rooster_deps:local_path(State#config.static_path, app)},
                 State#config.ssl,
                 State#config.ssl_opts
                ],
    {Mod, {Mod, start, [WebConfig]}, permanent, 5000, worker, dynamic}.

%% @doc generate rooster_config specs to be used by supervisor
%%
register_rooster() ->
    {rooster_config, {rooster_config, start, []},
     permanent,
     5000,
     worker,
     [rooster_config]}.
