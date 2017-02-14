-module(rooster_sup).

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
init({Port, Routes}) ->
    Web = web_specs(rooster_web, Port),
    _Rooster_specs = register_rooster(Routes),
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, [Web]}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, rooster_deps:local_path(["priv", "www"])}],
    {Mod, {Mod, start, [WebConfig]}, permanent, 5000, worker, dynamic}.

register_rooster(Routes) ->
    {rooster_srv, {rooster_srv, start, [[{routes, Routes}, {middlewares, [middleware_example]}]]},
     permanent,
     5000,
     worker,
     [rooster_srv]}.
