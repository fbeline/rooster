-module(rooster_config).

-behaviour(gen_server).
-include_lib("rooster.hrl").

-export([start/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ===============
%% Public API
%% ===============
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).


%% ===============
%% Server API
%% ===============
handle_cast(stop, Env) ->
  {stop, normal, Env};

handle_cast({set_state, State}, _State) ->
  {noreply, State}.

handle_call(get_state, _From, State) ->
  NewState = app:exports(),
  get_state(State, NewState).

%% ===============
%% Server callbacks
%% ===============
handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init(Env) ->
  {ok, Env}.

terminate(_Reason, _Env) ->
  ok.

%% ===============
%% Private API
%% ===============

%% @doc get available option for routes and middleware
%%
-spec get_state(state(), state()) -> {reply, state(), state()}.

get_state(State = #{version := Version}, #{version := Version}) ->
  {reply, State, State};

get_state(_, State) ->
  NewState = rooster_util:forge_state(State),
  Params = {maps:get(routes, NewState, []), maps:get(middleware, NewState, [])},
  {Routes, Middleware} = parse_state(Params),
  FinalState = maps:merge(NewState, #{routes => Routes, middleware => Middleware}),
  {reply, FinalState, FinalState}.

%% @doc configure final routes/middleware state
%%
-spec parse_state({list(), list()}) -> {list(route()), list(middleware())}.

parse_state({Routes, Middleware}) ->
  NewRoutes = add_module(Routes, []),
  NewMiddleware = add_module(Middleware, []),
  {NewRoutes, NewMiddleware}.

%% @doc add module to all routes and middleware
%%
-spec add_module(list(), list()) -> list(route()).

add_module([], Acc) -> Acc;
add_module([M | T], Acc) ->
  NewAcc = Acc ++ apply_module(apply(M, exports, []), M, []),
  add_module(T, NewAcc).

%% @doc add module to routes tuples
%%
-spec apply_module(list(route()), module(), list()) -> list(route()).

apply_module([], _, Acc) -> Acc;
apply_module([H | T], Module, Acc) ->
  NewEntry = erlang:insert_element(1, H, Module),
  apply_module(T, Module, [NewEntry | Acc]).