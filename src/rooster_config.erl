-module(rooster_config).

-behaviour(gen_server).
-include_lib("rooster.hrl").

-export([start/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init(Env) ->
    {ok, Env}.

terminate(_Reason, _Env) ->
    ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(stop, Env) ->
    {stop, normal, Env};

handle_cast({set_state, State}, _State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    NewState = app:exports(),
    get_state(State, NewState).

get_state(State = #state{version=Version}, #state{version=Version}) ->
    Resp = {State#state.routes, State#state.middlewares, State#state.resp_headers},
    {reply, Resp, State};

get_state(_, NewState) ->
    {Routes, Middlewares} = parse_state({NewState#state.routes, NewState#state.middlewares}),
    FinalState = #state{routes=Routes,
                        middlewares=Middlewares, 
                        resp_headers=NewState#state.resp_headers,
                        version=NewState#state.version},
    Resp = {Routes, Middlewares, NewState#state.resp_headers},
    {reply, Resp, FinalState}.

%% @doc configure final routes/middlewares state
%%
-spec parse_state({list(), list()}) -> {list(route()), list(middleware())}.

parse_state({Routes, Middlewares}) ->
    NewRoutes = add_module(Routes, []),
    NewMiddlewares = add_module(Middlewares, []),
    {NewRoutes, NewMiddlewares}. 

%% @doc add module to all routes and middlewares 
%%
-spec add_module(list(), list()) -> list(route()).

add_module([], Acc) -> Acc;
add_module([M|T], Acc) ->
    NewAcc = Acc ++ apply_module(apply(M, exports, []), M, []),
    add_module(T, NewAcc).

%% @doc add module to routes tuples
%%
-spec apply_module(list(route()), module(), list()) -> list(route()).

apply_module([], _, Acc) -> Acc;
apply_module([H|T], Module, Acc) ->
    NewEntry = erlang:insert_element(1, H, Module),
    apply_module(T, Module, [NewEntry|Acc]).
