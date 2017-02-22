-module(rooster_config_srv).

-behaviour(gen_server).
-include_lib("rooster.hrl").

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start({Routes, Middlewares, Cors}) ->
    ParsedState = parse_state({Routes, Middlewares}),
    ParsedCors = parse_cors(Cors),
    FinalState = erlang:insert_element(3, ParsedState, ParsedCors),
    gen_server:start_link({local, ?MODULE}, ?MODULE, FinalState, []).

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
    {noreply, parse_state(State)}.

handle_call(get_state, _From, State) ->
    {Routes, Middlewares, _ } = State,
    {reply, {Routes, Middlewares}, State};

handle_call(get_cors, _From, State) ->
    {_, _, Cors} = State,
    {reply, Cors, State}.

parse_state({Routes, Middlewares}) ->
    NewRoutes = add_module(Routes, []),
    NewMiddlewares = add_module(Middlewares, []),
    {NewRoutes, NewMiddlewares}. 


%% @doc generate headers
%%
-spec parse_cors(string()) -> [{string(), string()}].

parse_cors(Cors) -> 
    [{"Access-Control-Request-Method", "*"}, {"Access-Control-Request-Headers", "*"}, {"Access-Control-Allow-Origin", Cors}].

%% @doc add module to all routes tuples
%%
-spec add_module(list(route()), list()) -> list(route()).

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
