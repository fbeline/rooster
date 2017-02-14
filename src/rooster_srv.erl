-module(rooster_srv).
-include_lib("rooster.hrl").
-behaviour(gen_server).

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start(State) ->
    Routes = add_module(proplists:get_value(routes, State), []),
    Middlewares = add_module(proplists:get_value(middlewares, State), []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Routes, Middlewares], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init(Env) ->
    io:format("Starting...~n"),
    {ok, Env}.

terminate(_Reason, _Env) ->
    io:format("Terminating...~n").

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(stop, Env) ->
    {stop, normal, Env}.

%% @doc check requested Path
%%
handle_call({analyze_route, Req}, _From, [Routes, Middlewares]) ->
    {Status, Response} = rooster_dispatcher:match_route(Req, Routes, Middlewares),
    case Status of
        404 ->
            {reply, {404, [{"Content-type", "text/plain"}], "Requested endpoint not found."}, [Routes, Middlewares]};
        _ ->
            {reply, {Status, [{"Content-type", "application/json"}], rooster_json:encode(Response)}, [Routes, Middlewares]}
    end.

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
