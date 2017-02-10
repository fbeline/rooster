-module(rooster_srv).
-include_lib("rooster.hrl").
-behaviour(gen_server).

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start(Routes) ->
    NewRoutes = load_routes(Routes, []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, NewRoutes, []).

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
handle_call({analyze_route, Req}, _From, Routes) ->
    {Status, Response} = rooster_dispatcher:match_route(Req, Routes),
    case Status of
        404 ->
            {reply, {404, [{"Content-type", "text/plain"}], "Requested endpoint not found."}, Routes};
        _ ->
            {reply, {Status, [{"Content-type", "application/json"}], rooster_json:encode(Response)}, Routes}
    end.

%% @doc add module to all routes tuples
%%
load_routes([], Acc) -> Acc;
load_routes([M|T], Acc) ->
    NewAcc = Acc ++ apply_module(apply(M, exports, []), M, []),
    load_routes(T, NewAcc).

%% @doc add module to routes tuples
%%
apply_module([], _, Acc) -> Acc;
apply_module([H|T], Module, Acc) ->
    NewEntry = erlang:insert_element(1, H, Module),
    apply_module(T, Module, [NewEntry|Acc]).
