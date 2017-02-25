-module(rooster_srv).
-include_lib("rooster.hrl").
-behaviour(gen_server).

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start(State) ->
    gen_server:start_link(?MODULE, State, []).

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
    {stop, normal, Env}.

%% @doc handle http request, executing relevant routes and middlewares that fit on it
%%
handle_call({analyze_route, Req}, _From, {Routes, Middlewares, RespHeaders}) ->
    {Status, Response} = rooster_dispatcher:match_route(Req, Routes, Middlewares),
    case Status of
        404 ->
            {stop, normal, {404, [{"Content-type", "text/plain"}], "Requested endpoint not found."}, []};
        _ ->
            {stop, normal, {Status, [{"Content-type", "application/json"}] ++ RespHeaders, rooster_json:encode(Response)}, []}
    end.

