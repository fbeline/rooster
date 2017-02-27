-module(rooster_holder).

-behaviour(gen_server).
-include_lib("rooster.hrl").

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

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

handle_call(get_state, _From, State) ->
    {reply, State, State}.
