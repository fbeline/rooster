-module(rooster_state).

-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, get/0]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ===============
%% Public API
%% ===============
start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

get() ->
  gen_server:call(?MODULE, get_state).

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
  {reply, State, State}.

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