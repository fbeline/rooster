-module(rooster_srv).
-behaviour(gen_server).

-export([start/1, stop/0, analyze_route/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ===============
%% Public API
%% ===============
start(State) ->
  gen_server:start_link(?MODULE, State, []).

stop() ->
  gen_server:cast(?MODULE, stop).

analyze_route(Pid, Req) ->
  gen_server:call(Pid, {analyze_route, Req}).

%% ===============
%% Server API
%% ===============

handle_call({analyze_route, Req}, _From, Routes) ->
  Resp = rooster_dispatcher:match_route(Req, Routes),
  {stop, normal, rooster_adapter:server_response(Resp), []}.

handle_cast(stop, Env) ->
  {stop, normal, Env}.

%% ===============
%% Server callbacks
%% ===============
init(Env) ->
  process_flag(trap_exit, true),
  {ok, Env}.

terminate(_Reason, _Env) ->
  ok.

handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ===============
%% Private API
%% ===============
