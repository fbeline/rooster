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

%% @doc handle http request, executing relevant routes and middleware that fit on it
%%
handle_call({analyze_route, Req}, _From, {Routes, Middlewares, RespHeaders}) ->
  {Status, Response} = rooster_dispatcher:match_route(Req, Routes, Middlewares),
  Headers = [{"Content-type", "application/json"}] ++ RespHeaders,
  make_response(Status, Response, Headers).

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

%% @doc build response based on status and headers
%%
-spec make_response(integer(), any(), list()) -> {atom(), atom(), tuple(), list()}.

make_response(404, _, Headers) ->
  Msg = rooster_json:encode(#{message => <<"Requested endpoint not found.">>}),
  {stop, normal, {404, Headers, Msg}, []};

make_response(Status, Response, Headers) ->
  {stop, normal, {Status, Headers, rooster_json:encode(Response)}, []}.
