-module(rooster_route).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% ===============
%%% API
%% ===============
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===============
%%% gen_server callbacks
%% ===============
init(Env) ->
  Routes = routes(Env),
  {ok, Routes}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ===============
%%% Internal functions
%% ===============
routes(Routes) ->
  Routes.
