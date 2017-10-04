-module(rooster_middleware).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3,
         enter/2, leave/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(State) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

enter(fn, [H|T]) ->
  foo.

leave(fn, [H|T]) ->
  foo.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Env) ->
  {ok, Env}.

handle_call(get_state, _From, State) ->
  {reply, State, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

