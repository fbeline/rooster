-module(rooster_state).

-behaviour(gen_server).
-include_lib("rooster.hrl").

-export([start/0, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ===============
%% Public API
%% ===============
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).


%% ===============
%% Server API
%% ===============
handle_cast(stop, Env) ->
  {stop, normal, Env};

handle_cast({set_state, State}, _State) ->
  {noreply, rooster_adapter:state(State)}.

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(get_new_state, _From, State) ->
  NewState = app:exports(),
  get_state(State, NewState).


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

%% ===============
%% Private API
%% ===============

%% @doc get available option for routes and middleware
%%
-spec get_state(state(), state()) -> {reply, state(), state()}.

get_state(State = #{version := Version}, #{version := Version}) ->
  {reply, State, State};

get_state(_, State) ->
  NewState = rooster_adapter:state(State),
  {reply, NewState, NewState}.
