-module(rooster_middleware).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3,
         enter/2, leave/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

enter(ReqResp, Names) ->
  State = gen_server:call(?MODULE, get_state),
  Middleware = match_middleware(Names, State),
  middleware_reduce(ReqResp, Middleware, enter).

leave(ReqResp, Names) ->
  State = gen_server:call(?MODULE, get_state),
  Middleware = match_middleware(Names, State),
  middleware_reduce(ReqResp, Middleware, leave).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Env) ->
  InternalState = lists:map(fun rooster_adapter:middleware/1, Env),
  {ok, InternalState}.

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

match_middleware(Names, Middleware) -> match_middleware(Names, Middleware, []).
match_middleware(_, [], Acc) -> Acc;
match_middleware(Names, [Middleware | T], Acc) ->
  Match = lists:filter(fun(Name) -> Name =:= maps:get(name, Middleware) end, Names),
  if Match =/= [] ->
    match_middleware(Names, T, Acc ++ [Middleware]);
  true ->
    match_middleware(Names, T, Acc)
  end.

middleware_reduce(ReqResp, [], _) -> ReqResp;
middleware_reduce(ReqResp, [#{enter := Fn} | T], enter) ->
  Result = Fn(ReqResp),
  middleware_reduce(Result, T, enter);
middleware_reduce(ReqResp, [#{leave := Fn} | T], leave) ->
  Result = Fn(ReqResp),
  middleware_reduce(Result, T, leave).


