-module(rooster_middleware).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3,
         enter/2, leave/2]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

enter(ReqResp, Names) -> dispatch(ReqResp, Names, enter).

leave(ReqResp, Names) -> dispatch(ReqResp, Names, leave).

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
dispatch(ReqResp, Names, Action) ->
  State = gen_server:call(?MODULE, get_state),
  Middleware = match_middleware(Names, State),
  middleware_reduce(ReqResp, Middleware, Action).

match_middleware(Names, Middleware) -> match_middleware(Names, Middleware, []).
match_middleware(_, [], Acc) -> Acc;
match_middleware(Names, [Middleware | T], Acc) ->
  Match = lists:filter(fun(Name) -> Name =:= maps:get(name, Middleware) end, Names),
  add_middleware(Match, {Names, T, Middleware, Acc}).

add_middleware([], {Names, T, _, Acc}) ->
  match_middleware(Names, T, Acc);
add_middleware(_, {Names, T, Middleware, Acc}) ->
  match_middleware(Names, T, [Middleware] ++ Acc).

middleware_reduce(ReqResp, [], _) -> ReqResp;
middleware_reduce(ReqResp, [Middleware | T], Action) ->
  Fun = maps:get(Action, Middleware),
  middleware_reduce(Fun(ReqResp), T, Action).