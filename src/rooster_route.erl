-module(rooster_route).

-behaviour(gen_server).

-export([start_link/1, get/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% ===============
%%% API
%% ===============
start_link(State) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

get() ->
  gen_server:call(?MODULE, get_state).

%% ===============
%%% gen_server callbacks
%% ===============
init(Env) ->
  {ok, routes(Env)}.

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

%% ===============
%%% Internal functions
%% ===============
routes(Routes) ->
  adapt_nested(lists:flatten(Routes), []).

adapt_nested([], Acc) -> Acc;
adapt_nested([{Path, Middleware, Nested}|T], Acc)
  when erlang:is_list(Middleware), erlang:is_list(Nested) ->
  adapt_nested(T, Acc ++ nested(Path, Middleware, rooster_adapter:nested_route(Nested)));
adapt_nested([Route|T], Acc) ->
  adapt_nested(T, Acc ++ [rooster_adapter:with_middleware(Route)]).

nested(_, _, []) -> [];
nested(Path, Middleware, [{Method, NPath, Fn, NMiddleware}|T]) ->
  [{Method, Path ++ NPath, Fn, Middleware ++ NMiddleware}] ++ nested(Path, Middleware, T).
