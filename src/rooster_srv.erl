-module(rooster_srv).
-behaviour(gen_server).

-export([start/1, stop/0, init/1]).
-export([handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start(Routes) ->
	NewRoutes = load_routes(Routes, []),
	gen_server:start_link({local, ?MODULE}, ?MODULE, NewRoutes, []).

stop() ->
	gen_server:cast(?MODULE, stop).

init(Env) ->
	io:format("Starting...~n"),
	{ok, Env}.

terminate(_Reason, _Env) ->
	io:format("Terminating...~n").

handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreplay, State}.

code_change(_OldVsn, State, _Extra) ->
	{noreplay, State}.

handle_cast(stop, Env) ->
	{stop, normal, Env}.

%% check requested Path
%%
handle_call({analyze_route, Req}, _From, Routes) ->
	"/" ++ Path = Req:get(path),
	try
		Method = Req:get(method),
		{Status, Response} = dispatcher:match_route(Path, Method, Req, Routes),
		io:format("~n~p~n", [Response]),
		case Status of
			404 ->
				Req:respond({404, [{"Content-type", "text/plain"}], "Requested endpoint not found."});
			_ ->
				Req:respond({Status, [{"Content-type", "application/json"}], mochijson2:encode(Response)})
		end
	catch
		Type:What ->
			Report = ["web request failed",
				  {path, Path},
				  {type, Type}, {what, What},
				  {trace, erlang:get_stacktrace()}],
			error_logger:error_report(Report),
			Req:respond({500, [{"Content-Type", "text/plain"}],
				     "request failed, sorry\n"})
	after
		io:format("~n=====================~n~p~n", [Routes]),
		{replay, {}, Routes}
	end.
%% add module to all routes tuples
%%
load_routes([], Acc) -> Acc;
load_routes([M|T], Acc) ->
	NewAcc = Acc ++ apply_module(apply(M, exports, []), M, []),
	load_routes(T, NewAcc).

%% add module to routes tuples
%%
apply_module([], _, Acc) -> Acc;
apply_module([H|T], Module, Acc) ->
	NewEntry = erlang:insert_element(1, H, Module),
	apply_module(T, Module, [NewEntry|Acc]).
