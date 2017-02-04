-module(rooster).
-export([start/0, stop/0, analyze_request/1, get_payload/1, get_path_params/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the greeting server.
start() ->
    rooster_deps:ensure(),
    ensure_started(crypto),
    application:start(rooster).


%% @spec stop() -> ok
%% @doc Stop the greeting server.
stop() ->
    application:stop(rooster).


%% analyze request
%%
analyze_request(Req) ->
	gen_server:call(rooster_srv, {analyze_route, Req}).

%% get request payload
%%
get_payload(Params) ->
	get_info(Params, body).

%% get request parh params
get_path_params(Params) ->
	get_info(Params, path_params).


%%  get information from params
get_info([{Type, Params}|_T], Type) ->
	Params;
get_info([_|T], Type) ->
	get_info(T, Type);
get_info([], _) ->
	{}.
