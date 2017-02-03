-module(rooster_web).

-export([start/1, stop/0, loop/2]).


start(Options) ->
	{DocRoot, Options1} = get_option(docroot, Options),
	Loop = fun (Req) ->
			       ?MODULE:loop(Req, DocRoot)
	       end,
	mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
	mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
	rooster:analyze_request(Req).


get_option(Option, Options) ->
	{proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

