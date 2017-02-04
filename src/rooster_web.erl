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
	"/" ++ Path = Req:get(path),
	try
		Req:respond(rooster:analyze_request(Req))
	catch
		Type:What ->
			Report = ["web request failed",
				  {path, Path},
				  {type, Type}, {what, What},
				  {trace, erlang:get_stacktrace()}],
			error_logger:error_report(Report),
			Req:respond({500, [{"Content-Type", "text/plain"}],
				     "request failed, sorry\n"})
	end.

get_option(Option, Options) ->
	{proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
