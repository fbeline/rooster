![logo](https://cloud.githubusercontent.com/assets/5730881/23146635/5fe187f4-f7b7-11e6-96fd-858b33b57ee2.png)

#Features
- **Routes** that supports `GET` `POST` `PUT` and `DELETE` methods.
- **Middlewares**: Functions that have access to the request and the response, intercepting routes before and/or after execution, also can judge and decide if the next middleware/route in the application cycle will be executed.
- **Basic Authentication**: Rooster provide a basic authentication module that can be easily intregated with the middleware system.
- **0% down time**: You can change your code in real time! The changes will be available in the the next request (without stopping the application).

#Installation
1) Download and install [rebar3](https://www.rebar3.org/)

2) Edit the file **rebar.config** and add the following lines inside deps:

	{deps, [
	        ...
	        {rooster, ".*", {git, "git://github.com/FelipeBB/rooster.git", {branch, "master"}}}
	       ]}.

3) Run the command: rebar3 compile

That is it, we are ready to move foward.

#Middleware example

Follows an example of a middleware used to authenticate the API through basic authentication.

	-module(middleware_example).
	-export([exports/0, basic_auth/2]).

	-include_lib("rooster.hrl").

	basic_auth(Req, Resp) ->
	    Auth = Req#request.authorization,
	    Authorizated = rooster_basic_auth:is_authorized(Auth, {"admin", "admin"}),
	    case Authorizated of
		true ->
		    {next, Resp};
		_ ->
		    {break, {403, {[{<<"reason">>, <<"Acess Forbidden">>}]}}}
	    end. 


	exports() ->
	    [{'BEFORE', ".*", basic_auth}].

The method **exports** will return a list of tuples, the first argument is the moment when the middleware will be executed(before or after the request), the second is the regex that will be evaluated based on the requested route, and the final one is the method that will be executed.

	{'BEFORE'|'AFTER', RegEx, Method}
	
The method return should be `{next|any(), any()}`. When something different from `next` is passed the rooster will not execute the following middleware/route and will return the Resp directly to the client. Otherwise the next middleware/route will be executed and the `Resp` parameter of it will be the Result of our current middleware, creating a chain of executions.
#Dependencies
- mochiweb: HTTP server
- jiffy: JSON parser

#License
MIT