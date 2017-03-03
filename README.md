![logo](https://cloud.githubusercontent.com/assets/5730881/23146635/5fe187f4-f7b7-11e6-96fd-858b33b57ee2.png)

#Features
- **Routes** that supports `GET` `POST` `PUT` and `DELETE` methods.
- **Middlewares**: Functions that have access to the request and the response, intercepting routes before and/or after execution, also can judge and decide if the next middleware/route in the application cycle will be executed.
- **Basic Authentication**: Rooster provide a basic authentication module that can be easily integrated with middlewares.
- **CORS configuration**: You are able to easily configure CORS for your application.
- **HTTPS Support**
- **0% down time**: You can change your code in real time! The changes will be available in the the next request (without stopping the application).

#Installation
1) Download and install [rebar3](https://www.rebar3.org/)

2) Create an application using rebar

3) Edit the file **rebar.config** and add the following lines inside deps:

	{deps, [
	        ...
	        {rooster, ".*", {git, "git://github.com/FelipeBB/rooster.git", {branch, "master"}}}
	       ]}.

4) Run the command: rebar3 compile

That is it, we are ready to move forward.

#Running the server

Create a `app` module as the following one:
*ps: The name for the starter module is a convention, if the `app` module doesn't exists the server will not start.*

	-module(app).
	-include_lib("rooster/include/rooster.hrl").

	-export([start/0]).

	start() ->
		Options = #config{port=8080,
				  routes=[route_example], % implemented routes
				  middlewares=[], % implemented middlewares
				  resp_headers=[{"access-control-allow-methods", "*"},
				                {"access-control-allow-headers", "*"},
				                {"access-control-allow-origin", "*"}]},
		rooster:start_server(Options). 

This module will be responsible for starting the server. The **#config** record is used to configure the server port, the response headers and also the implemented routes and middlewares that the framework should handle. With this module created just run the following command in the terminal and your server should start.

	erl \
	    -pa ebin _build/default/lib/*/ebin \
	    -boot start_sasl \
	    -s app \
	    -s reloader
	    
if you see something like it`rooster listening on port 8080`, then everything is fine.

#Route example
Simple route example.

	-module(route_example).
	-include_lib("rooster/include/rooster.hrl").
	-export([exports/0, get_products/2, save_product/2, get_product/2]).


	get_products(_Req, _Resp) ->
	    {200, #{id => 43, price => 150}}.	

	get_product(Req, _Resp) ->
	    PathParams = Req#request.pathParams,
	    Id = proplists:get_value(":id", PathParams),
	    {200, #{id => Id, price => 8000}}.

	save_product(Req, _Resp) ->
	    {201, Req#request.body}.

	exports() ->
	    [{'GET', "products", get_products}, {'GET', "products/:id", get_product}, {'POST', "products", save_product}].


The **exports** method is required, it will provide the list of available endpoints that this module contains. Each tuple should have the http method, the route itself and the function that will be executed. 

Is important to note that the functions **must** have two parameters, **Req** and **Resp**, the `Resp` will contains the possible result of previous middlewares and the `Req` all the major information.

	-record(request,{
			 path,
			 method,
			 headers,
			 body,
			 qs,
			 cookies,
			 authorization,
			 pathParams
			}).

#Middleware example

Follows an example of a middleware used to authenticate the API through basic authentication.

	-module(middleware_example).
	-include_lib("rooster/include/rooster.hrl").
	-export([exports/0, basic_auth/2]).



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

The method **exports** will return a list of tuples, the first argument is the moment when the middleware will be executed(before or after the request), the second is the regex that will be evaluated based on the requested route, and the final one is the function that will be executed.

	{'BEFORE'|'AFTER', RegEx, Method}
	
The function return should be `{next|any(), any()}`. When something different from `next` is passed the rooster will not execute the following middleware/route and will return the Resp directly to the client. Otherwise the next middleware/route will be executed and the `Resp` parameter of it will be the Result of the current middleware, creating a chain of executions.

#SSL configuration
After generate the ssl certifier for your domain, everything that need to be done is to pass some extra parameters in the application initialization (**ssl** and **ssl_opts**). Follows an example of how the `app.erl` should looks like:

	-module(app).
	-include_lib("rooster/include/rooster.hrl").

	-export([start/0]).

	start() ->
	    Options = #config{port=443,
		              routes=[example_route], % implemented routes
		              ssl={ssl, true},
		              ssl_opts={ssl_opts, [
		                                   {certfile, "/etc/letsencrypt/live/{some.domain}/cert.pem"},
		                                   {keyfile, "/etc/letsencrypt/live/{some.domain}/privkey.pem"}]}
		             },
	    rooster:start_server(Options).


#Benchmark

The tests was made in a machine with 4 cores of 3.10GHz and 8gb of RAM running a Ubuntu OS version 16.04. All the tested API's was handling exactly the same request, under the minimal framework configuration.
The tool used to benchmark the API's was the [wrk](https://github.com/wg/wrk).


![benchmark](https://cloud.githubusercontent.com/assets/5730881/23285787/09a2bfb8-fa12-11e6-990e-6a7014f52122.png)


You can find the complete information around this benchmark inside the file **benchmark.md**


#Dependencies
- mochiweb: HTTP server
- jiffy: JSON parser

#License
MIT
