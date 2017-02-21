![logo](https://cloud.githubusercontent.com/assets/5730881/23146635/5fe187f4-f7b7-11e6-96fd-858b33b57ee2.png)

#Features
- **Routes** that supports `GET` `POST` `PUT` and `DELETE` methods.
- **Middlewares**: Functions that have access to the request and the response, intercepting routes before and/or after execution, also can judge and decide if the next middleware/route in the application cycle will be executed.
- **Basic Authentication**: Rooster provide a basic authentication module that can be easily integrated with middlewares.
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

#Route example
Simple route example.

	-module(route_example).
	-include_lib("rooster.hrl").
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

The method **exports** will return a list of tuples, the first argument is the moment when the middleware will be executed(before or after the request), the second is the regex that will be evaluated based on the requested route, and the final one is the function that will be executed.

	{'BEFORE'|'AFTER', RegEx, Method}
	
The function return should be `{next|any(), any()}`. When something different from `next` is passed the rooster will not execute the following middleware/route and will return the Resp directly to the client. Otherwise the next middleware/route will be executed and the `Resp` parameter of it will be the Result of the current middleware, creating a chain of executions.

#Dependencies
- mochiweb: HTTP server
- jiffy: JSON parser

#License
MIT