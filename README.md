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

#examples

under construction..



#Dependencies
- mochiweb: HTTP server
- jiffy: JSON parser

#License
MIT