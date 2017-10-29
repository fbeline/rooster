# Rooster [![Build Status](https://travis-ci.org/fbeline/rooster.svg?branch=master)](https://travis-ci.org/fbeline/rooster) 
Simplistic REST framework that runs on top of mochiweb.
## Features
- **Routes** that supports `GET` `POST` `PUT` and `DELETE` methods.
- **Middleware**: Functions that have access to the request and the response, intercepting routes before and/or after execution.
- **Basic Authentication**: Rooster provide a basic authentication module that can be easily integrated with Middleware.
- **HTTPS Support**

## Installation
1) Download and install [rebar3](https://www.rebar3.org/)

2) Create an application using rebar

3) Edit the file **rebar.config** and add the following lines inside deps:

`{deps, [ {rooster, ".*", {git, "git://github.com/fbeline/rooster.git", {branch, "master"}}} ]}.`

4) Run the command: rebar3 compile

## Route example
Simple route example.

```Erlang
-export([exports/0]).

get_products(_Req) ->
  {200, #{id => 43, price => 150}, [{"custom-header", "foo"}]}.

get_product(#{params := params}) ->
  Id = maps:get(id, params),
  {200, #{id => Id, price => 8000}}.

save_product(#{body := Body}) ->
  {201, Body}.

exports() ->
  [{'GET', "products", fun get_products/1},
   {'GET', "products/:id", fun get_product/1},
   {'POST', "products", fun save_product/1, [auth]}].
```

The **exports** method will provide the list of available endpoints that this module contains. Each tuple should have the HTTP method, the route itself, the route function and the middleware (name) that will be executed for this route. 

Is important to note that the function **must** have one parameter, that will contains the request information.

```erlang

#{path          => ...,
  method        => ...,
  headers       => ...,
  body          => ...,
  qs            => ...,
  params        => ...,
  cookies       => ...,
  authorization => ...}
```

## Middleware example

- Middleware get the response returned by route (or other `leave` middleware) and multiply it by 2.

```Erlang
-export([double/0]).

double() ->
  #{name  => double,
    leave => fun({Status, Resp, Headers}) -> {Status, #{result => Resp * 2}, Headers} end}.
```

- Middleware used to authenticate through basic authentication.

```erlang
-export([auth/0]).

basic_auth(#{authorization := Auth} = Req) ->
  Authorizated = rooster_basic_auth:is_authorized(Auth, {"admin", "admin"}),
  case Authorizated of
    true ->
      Req;
    _ ->
      {break, {403, #{reason => <<"Acess Forbidden">>}}}
  end.
  
auth() ->
  #{name => auth,
    enter => fun basic_auth/1}.
```

The middleware map can have both `leave` and `enter` keys. The `enter` function will have access to the request information and will be able to change it, the `leave` function will have access to the response and will be able to change it as well.
At any moment that a middleware returns `{break, {status, response}}` the chain of execution will be break and the `response` will be evaluated as the request result.

![middleware](https://user-images.githubusercontent.com/5730881/32140052-75ae38aa-bc3a-11e7-9f54-855b96390bd9.png)

## How to configure and run

under construction..

## SSL
After generating the SSL certificate for your domain, everything that needs to be done is to pass some extra parameters for the server configuration map: (**ssl** and **ssl_opts**).

```Erlang
#{port     => 8080,
  ssl      => {ssl, true},
  ssl_opts => {ssl_opts, [{certfile, "{PATH}/server_cert.pem"},
                          {keyfile, "{PATH}/server_key.pem"}]}}
```

## Benchmark

The tests were made on a machine with 4 cores of 3.10GHz and 8gb of RAM running a Ubuntu OS version 16.04. All the tested API's was handling exactly the same request, under the minimal framework configuration.
The tool used to benchmark the API's was the [wrk](https://github.com/wg/wrk).

![benchmark](https://cloud.githubusercontent.com/assets/5730881/23285787/09a2bfb8-fa12-11e6-990e-6a7014f52122.png)

You can find the complete information around this benchmark inside the file [**benchmark**](benchmark.txt)

## Dependencies
- mochiweb: HTTP server
- jsx: JSON parser

## License
MIT
