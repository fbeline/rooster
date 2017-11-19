# Rooster [![Build Status](https://travis-ci.org/fbeline/rooster.svg?branch=master)](https://travis-ci.org/fbeline/rooster)
Simplistic REST framework that runs on top of mochiweb.
## Features
- **Routes** Composable routing system that supports `GET` `POST` `PUT` and `DELETE` http verbs.
- **Middleware**: Functions that have access to the request and the response, intercepting routes before and/or after execution.
- **Basic Authentication**: Authentication module that can be easily integrated with Middleware.
- **HTTPS Support**

## Installation
1) Download and install [rebar3](https://www.rebar3.org/)

2) Create a new application using rebar

3) Edit the file **rebar.config** and add the following lines inside deps:

`{deps, [ {rooster, ".*", {git, "git://github.com/fbeline/rooster.git", {branch, "master"}}} ]}.`

4) Compile and download dependencies with `rebar3 compile`

## Quick start
Create an entry file that will be the initialization module of your application:

```Erlang
-module(server).
-export([start/0]).

start() ->
  rooster:start(#{port => 8080},
                #{routes => [hello()]}).

hello() ->
  {'GET', "/hello", fun(_) -> {200, #{message => <<"hello world">>}} end}.
```

Start it using the command:

```Bash
erl \
  -pa ebin _build/default/lib/*/ebin \
  -boot start_sasl \
  -s server \
  -s reloader
```

Run `curl localhost:8080/hello` and it should return:

```JSON
{"message": "hello world"}
```

## Routes
Given the following functions, you will find how to define routes using *generic* or *nested* definition.

```Erlang
-export([exports/0]).

% custom header
get_products(_Req) ->
  {200, [#{..}, #{..}], [{"custom-header", "foo"}]}.

% request path param
get_product(#{params := params}) ->
  Id = maps:get(id, params),
  {200, #{id => Id, price => 8000}}.

% request payload
save_product(#{body := Body}) ->
  {201, Body}.
```
Is important to note that the function **must** have one parameter, that will contain the request information.

### Generic definition
The simplest way of defining routes.

```Erlang
exports() ->
  [{'GET', "/products", fun get_products/1, [auth]},
   {'POST', "/products", fun save_product/1, [auth, admin]}
   {'GET', "/products/:id", fun get_product/1, [auth]}].
```

The **exports** method will provide the list of available endpoints that this module contains. Each tuple should have `{HTTP verb, route path, route handler, list of middleware}`, the list of middleware is not a required parameter as a specific route may use none.

### Nested definition
For routes that gonna share a specific root path and or middleware, declaring routes in a nested way should be the proper solution.

```Erlang
exports() ->
  [{"/products", [auth],
    [{'GET', fun get_products/1},
     {'POST', fun save_product/1, [admin]}
     {'GET', "/:id", fun get_product/1}]}].
```
The nested definition should fit on the specification:

```
{root path, list of middleware,
   [{HTTP verb, *nested path*, route handler, *list of middleware*}]}
```
Ps: The parameters surround by * are not required.

### Request
The request that will be passed to the route handlers is a map as the one bellow:

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

## Middleware

The middleware map can have both `leave` and `enter` keys. The `enter` function will have access to the request information and will be able to change it, the `leave` function will have access to the response and will be able to change it as well.
At any moment that a middleware returns `{break, {status, response}}` the chain of execution will terminate and the `response` will be evaluated as the request result.

![middleware](https://user-images.githubusercontent.com/5730881/32140052-75ae38aa-bc3a-11e7-9f54-855b96390bd9.png)

### CORS
Simple example using a middleware that intercepts the route handler response and
add to it custom headers.

```Erlang
-export([cors/0]).

access_control() ->
  [{"access-control-allow-methods", "*"},
   {"access-control-allow-headers", "*"},
   {"access-control-allow-origin", "*"}].

cors() ->
  #{name  => cors,
    leave => fun({Status, Resp, Headers}) -> {Status, Resp, Headers ++ access_control()} end}.
```

### Basic authentication
Intercepts the http request before the route handler executes and returns `403` if
credentials do not match.

```erlang
-export([auth/0]).

basic_auth(#{authorization := Auth} = Req) ->
  Authorizated = rooster_basic_auth:is_authorized(Auth, {"admin", "admin"}),
  case Authorizated of
    true ->
      Req;
    _ ->
      {break, {403, #{reason => <<"Access Forbidden">>}}}
  end.

auth() ->
  #{name => auth,
    enter => fun basic_auth/1}.
```

## SSL
After generating the SSL certificate for your domain, everything that needs to be done is to pass some extra parameters for the server configuration map: (**ssl** and **ssl_opts**).

```Erlang
#{port     => 8080,
  ssl      => {ssl, true},
  ssl_opts => {ssl_opts, [{certfile, "{PATH}/server_cert.pem"},
                          {keyfile, "{PATH}/server_key.pem"}]}}
```

## Dependencies
- mochiweb: HTTP server
- jsx: JSON parser

## License
MIT
