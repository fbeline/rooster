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
