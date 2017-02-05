-module(route_example).
-include_lib("rooster.hrl").
-export([exports/0, get_products/1, save_product/1]).


get_products(_Req) ->
    {200, {[{<<"name">>, <<"pool">>}, {<<"price">>,<<"150">>}]}}.	

save_product(Req) ->
    {201, Req#request.body}.

exports() ->
    [{'GET', "products", get_products}, {'POST', "products", save_product}].
