-module(route_example).
-export([exports/0, get_products/2, save_product/2]).


get_products(_Req, _Params) ->
    {200, {[{<<"name">>, <<"pool">>}, {<<"price">>,<<"150">>}]}}.	

save_product(_Req, Params) ->
    {201, rooster:get_payload(Params)}.

exports() ->
    [{'GET', "products", get_products}, {'POST', "products", save_product}].
