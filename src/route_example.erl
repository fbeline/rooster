-module(route_example).
-include_lib("rooster.hrl").
-export([exports/0, get_products/2, save_product/2]).


get_products(_Req, Resp) ->
    error_logger:info_msg("~p", [Resp]), 
    {200, {[{<<"name">>, <<"pool">>}, {<<"price">>,<<"150">>}]}}.	

save_product(Req, _Resp) ->
    {201, Req#request.body}.

exports() ->
    [{'GET', "products", get_products}, {'POST', "products", save_product}].
