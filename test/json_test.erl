-module(json_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
  Expected = <<"{\"foo\":\"bar\"}">>,
  Result = rooster_json:encode(#{foo => bar}),
  ?assertEqual(Expected, Result).

decode_test() ->
  Expected = #{foo => 42},
  Result = rooster_json:decode(<<"{\"foo\":42}">>),
  ?assertEqual(Expected, Result).
