-module(basic_auth_test).

-include_lib("eunit/include/eunit.hrl").

is_authorized_sanity_test() ->
  Auth = "Basic YWRtaW46YWRtaW4=", % admin:admin
  Credentials = {"admin", "admin"},
  Resp = rooster_basic_auth:is_authorized(Auth, Credentials),
  ?assertEqual(true, Resp).

is_authorized_bad_credentials_test() ->
  Auth = "Basic YWRtaW46YWRtaW4=", % admin:admin
  Credentials = {"admin", "123"},
  Resp = rooster_basic_auth:is_authorized(Auth, Credentials),
  ?assertEqual(false, Resp).

parse_credentials_sanity_credentials_test() ->
  Auth = "YWRtaW46YWRtaW4=",
  Credentials = {"admin", "admin"},
  Resp = rooster_basic_auth:parse_credentials(Auth),
  ?assertEqual(Credentials, Resp).

parse_credentials_malformed_credentials_test() ->
  Auth = "Basic YWRtRtaW4=",
  Credentials = {"admin", "admin"},
  Resp = rooster_basic_auth:is_authorized(Auth, Credentials),
  ?assertEqual(false, Resp).
