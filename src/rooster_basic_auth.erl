-module(rooster_basic_auth).

-export([is_authorized/2]).

-type credentials() :: {string(), string()}.

%% @doc check if the request basic authentication 
%% credentials match the configured
-spec is_authorized(string(), credentials()) -> true | false.

is_authorized(Auth, Credentials) ->
    case Auth of
        "Basic" ++ EncodedCredentials ->
            RCredentials = parse_credentials(EncodedCredentials),
            RCredentials =:= Credentials;
        _ ->
            false
    end.

%% @doc decode base64 credential
%%
-spec parse_credentials(string()) -> credentials() | malformed_credentials.

parse_credentials(EncodedCredentials) ->
    Credentials = base64:decode_to_string(EncodedCredentials),
    case string:tokens(Credentials, ":") of
        [Username, Password] ->
            {Username, Password};
        _ ->
            malformed_credentials
    end.
