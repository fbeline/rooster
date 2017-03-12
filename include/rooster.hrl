-record(request, {path :: string(),
                  method :: atom(),
                  headers,
                  body,
                  qs,
                  cookies,
                  authorization :: string(),
                  pathParams :: list()}).

-record(config, {ip={0,0,0,0} :: {integer(), integer(), integer(), integer()},
                 port=8080 :: integer(),
                 static_path=["priv", "www"] :: list(string()),
                 ssl={ssl, false},
                 ssl_opts={ssl_opts,[]}}).

-record(state, {routes=[] :: list(atom()),
                middleware=[] :: list(atom()),
                resp_headers=[] :: list({string(), string()}),
                version="0.0.0" :: string()}).

-type request() :: #request{}.
-type config() :: #config{}.
-type state() :: #state{}.
-type route() :: {module(), atom(), string(), any()}.
-type middleware() :: {module(), atom(), string(), any()}.
-type response() :: {integer(), any()}.
