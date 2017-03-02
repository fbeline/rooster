-record(request, {path :: string(),
                  method :: atom(),
                  headers,
                  body,
                  qs,
                  cookies,
                  authorization :: string(),
                  pathParams :: list()}).

-record(config, {ip={0,0,0,0} :: {},
                 port=8080 :: integer(),
                 routes=[] :: list(atom()),
                 middlewares=[] :: list(atom()),
                 resp_headers=[] :: list({string(), string()}),
                 static_path=["priv", "www"] :: list(string()),
                 ssl={ssl, false},
                 ssl_opts={ssl_opts,[]}}).

-type request() :: #request{}.
-type config() :: #config{}.
-type route() :: {module(), atom(), string(), any()}.
-type middleware() :: {module(), atom(), string(), any()}.
-type response() :: {integer(), any()}.
