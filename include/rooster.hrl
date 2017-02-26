-record(request, {path :: string(),
                  method :: atom(),
                  headers,
                  body,
                  qs,
                  cookies,
                  authorization :: string(),
                  pathParams :: list()}).

-record(config, {port=8080 :: integer(),
                 routes=[] :: list(atom()),
                 middlewares=[] :: list(atom()),
                 resp_headers=[] :: list({string(), string()})}).

-type request() :: #request{}.
-type route() :: {module(), atom(), string(), any()}.
-type middleware() :: {module(), atom(), string(), any()}.
-type response() :: {integer(), any()}.
