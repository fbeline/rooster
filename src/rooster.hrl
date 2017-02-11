-record(request, {path :: string(),
                  method :: atom(),
                  headers,
                  body,
                  qs,
                  cookies,
                  pathParams :: list()}).

-type request() :: #request{}.
-type route() :: {module(), atom(), string(), any()}.
-type response() :: {integer(), any()}.
