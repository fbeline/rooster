-record(request, {path, method, headers, body, qs, cookies, pathParams}).

-type request() :: #request{}.
-type route() :: {module(), atom(), string(), any()}.
