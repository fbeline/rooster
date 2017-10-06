-type state() :: #{routes       => list(route()),
                   middleware   => list(map()),
                   resp_headers => list({string(), string()}),
                   version      => string()}.

-type request() :: #{path          => string(),
                     method        => atom(),
                     headers       => any(),
                     body          => any(),
                     qs            => any(),
                     cookies       => any(),
                     authorization => string(),
                     pathParams    => list()}.

-type config() :: #{ip          => {integer(), integer(), integer(), integer()},
                    port        => integer(),
                    static_path => list(string()),
                    ssl         => any(),
                    ssl_opts    => any()}.

-type route() :: {module(), atom(), string(), any()}.
-type middleware() :: {module(), atom(), string(), any()}.
-type response() :: {integer(), any()}.
