-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].

-record(erls_params, {
          host        = "127.0.0.1" :: string(),
          port        = 9200 :: integer(),
          ssl         = false :: boolean(),
          prefix      = "/" :: string(),
          name        = default :: term(),
          timeout     = infinity :: integer() | infinity
}).
