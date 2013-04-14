-type header() :: {binary(), binary()}.
-type headers() :: [header()].

-record(erls_params, {
          host        = <<"127.0.0.1">> :: binary(),
          port        = 9200 :: integer(),
          ssl         = false :: boolean(),
          prefix      = <<"/">> :: binary(),
          name        = default :: term(),
          timeout     = infinity :: integer() | infinity
}).
