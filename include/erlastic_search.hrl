-type header() :: {binary(), binary()}.
-type headers() :: [header()].

-record(erls_params, {
          host        = <<"127.0.0.1">> :: binary(),
          port        = 9200 :: integer(),

          % These are passed verbatim to the underlying http client in use.
          http_client_options = []:: [term()], 

          % Keeping the following two options for backwards compatibility.
          timeout     = infinity :: integer() | infinity,
          ctimeout    = infinity :: integer() | infinity
         }).
