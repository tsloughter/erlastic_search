-type header() :: {binary(), binary()}.
-type headers() :: [header()].

-record(erls_params, {
          host        = get_host() :: binary(),
          port        = get_port() :: integer(),

          % These are passed verbatim to the underlying http client in use.
          http_client_options = []:: [term()], 

          % Keeping the following two options for backwards compatibility.
          timeout     = infinity :: integer() | infinity,
          ctimeout    = infinity :: integer() | infinity
         }).

get_host() ->
    case application:get_env(erlastic_search, host) of
        undefined ->
            <<"localhost">>;
        {ok, Host}->
            Host
    end.

get_port() ->
    case application:get_env(erlastic_search, port) of
        undefined ->
            9200;
        {ok, Port}->
            Port
    end.
