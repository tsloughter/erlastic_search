-type header() :: {binary(), binary()}.
-type headers() :: [header()].
-type erlastic_json() :: tuple() | list().
%% Hackney async references actually are just that, references... but it seems
%% to be an undocumented implementation detail; doc (and specs) only says `any()'
-type erlastic_success_result() :: erlastic_json() | {async, HackneyRef :: any()}.

-record(erls_params, {
          host        = erls_config:get_host() :: binary(),
          port        = erls_config:get_port() :: integer(),

          % These are passed verbatim to the underlying http client in use.
          http_client_options = []:: [term()], 

          % Keeping the following two options for backwards compatibility.
          timeout     = infinity :: integer() | infinity,
          ctimeout    = infinity :: integer() | infinity
         }).
