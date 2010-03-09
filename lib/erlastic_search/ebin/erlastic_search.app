%% This is the application resource file (.app file) for the erlastic_search,
%% application.
{application, erlastic_search, 
  [{description, "An Erlang app for communicating with Elastic Search's rest interface."},
   {vsn, "0.1.0"},
   {modules, [erlastic_search,
              erls_resource,

              erls_query_constructor,
              erls_mochijson2]},
   {registered,[]},
   {applications, [kernel, stdlib, sasl, gas, ssl, crypto, lhttpc]},
   {start_phases, []}]}.

