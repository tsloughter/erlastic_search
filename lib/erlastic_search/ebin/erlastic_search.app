%% This is the application resource file (.app file) for the erlastic_search,
%% application.
{application, erlastic_search, 
  [{description, "An Erlang app for communicating with Elastic Search's rest interface."},
   {vsn, "0.1.0"},
   {modules, [erlastic_search_app,
              erlastic_search_sup,
              erlastic_search,
              erls_resource,

              erls_mochijson2]},
   {registered,[erlastic_search_sup]},
   {applications, [kernel, stdlib, sasl, gas, ssl, crypto, lhttpc]},
   {mod, {erlastic_search_app,[]}},
   {start_phases, []}]}.

