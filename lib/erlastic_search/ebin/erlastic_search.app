%% This is the application resource file (.app file) for the erlastic_search,
%% application.
{application, erlastic_search, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [erlastic_search_app,
              erlastic_search_sup]},
   {registered,[erlastic_search_sup]},
   {applications, [kernel, stdlib]},
   {mod, {erlastic_search_app,[]}},
   {start_phases, []}]}.

