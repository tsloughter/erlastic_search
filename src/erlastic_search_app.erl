%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@zinn>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 31 Aug 2012 by Tristan Sloughter <tristan@zinn>
%%%-------------------------------------------------------------------
-module(erlastic_search_app).

-behaviour(application).

%% Application callbacks
-export([start/0
         ,start/2
         ,stop/1]).

-define(APP, erlastic_search).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
start() ->
    start_deps(?APP, permanent).

start_deps(App, Type) ->
    case application:start(App, Type) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            start_deps(Dep, Type),
            start_deps(App, Type)
    end.

start(_StartType, _StartArgs) ->
    ok.

stop(_State) ->
    ok.
