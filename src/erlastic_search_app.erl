%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@zinn>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 31 Aug 2012 by Tristan Sloughter <tristan@zinn>
%%%-------------------------------------------------------------------
-module(erlastic_search_app).

-export([start_deps/0]).

start_deps() ->
    start_deps(erlastic_search, permanent).

start_deps(App, Type) ->
    case application:start(App, Type) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            start_deps(Dep, Type),
            start_deps(App, Type)
    end.

