%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erls_query_constructor).
-compile([export_all]).

-include("erlastic_search.hrl").

term_query() ->
    ok.

range_query() ->
    ok.

prefix_query() ->
    ok.

wildcard_query() ->
    ok.

match_all_query() ->
    ok.

query_string_query() ->
    ok.

field_query() ->
    ok.

boolean_query() ->
    ok.

disjunction_max_query() ->
    ok.

constant_score_query() ->
    ok.

filtered_query() ->
    ok.

more_like_this_query() ->
    ok.

more_like_this_field_query() ->
    ok.

term_filter() ->
    ok.

range_filter() ->
    ok.

prefix_filter() ->
    ok.

query_filter() ->
    ok.

boolean_filter() ->
    ok.
