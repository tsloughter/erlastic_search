%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erlastic_search).
-compile([export_all]).

-include("erlastic_search.hrl").

create_index(Index) ->
    erls_resource:put(#erls_params{}, Index, [], [], [], []).
 
create_index(Params, Index) ->
    erls_resource:put(Params, Index, [], [], [], []).

index_doc(Index, Type, Doc) when is_tuple(Doc) ->
    Json = erls_mochijson2:encode(Doc),
    erls_resource:post(#erls_params{}, Index++"/"++Type, [], [], Json, []).

index_doc(Index, Type, Id, Doc) when is_tuple(Doc) ->
    {struct, PropList} = Doc,
    Json = erls_mochijson2:encode({struct, [{<<"_id">>, Id} | PropList]}),
    erls_resource:post(#erls_params{}, Index++"/"++Type, [], [], Json, []).

search(Index, Type, Query) ->
    erls_resource:get(#erls_params{}, Index++"/"++Type++"/_search", [], [{"q", Query}], []).

get_doc(Index, Type, Id) ->
    erls_resource:get(#erls_params{}, Index++"/"++Type++"/"++Id, [], [], []).
