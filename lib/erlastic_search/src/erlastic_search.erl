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

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index to create and sends the request to 
%% Elastic Search, the default settings on localhost.
%%
%% @spec create_index(Index) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
create_index(Index) ->
    create_index(#erls_params{}, Index).

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index and the record describing the servers
%% details to create and sends the request to Elastic Search.
%%
%% @spec create_index(Params, Index) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
create_index(Params, Index) ->
    erls_resource:put(Params, Index, [], [], [], []).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in 
%% Erlang terms, converts the document to a string and passes to the
%% default server. Elastic Search provides the doc with an id.
%%
%% @spec index(Index, Type, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
index_doc(Index, Type, Doc) when is_tuple(Doc) ->
    index_doc(#erls_params{}, Index, Type, Doc).
    
%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in 
%% Erlang terms, converts the document to a string and passes to the
%% server. Elastic Search provides the doc with an id.
%%
%% @spec index(Params Index, Type, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
index_doc(Params, Index, Type, Doc) when is_tuple(Doc) ->
    Json = erls_mochijson2:encode(Doc),
    erls_resource:post(Params, filename:join(Index, Type), [], [], Json, []).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in 
%% Erlang terms, converts the document to a string after adding the _id field
%% and passes to the default server. 
%%
%% @spec index(Index, Type, Id, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
index_doc_with_id(Index, Type, Id, Doc) when is_tuple(Doc) ->
    index_doc_with_id(#erls_params{}, Index, Type, Id, Doc).
    
%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in 
%% Erlang terms, converts the document to a string after adding the _id field
%% and passes to the server. 
%%
%% @spec index(Params, Index, Type, Id, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
index_doc_with_id(Params, Index, Type, Id, Doc) when is_tuple(Doc) ->
    Json = erls_mochijson2:encode(Doc),
    erls_resource:post(Params, filename:join([Index, Type, Id]), [], [], Json, []).

search(Index, Query) ->
    search(#erls_params{}, Index, "", Query).

search(Params, Index, Query) when is_record(Params, erls_params) ->
    search(Params, Index, "", Query);

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a query as "key:value" and sends 
%% it to the default Elastic Search server on localhost:9100
%%
%% @spec search(Index, Type, Query) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
search(Index, Type, Query) ->
    search(#erls_params{}, Index, Type, Query). 

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a query as "key:value" and sends 
%% it to the Elastic Search server specified in Params.
%%
%% @spec search(Params, Index, Type, Query) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
search(Params, Index=[H|_T], Type=[H2|_T2], Query) when not is_list(H), is_list(H2) ->
    search(Params, [Index], Type, Query);
search(Params, Index=[H|_T], Type=[H2|_T2], Query) when is_list(H), not is_list(H2) ->
    search(Params, Index, [Type], Query);
search(Params, Index=[H|_T], Type=[H2|_T2], Query) when not is_list(H), not is_list(H2) ->
    search(Params, [Index], [Type], Query);
search(Params, Index, Type, Query) ->
    erls_resource:get(Params, filename:join([erls_utils:comma_separate(Index), Type, "_search"]), [], [{"q", Query}], []).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends 
%% it to the default Elastic Search server on localhost:9100
%%
%% @spec index(Index, Type, Id, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_doc(Index, Type, Id) ->
    get_doc(#erls_params{}, Index, Type, Id).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends 
%% it to the Elastic Search server specified in Params.
%%
%% @spec index(Params, Index, Type, Id, Doc) -> {ok, Data} | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_doc(Params, Index, Type, Id) ->
    erls_resource:get(Params, filename:join([Index, Type, Id]), [], [], []).

flush_index(Index) ->
    flush_index(#erls_params{}, Index).

flush_index(Params, Index=[H|_T]) when not is_list(H) ->
    flush_index(Params, [Index]);
flush_index(Params, Index) ->
    erls_resource:post(Params, filename:join([erls_utils:comma_separate(Index), "_flush"]), [], [], [], []).

flush_all() ->
    refresh_all(#erls_params{}).

flush_all(Params) ->
    erls_resource:post(Params, "_flush", [], [], [], []).

refresh_index(Index) ->
    refresh_index(#erls_params{}, Index).

refresh_index(Params, Index=[H|_T]) when not is_list(H) ->
    refresh_index(Params, [Index]);
refresh_index(Params, Index) ->
    erls_resource:post(Params, filename:join([erls_utils:comma_separate(Index), "_refresh"]), [], [], [], []).

refresh_all() ->
    refresh_all(#erls_params{}).

refresh_all(Params) ->
    erls_resource:post(Params, "_refresh", [], [], [], []).

delete_doc(Index, Type, Id) ->
    delete_doc(#erls_params{}, Index, Type, Id).

delete_doc(Params, Index, Type, Id) ->
    erls_resource:delete(Params, filename:join([Index, Type, Id]), [], [], []).

delete_doc_by_query(Index, Type, Query) ->
    delete_doc_by_query(#erls_params{}, Index, Type, Query).

delete_doc_by_query(Params, Index, Type, Query) ->
    erls_resource:delete(Params, filename:join([Index, Type]), [], [{"q", Query}], []).

optimize_index(Index) ->
    optimize_index(#erls_params{}, Index).

optimize_index(Params, Index=[H|_T]) when not is_list(H)->
    optimize_index(Params, [Index]);
optimize_index(Params, Index) ->
    erls_resource:post(Params, filename:join([erls_utils:comma_separate(Index), "_optimize"]), [], [], [], []).

