%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erlastic_search).

-export([create_index/1
        ,create_index/2
        ,index_doc/3
        ,index_doc/4
        ,index_doc_with_id/4
        ,index_doc_with_id/5
        ,index_doc_with_id_opts/6
        ,bulk_index_docs/2
        ,search/2
        ,search/3
        ,search/5
        ,search_limit/4
        ,get_doc/3
        ,get_doc/4
        ,flush_index/1
        ,flush_index/2
        ,flush_all/0
        ,flush_all/1
        ,refresh_all/0
        ,refresh_all/1
        ,refresh_index/1
        ,refresh_index/2
        ,delete_doc/3
        ,delete_doc/4
        ,delete_doc_by_query/3
        ,delete_doc_by_query/4
        ,delete_doc_by_query_doc/3
        ,delete_doc_by_query_doc/4
        ,optimize_index/1
        ,optimize_index/2]).

-include("erlastic_search.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index to create and sends the request to
%% Elastic Search, the default settings on localhost.
%% @end
%%--------------------------------------------------------------------
-spec create_index(binary()) -> {ok, list()} | {error, any()}.
create_index(Index) ->
    create_index(#erls_params{}, Index).

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index and the record describing the servers
%% details to create and sends the request to Elastic Search.
%% @end
%%--------------------------------------------------------------------
-spec create_index(record(erls_params), binary()) -> {ok, list()} | {error, any()}.
create_index(Params, Index) ->
    erls_resource:put(Params, Index, [], [], [], Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in
%% Erlang terms, converts the document to a string and passes to the
%% default server. Elastic Search provides the doc with an id.
%% @end
%%--------------------------------------------------------------------
-spec index_doc(binary(), binary(), list() | binary()) -> {ok, list()} | {error, any()}.
index_doc(Index, Type, Doc) ->
    index_doc(#erls_params{}, Index, Type, Doc).

-spec index_doc(record(erls_params), binary(), binary(), list() | binary()) -> {ok, list()} | {error, any()}.
index_doc(Params, Index, Type, Doc) when is_list(Doc) ->
    index_doc(Params, Index, Type, jsx:encode(Doc));
index_doc(Params, Index, Type, Doc) when is_binary(Doc) ->
    erls_resource:post(Params, filename:join(Index, Type), [], [], Doc, Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in
%% Erlang terms, converts the document to a string after adding the _id field
%% and passes to the default server.
%% @end
%%--------------------------------------------------------------------
-spec index_doc_with_id(binary(), binary(), binary(), list() | binary()) -> {ok, list()} | {error, any()}.
index_doc_with_id(Index, Type, Id, Doc) ->
    index_doc_with_id_opts(#erls_params{}, Index, Type, Id, Doc, []).

-spec index_doc_with_id(record(erls_params), binary(), binary(), binary(), list() | binary()) -> {ok, list()} | {error, any()}.
index_doc_with_id(Params, Index, Type, Id, Doc) ->
    index_doc_with_id_opts(Params, Index, Type, Id, Doc, []).

-spec index_doc_with_id_opts(record(erls_params), binary(), binary(), binary(), list() | binary(), list()) -> {ok, list()} | {error, any()}.
index_doc_with_id_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Doc), is_list(Opts) ->
    index_doc_with_id_opts(Params, Index, Type, Id, jsx:encode(Doc), []);
index_doc_with_id_opts(Params, Index, Type, Id, Doc, Opts) when is_binary(Doc), is_list(Opts) ->
    erls_resource:post(Params, filename:join([Index, Type, Id]), [], Opts, Doc, Params#erls_params.http_client_options).

%% Documents is [ {Index, Type, Id, Json}, ... ]
-spec bulk_index_docs(record(erls_params), list()) -> {ok, list()} | {error, any()}.
bulk_index_docs(Params, IndexTypeIdJsonTuples) ->
    Body = lists:map(fun({Index, Type, Id, Doc}) when is_binary(Doc) ->
                             Header = jsx:encode([
                                                  {<<"index">>, [{[
                                                                  {<<"_index">>, Index},
                                                                  {<<"_type">>, Type},
                                                                  {<<"_id">>, Id}
                                                                  ]}]}]),
                             [Header, <<"\n">>, Doc, <<"\n">>];
                        ({Index, Type, Id, Doc}) when is_list(Doc) ->
                             Header = jsx:encode([
                                                 {<<"index">>, [
                                                                 {<<"_index">>, Index},
                                                                 {<<"_type">>, Type},
                                                                 {<<"_id">>, Id}
                                                                 ]}]),
                             [Header, <<"\n">>, jsx:encode(Doc), <<"\n">>]
                     end, IndexTypeIdJsonTuples),
    erls_resource:post(Params, <<"/_bulk">>, [], [], iolist_to_binary(Body), Params#erls_params.http_client_options).


%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a query as "key:value" and sends
%% it to the Elastic Search server specified in Params.
%% @end
%%--------------------------------------------------------------------
-spec search(binary() | list(), list() | binary()) -> {ok, list()} | {error, any()}.
search(Index, Query) ->
    search(#erls_params{}, Index, <<>>, Query, []).

-spec search(binary() | list() | record(erls_params), binary() | list(), list() | binary()) -> {ok, list()} | {error, any()}.
search(Params, Index, Query) when is_record(Params, erls_params) ->
    search(Params, Index, <<>>, Query, []);
search(Index, Type, Query) ->
    search(#erls_params{}, Index, Type, Query, []). 

-spec search_limit(binary() | list(), binary(), list() | binary(), integer()) -> {ok, list()} | {error, any()}.
search_limit(Index, Type, Query, Limit) when is_integer(Limit) ->
    search(#erls_params{}, Index, Type, Query, [{<<"size">>, integer_to_list(Limit)}]).

-spec search(record(erls_params), list() | binary(), list() | binary(), list() | binary(), list()) -> {ok, list()} | {error, any()}.
search(Params, Index, Type, Query, Opts) when is_binary(Query) ->
    erls_resource:get(Params, filename:join([commas(Index), Type, <<"_search">>]), [], [{<<"q">>, Query}]++Opts, Params#erls_params.http_client_options);
search(Params, Index, Type, Query, Opts) ->
    erls_resource:post(Params, filename:join([commas(Index), Type, <<"_search">>]), [], Opts, jsx:encode(Query), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends
%% it to the default Elastic Search server on localhost:9100
%% @end
%%--------------------------------------------------------------------
-spec get_doc(binary(), binary(), binary()) -> {ok, list()} | {error, any()}.
get_doc(Index, Type, Id) ->
    get_doc(#erls_params{}, Index, Type, Id).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends
%% it to the Elastic Search server specified in Params.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec get_doc(record(erls_params), binary(), binary(), binary()) -> {ok, list()} | {error, any()}.
get_doc(Params, Index, Type, Id) ->
    erls_resource:get(Params, filename:join([Index, Type, Id]), [], [], Params#erls_params.http_client_options).

flush_index(Index) ->
    flush_index(#erls_params{}, Index).

flush_index(Params, Index) ->
    erls_resource:post(Params, filename:join([commas(Index), <<"_flush">>]), [], [], [], Params#erls_params.http_client_options).

flush_all() ->
    refresh_all(#erls_params{}).

flush_all(Params) ->
    erls_resource:post(Params, <<"_flush">>, [], [], [], Params#erls_params.http_client_options).

refresh_index(Index) ->
    refresh_index(#erls_params{}, Index).

refresh_index(Params, Index) ->
    erls_resource:post(Params, filename:join([commas(Index), <<"_refresh">>]), [], [], [], Params#erls_params.http_client_options).

refresh_all() ->
    refresh_all(#erls_params{}).

refresh_all(Params) ->
    erls_resource:post(Params, <<"_refresh">>, [], [], [], Params#erls_params.http_client_options).

delete_doc(Index, Type, Id) ->
    delete_doc(#erls_params{}, Index, Type, Id).

delete_doc(Params, Index, Type, Id) ->
    erls_resource:delete(Params, filename:join([Index, Type, Id]), [], [], Params#erls_params.http_client_options).

delete_doc_by_query(Index, Type, Query) ->
    delete_doc_by_query(#erls_params{}, Index, Type, Query).

delete_doc_by_query(Params, Index, Type, Query) ->
    erls_resource:delete(Params, filename:join([Index, Type]), [], [{<<"q">>, Query}], Params#erls_params.http_client_options).

delete_doc_by_query_doc(Index, Type, Doc) ->
    delete_doc_by_query_doc(#erls_params{}, Index, Type, Doc).

delete_doc_by_query_doc(Params, Index, any, Doc) ->
    erls_resource:delete(Params, filename:join([Index, <<"_query">>]), [], [], jsx:encode(Doc), Params#erls_params.http_client_options);

delete_doc_by_query_doc(Params, Index, Type, Doc) ->
    erls_resource:delete(Params, filename:join([Index, Type, <<"_query">>]), [], [], jsx:encode(Doc), Params#erls_params.http_client_options).

optimize_index(Index) ->
    optimize_index(#erls_params{}, Index).

optimize_index(Params, Index) ->
    erls_resource:post(Params, filename:join([commas(Index), <<"_optimize">>]), [], [], [], Params#erls_params.http_client_options).

%%% Internal functions

-spec commas(list(binary()) | binary()) -> binary().
commas(Bin) when is_binary(Bin) ->
    Bin;
commas([]) ->
    <<>>;
commas([H | T]) ->
    << H/binary, << <<",", B/binary>> || B <- T >> >>.

