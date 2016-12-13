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
        ,create_index/3
        ,stats_index/0
        ,stats_index/1
        ,stats_index/2
        ,nodes_info/0
        ,nodes_info/1
        ,nodes_info/2
        ,put_mapping/3
        ,put_mapping/4
        ,get_mapping/0
        ,get_mapping/1
        ,get_mapping/2
        ,get_mapping/3
        ,get_settings/0
        ,get_settings/1
        ,get_settings/2
        ,index_doc/3
        ,index_doc/4
        ,index_doc_with_opts/5
        ,index_doc_with_id/4
        ,index_doc_with_id/5
        ,index_doc_with_id_opts/6
        ,upsert_doc/4
        ,upsert_doc/5
        ,upsert_doc_opts/6
        ,bulk_index_docs/2
        ,search/2
        ,search/3
        ,search/5
        ,search_limit/4
        ,search_scroll/4
        ,search_scroll/1
        ,multi_search/2
        ,get_doc/3
        ,get_doc/4
        ,get_doc_opts/5
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
        ,delete_index/1
        ,delete_index/2
        ,index_exists/1
        ,index_exists/2
        ,optimize_index/1
        ,optimize_index/2
        ,percolator_add/3
        ,percolator_add/4
        ,percolator_del/2
        ,percolator_del/3
        ,percolate/3
        ,percolate/4]).

-include("erlastic_search.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index to create and sends the request to
%% Elasticsearch, the default settings on localhost.
%% @end
%%--------------------------------------------------------------------
-spec create_index(binary()) -> {ok, erlastic_success_result()} | {error, any()}.
create_index(Index) ->
    create_index(#erls_params{}, Index, <<>>).

%%--------------------------------------------------------------------
%% @doc
%% Takes the name of an index and the record describing the servers
%% details to create and sends the request to Elasticsearch; or else
%% takes the name of the index to create and a body to use in the request; and
%% creates that index using the default settings on localhost.
%% (see the doc at https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html)
%% @end
%%--------------------------------------------------------------------
-spec create_index(#erls_params{}, binary()) -> {ok, erlastic_success_result()} | {error, any()}.
create_index(#erls_params{} = Params, Index) when is_binary(Index) ->
    create_index(Params, Index, <<>>);
create_index(Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    create_index(#erls_params{}, Index, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Takes a record describing the servers details, an index name, and a request body, and creates that index
%% (see the doc at https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html)
%% @end
%%--------------------------------------------------------------------
-spec create_index(#erls_params{}, binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
create_index(Params, Index, Doc) when is_binary(Index), (is_binary(Doc) orelse is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    erls_resource:put(Params, Index, [], [], maybe_encode_doc(Doc), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes an optional list of index names and the record describing the servers
%% details to read the stats for these index.
%% If no index in supplied then stats for all indices are returned.
%% https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-stats.html
%% @end
%%--------------------------------------------------------------------

stats_index() ->
    stats_index(#erls_params{}).

stats_index(Params) ->
    stats_index(Params, []).

stats_index(Params, Index) ->
    erls_resource:get(Params, filename:join(commas(Index),"_stats"), [], [],
                      Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes an optional list of node names and the record describing the servers
%% details to read the infos for these nodes.
%% If no index in supplied then stats for all indices are returned.
%% https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-info.html
%% @end
%%--------------------------------------------------------------------

-spec nodes_info() -> {ok, erlastic_success_result()} | {error, any()}.
nodes_info() ->
    nodes_info(#erls_params{}).

-spec nodes_info(#erls_params{}) -> {ok, erlastic_success_result()} | {error, any()}.
nodes_info(#erls_params{} = Params) ->
    nodes_info(Params, []).

-spec nodes_info(#erls_params{}, [binary()]) -> {ok, erlastic_success_result()} | {error, any()}.
nodes_info(#erls_params{} = Params, Nodes) when erlang:is_list(Nodes) ->
    erls_resource:get(Params, filename:join("_nodes", commas(Nodes)), [], [],
                      Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Insert a mapping into an Elasticsearch index
%% @end
%%--------------------------------------------------------------------
-spec put_mapping(binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
put_mapping(Index, Type, Doc) ->
    put_mapping(#erls_params{}, Index, Type, Doc).

-spec put_mapping(#erls_params{}, binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
put_mapping(Params, Index, Type, Doc) ->
    erls_resource:put(Params, filename:join([Index, Type, "_mapping"]), [], [], maybe_encode_doc(Doc), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the mapping for all indices, using default server parameters
%% For all flavours of `get_mapping/*' functions, see the doc at
%% https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-mapping.html
%% @end
%%--------------------------------------------------------------------
-spec get_mapping() -> {ok, erlastic_success_result()} | {error, any()}.
get_mapping() ->
    get_mapping(#erls_params{}, <<"_all">>, <<"_all">>).

%%--------------------------------------------------------------------
%% @doc
%% If passed server parameters, retrieves the mapping for all indices on that
%% server; if passed an index name, retrieves the mapping for that index using
%% default server parameters
%% @end
%%--------------------------------------------------------------------
-spec get_mapping(#erls_params{} | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_mapping(#erls_params{} = Params) ->
    get_mapping(Params, <<"_all">>, <<"_all">>);
get_mapping(Index) when is_binary(Index) ->
    get_mapping(#erls_params{}, Index, <<"_all">>).

%%--------------------------------------------------------------------
%% @doc
%% If passed server parameters and an index name, retrieves the mapping for
%% that index on that server; if passed an index name and a type name,
%% retrieves the mapping for that specific type
%% @end
%%--------------------------------------------------------------------
-spec get_mapping(#erls_params{} | binary(), binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_mapping(#erls_params{} = Params, Index) when is_binary(Index) ->
    get_mapping(Params, Index, <<"_all">>);
get_mapping(Index, Type) when is_binary(Index), is_binary(Type) ->
    get_mapping(#erls_params{}, Index, Type).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the mapping for the given index and type, using the provided
%% server parameters
%% @end
%%--------------------------------------------------------------------
-spec get_mapping(#erls_params{}, binary(), binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_mapping(#erls_params{} = Params, Index, Type) when is_binary(Index), is_binary(Type) ->
    erls_resource:get(Params, filename:join([Index, <<"_mapping">>, Type]), [], [], [], Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the settings for all indices, using default server parameters
%% For all flavours of `get_settings/*' functions, see the doc at
%% https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-settings.html
%% @end
%%--------------------------------------------------------------------
-spec get_settings() -> {ok, erlastic_success_result()} | {error, any()}.
get_settings() ->
    get_settings(#erls_params{}, <<"_all">>).

%%--------------------------------------------------------------------
%% @doc
%% If passed server parameters, retrieves the settings for all indices on that
%% server; if passed an index name, retrieves the settings for that index using
%% default server parameters
%% @end
%%--------------------------------------------------------------------
-spec get_settings(#erls_params{} | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_settings(#erls_params{} = Params) ->
    get_settings(Params, <<"_all">>);
get_settings(Index) when is_binary(Index) ->
    get_settings(#erls_params{}, Index).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the settings for the given index, using the provided server
%% parameters
%% @end
%%--------------------------------------------------------------------
-spec get_settings(#erls_params{}, binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_settings(#erls_params{} = Params, Index) when is_binary(Index) ->
    erls_resource:get(Params, filename:join([Index, <<"_settings">>]), [], [], [], Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in
%% Erlang terms, converts the document to a string and passes to the
%% default server. Elasticsearch provides the doc with an id.
%% @end
%%--------------------------------------------------------------------
-spec index_doc(binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc(Index, Type, Doc) ->
    index_doc(#erls_params{}, Index, Type, Doc).

-spec index_doc(#erls_params{}, binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc(Params, Index, Type, Doc) ->
    index_doc_with_opts(Params, Index, Type, Doc, []).

-spec index_doc_with_opts(#erls_params{}, binary(), binary(), erlastic_json() | binary(), list()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc_with_opts(Params, Index, Type, Doc, Opts) when is_list(Opts) ->
    erls_resource:post(Params, filename:join(Index, Type), [], Opts, maybe_encode_doc(Doc), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a Json document described in
%% Erlang terms, converts the document to a string after adding the _id field
%% and passes to the default server.
%% @end
%%--------------------------------------------------------------------
-spec index_doc_with_id(binary(), binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc_with_id(Index, Type, Id, Doc) ->
    index_doc_with_id_opts(#erls_params{}, Index, Type, Id, Doc, []).

-spec index_doc_with_id(#erls_params{}, binary(), binary(), binary(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc_with_id(Params, Index, Type, Id, Doc) ->
    index_doc_with_id_opts(Params, Index, Type, Id, Doc, []).

-spec index_doc_with_id_opts(#erls_params{}, binary(), binary(), binary(), erlastic_json() | binary(), list()) -> {ok, erlastic_success_result()} | {error, any()}.
index_doc_with_id_opts(Params, Index, Type, undefined, Doc, Opts) ->
    index_doc_with_opts(Params, Index, Type, Doc, Opts);
index_doc_with_id_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Opts) ->
    erls_resource:post(Params, filename:join([Index, Type, Id]), [], Opts, maybe_encode_doc(Doc), Params#erls_params.http_client_options).


%%--------------------------------------------------------------------
%% @doc Insert the document, or replacing it when it already exists (upsert)
%% (https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)
%% --------------------------------------------------------------------

-spec upsert_doc(binary(), binary(), binary(), erlastic_json()) -> {ok, erlastic_success_result()} | {error, any()}.
upsert_doc(Index, Type, Id, Doc) ->
    upsert_doc_opts(#erls_params{}, Index, Type, Id, Doc, []).

-spec upsert_doc(#erls_params{}, binary(), binary(), binary(), erlastic_json()) -> {ok, erlastic_success_result()} | {error, any()}.
upsert_doc(Params, Index, Type, Id, Doc) ->
    upsert_doc_opts(Params, Index, Type, Id, Doc, []).

-spec upsert_doc_opts(#erls_params{}, binary(), binary(), binary(), erlastic_json(), list()) -> {ok, erlastic_success_result()} | {error, any()}.
upsert_doc_opts(Params, Index, Type, Id, Doc, Opts) when is_list(Opts), (is_list(Doc) orelse is_tuple(Doc) orelse is_map(Doc)) ->
    DocBin = erls_json:encode(Doc),
    %% we cannot use erls_json to generate this, see the doc string for `erls_json:encode/1'
    Body = <<"{\"doc_as_upsert\":true,\"doc\":", DocBin/binary, "}">>,
    erls_resource:post(Params, filename:join([Index, Type, Id, "_update"]), [], Opts,
                       Body,
                       Params#erls_params.http_client_options).

%% Documents is [ {Index, Type, Id, Json}, {Index, Type, Id, HeaderInformation, Json}... ]
-spec bulk_index_docs(#erls_params{}, list()) -> {ok, list()} | {error, any()}.
bulk_index_docs(Params, IndexTypeIdJsonTuples) ->
     Body = lists:map(fun
          Build({Index, Type, Id, Doc}) ->
               Build({Index, Type, Id, [], Doc});
          Build({Index, Type, Id, HeaderInformation, Doc}) ->
               Header = bulk_index_docs_header(Index, Type, Id, HeaderInformation),
               [ Header, <<"\n">>, maybe_encode_doc(Doc), <<"\n">> ]
     end, IndexTypeIdJsonTuples),
     erls_resource:post(Params, <<"/_bulk">>, [], [], iolist_to_binary(Body), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a query as "key:value" and sends
%% it to the Elasticsearch server specified in Params.
%% @end
%%--------------------------------------------------------------------
-spec search(binary() | list(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
search(Index, Query) ->
    search(#erls_params{}, Index, <<>>, Query, []).

-spec search(binary() | list() | #erls_params{}, binary() | list(), erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
search(Params, Index, Query) when is_record(Params, erls_params) ->
    search(Params, Index, <<>>, Query, []);
search(Index, Type, Query) ->
    search(#erls_params{}, Index, Type, Query, []).

-spec search_limit(binary() | list(), binary(), erlastic_json() | binary(), integer()) -> {ok, erlastic_success_result()} | {error, any()}.
search_limit(Index, Type, Query, Limit) when is_integer(Limit) ->
    search(#erls_params{}, Index, Type, Query, [{<<"size">>, integer_to_list(Limit)}]).

%%--------------------------------------------------------------------
%% @doc
%% search_scroll/4 -- Takes the index, type name and search query
%% sends it to the Elasticsearch server specified in Params.
%% Returns search results along with scroll id which can be passed
%% to search_scroll/1 to get next set of search results
%% @end
%%--------------------------------------------------------------------
-spec search_scroll(binary() | list(), binary(), erlastic_json() | binary(), list()) -> {ok, erlastic_success_result()} | {error, any()}.
search_scroll(Index, Type, Query, Timeout) ->
    search(#erls_params{}, Index, Type, Query, [{<<"scroll">>, list_to_binary(Timeout)}]).

-spec search_scroll(erlastic_json() | binary()) -> {ok, erlastic_success_result()} | {error, any()}.
search_scroll(Query) ->
     Params = #erls_params{},
     erls_resource:post(Params, filename:join([<<"_search">>, <<"scroll">>]), [], [], erls_json:encode(Query), Params#erls_params.http_client_options).

-spec search(#erls_params{}, list() | binary(), list() | binary(), erlastic_json() | binary(), list()) -> {ok, erlastic_success_result()} | {error, any()}.
search(Params, Index, Type, Query, Opts) when is_binary(Query) ->
    erls_resource:get(Params, filename:join([commas(Index), Type, <<"_search">>]), [], [{<<"q">>, Query}]++Opts, Params#erls_params.http_client_options);
search(Params, Index, Type, Query, Opts) ->
    erls_resource:post(Params, filename:join([commas(Index), Type, <<"_search">>]), [], Opts, erls_json:encode(Query), Params#erls_params.http_client_options).

-spec multi_search(#erls_params{}, list({HeaderInformation :: headers(), SearchRequest :: erlastic_json() | binary()})) -> {ok, ResultJson :: erlastic_success_result()} | {error, Reason :: any()}.
multi_search(Params, HeaderJsonTuples) ->
    Body = lists:map(fun({HeaderInformation, SearchRequest}) ->
        [ jsx:encode(HeaderInformation), <<"\n">>, maybe_encode_doc(SearchRequest), <<"\n">> ]
    end, HeaderJsonTuples),
    erls_resource:get(Params, <<"/_msearch">>, [], [], iolist_to_binary(Body), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends
%% it to the default Elasticsearch server on localhost:9100
%% @end
%%--------------------------------------------------------------------
-spec get_doc(binary(), binary(), binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_doc(Index, Type, Id) ->
    get_doc(#erls_params{}, Index, Type, Id).

%%--------------------------------------------------------------------
%% @doc
%% Takes the index and type name and a doc id and sends
%% it to the Elasticsearch server specified in Params.
%% @end
%%--------------------------------------------------------------------
-spec get_doc(#erls_params{}, binary(), binary(), binary()) -> {ok, erlastic_success_result()} | {error, any()}.
get_doc(Params, Index, Type, Id) ->
    get_doc_opts(Params, Index, Type, Id, []).

-spec get_doc_opts(#erls_params{}, binary(), binary(), binary(), list()) -> {ok, erlastic_success_result()}
                                                                          | {error, any()}.
get_doc_opts(Params, Index, Type, Id, Opts) ->
    erls_resource:get(Params, filename:join([Index, Type, Id]), [], Opts, Params#erls_params.http_client_options).

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
    erls_resource:delete(Params, filename:join([Index, <<"_query">>]), [], [], erls_json:encode(Doc), Params#erls_params.http_client_options);

delete_doc_by_query_doc(Params, Index, Type, Doc) ->
    erls_resource:delete(Params, filename:join([Index, Type, <<"_query">>]), [], [], erls_json:encode(Doc), Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Delete existing index
%% @end
%%--------------------------------------------------------------------
delete_index(Index) ->
    delete_index(#erls_params{}, Index).

delete_index(Params, Index) ->
    erls_resource:delete(Params, Index, [], [], [],
                         Params#erls_params.http_client_options).

%%--------------------------------------------------------------------
%% @doc
%% Tests if a given index exists
%% See https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-exists.html
%% @end
%%--------------------------------------------------------------------
-spec index_exists(binary()) -> {ok, boolean()} | {error, any()}.
index_exists(Index) ->
    index_exists(#erls_params{}, Index).

-spec index_exists(#erls_params{}, binary()) -> {ok, boolean()} | {error, any()}.
index_exists(Params, Index) ->
    case erls_resource:head(Params, Index, [], [], Params#erls_params.http_client_options) of
        ok -> {ok, true};
        {error, 404} -> {ok, false};
        {error, _Else} = Error -> Error
    end.

optimize_index(Index) ->
    optimize_index(#erls_params{}, Index).

optimize_index(Params, Index) ->
    erls_resource:post(Params, filename:join([commas(Index), <<"_optimize">>]), [], [], [], Params#erls_params.http_client_options).

percolator_add(Index, Name, Query) ->
    percolator_add(#erls_params{}, Index, Name, Query).

percolator_add(Params, Index, Name, Query) ->
    erls_resource:put(Params, filename:join([<<"_percolator">>, commas(Index), Name]), [], [], erls_json:encode(Query), Params#erls_params.http_client_options).

percolator_del(Index, Name) ->
    percolator_del(#erls_params{}, Index, Name).

percolator_del(Params, Index, Name) ->
    erls_resource:delete(Params, filename:join([<<"_percolator">>, commas(Index), Name]), [], [], [], Params#erls_params.http_client_options).

percolate(Index, Type, Doc) ->
    percolate(#erls_params{}, Index, Type, Doc).

percolate(Params, Index, Type, Doc) ->
    erls_resource:get(Params, filename:join([commas(Index), Type, <<"_percolate">>]), [], [], erls_json:encode(Doc), Params#erls_params.http_client_options).

%%% Internal functions

-spec commas(list(binary()) | binary()) -> binary().
commas(Bin) when is_binary(Bin) ->
    Bin;
commas([]) ->
    <<>>;
commas([H | T]) ->
    << H/binary, << <<",", B/binary>> || B <- T >>/binary >>.

-spec bulk_index_docs_header(binary(), binary(), binary(), list()) -> binary().
bulk_index_docs_header(Index, Type, Id, HeaderInformation) ->
    IndexHeaderJson1 = [
        {<<"_index">>, Index}
        ,{<<"_type">>, Type}
        | HeaderInformation
    ],

    IndexHeaderJson2 = case Id =:= undefined of
        true ->  IndexHeaderJson1;
        false -> [ {<<"_id">>, Id} | IndexHeaderJson1]
    end,

    %% we cannot use erls_json to generate this, see the doc string for `erls_json:encode/1'
    jsx:encode([{<<"index">>, IndexHeaderJson2}]).

-spec maybe_encode_doc(binary() | erlastic_json()) -> binary().
maybe_encode_doc(Bin) when is_binary(Bin) -> Bin;
maybe_encode_doc(Doc) when is_list(Doc); is_tuple(Doc); is_map(Doc) -> erls_json:encode(Doc).
