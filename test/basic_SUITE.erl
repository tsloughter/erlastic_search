-module(basic_SUITE).

-export([all/0
        ,groups/0
        ,init_per_group/2
        ,end_per_group/2]).
-export([index_id/1
        ,index_encoded_id/1
        ,index_no_id/1
        ,bulk_index_id/1
        ,search/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlastic_search.hrl").

all() ->
    [{group, index_access}].

groups() ->
    [{index_access, [], [index_id
                        ,index_encoded_id
                        ,index_no_id
                        ,bulk_index_id
                        ,search]}].

init_per_group(index_access, Config) ->
    erlastic_search_app:start_deps(),

    IndexName = create_random_name(<<"es_index_name_">>),
    {ok, _} = erlastic_search:create_index(IndexName),

    [{index_name, IndexName} | Config].


end_per_group(index_access, _Config) ->
    ok.

index_id(Config) ->
    IndexName = ?config(index_name, Config),
    Id = create_random_name(<<"es_id_">>),
    {ok, _} = erlastic_search:index_doc_with_id(IndexName, <<"type_1">>, Id, [{<<"hello">>, <<"there">>}]).

index_encoded_id(Config) ->
    IndexName = ?config(index_name, Config),
    Id = create_random_name(<<"es_id_">>),
    {ok, _} = erlastic_search:index_doc_with_id(IndexName, <<"type_1">>, Id, erls_json:encode([{<<"hello">>, <<"there">>}])).

index_no_id(Config) ->
    IndexName = ?config(index_name, Config),
    {ok, _} = erlastic_search:index_doc(IndexName, <<"type_1">>, [{<<"hello">>, <<"there">>}]).

bulk_index_id(Config) ->
    IndexName = ?config(index_name, Config),
    Id = create_random_name(<<"es_id_">>),
    Doc = {<<"how">>, <<"you_doing">>}, %% in Joey Tribbiani voice
    Items = [{IndexName, <<"type_1">>, Id, [Doc]}],
    {ok, _} = erlastic_search:bulk_index_docs(#erls_params{}, Items),
    {ok, _} = erlastic_search:flush_index(IndexName),
    {ok, Resp} = erlastic_search:search(IndexName, <<"how:you_doing">>),
    {<<"hits">>, Hits} = lists:keyfind(<<"hits">>, 1, Resp),
    {<<"hits">>, Hits1} = lists:keyfind(<<"hits">>, 1, Hits),
    F = fun(Item) ->
                {<<"_id">>, AId} = lists:keyfind(<<"_id">>, 1, Item),
                AId == Id
        end,
    [_] = lists:filter(F, Hits1).

search(Config) ->
    IndexName = ?config(index_name, Config),
    {ok, _} = erlastic_search:search(IndexName, <<"hello:there">>).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_random_name(Name) ->
    random:seed(os:timestamp()),
    <<Name/binary, (list_to_binary(erlang:integer_to_list(random:uniform(1000000))))/binary>>.
