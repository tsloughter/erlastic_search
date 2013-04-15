-module(basic_SUITE).

-export([all/0
        ,groups/0
        ,init_per_group/2
        ,end_per_group/2]).
-export([index_id/1
        ,index_no_id/1
        ,search/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, index_access}].

groups() ->
    [{index_access, [], [index_id
                        ,index_no_id
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

index_no_id(Config) ->
    IndexName = ?config(index_name, Config),
    {ok, _} = erlastic_search:index_doc(IndexName, <<"type_1">>, [{<<"hello">>, <<"there">>}]).

search(Config) ->
    IndexName = ?config(index_name, Config),
    {ok, _} = erlastic_search:search(IndexName, <<"hello:there">>).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

compare_json(J1, J2) ->
    sort(jsx:decode(J1)) == sort(jsx:decode(J2)).

sort(L = [X | _]) when is_list(X) ->
    [sort(Y) || Y <- L];
sort(L = [X | _]) when is_tuple(X)->
    lists:keysort(1, L).

create_random_name(Name) ->
    random:seed(erlang:now()),
    <<Name/binary, (list_to_binary(erlang:integer_to_list(random:uniform(1000000))))/binary>>.
