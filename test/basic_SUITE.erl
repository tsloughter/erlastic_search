-module(basic_SUITE).

-export([all/0
        ,groups/0
        ,init_per_group/2
        ,end_per_group/2]).
-export([index_id/1
        ,index_encoded_id/1
        ,index_no_id/1
        ,bulk_index_id/1
        ,search/1
        ,index_template_mapping/1]).

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
                        ,search
                        ,index_template_mapping]}].

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

%% @doc Creates an index template, and tests that it exists with the correct settings and mapping
index_template_mapping(_Config) ->
    %% First we create the index template
    TemplatePath = create_random_name(<<"_template/test_template_">>),
    {ok, _} = erlastic_search:create_index(TemplatePath, template_mapping_json()),

    %% When searching for an index template, we should only need the name, not the full path
    %% The get_templates/1 fun should handle creating the correct path
    [_, TemplateName] = binary:split(TemplatePath, <<"_template/">>),
    {ok, [{TemplateName, ActualTemplateSettingsAndMapping1}]} = erlastic_search:get_templates(TemplateName),

    %% Also make sure that the get_templates/0 fun returns the same thing as get_templates/1 with the current state
    {ok, [{TemplateName, ActualTemplateSettingsAndMapping1}]} = erlastic_search:get_templates(),

    %% The order and aliases are generated automatically, both of which will be default, we will not compare
    ActualTemplateSettingsAndMapping2 = proplists:delete(<<"order">>, ActualTemplateSettingsAndMapping1),
    ActualTemplateSettingsAndMapping3 = proplists:delete(<<"aliases">>, ActualTemplateSettingsAndMapping2),

    ExpectedTemplateMappingAndSettings = lists:sort(jsx:decode(jsx:encode(template_mapping_json()))),
    ActualTemplateSettingsAndMapping = lists:sort(ActualTemplateSettingsAndMapping3),

    ExpectedTemplateMappingAndSettings = ActualTemplateSettingsAndMapping,

    %% Remove this index template
    erlastic_search:delete_index(TemplatePath).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_random_name(Name) ->
    random:seed(os:timestamp()),
    <<Name/binary, (list_to_binary(erlang:integer_to_list(random:uniform(1000000))))/binary>>.

%% @doc Uses the example template settings from
%% https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-templates.html#indices-templates
template_mapping_json() ->
    #{
        <<"mappings">> => #{
            <<"test_type">> => #{
                <<"_source">> => #{
                    <<"enabled">> => false
                },
                <<"properties">> => #{
                    <<"host_name">> => #{
                        <<"type">> => <<"string">>
                    },
                    <<"created_at">> => #{
                        <<"type">> => <<"date">>,
                        <<"format">> => <<"EEE MMM dd HH:mm:ss Z YYYY">>
                    }
                }
            }
        },
        <<"settings">> => #{
            <<"index">> => #{
                <<"number_of_shards">> => <<"1">>
            }
        },
        <<"template">> => <<"test_template-*">>
    }.
