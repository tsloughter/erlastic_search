ErlasticSearch
======================================

An Erlang client for [Elasticsearch](https://www.elastic.co/products/elasticsearch).

Build and Run
-------------

```shell
$ ./rebar3 shell
==> mimetypes (compile)
==> hackney (compile)
==> jsx (compile)
==> erlastic_search (compile)
exec erl +K true -pa ebin -env ERL_LIBS deps -name erlastic@127.0.0.1 -s erlastic_search_app start_deps
Erlang R16B (erts-5.10.1) [source-05f1189] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:true]

Eshell V5.10.1  (abort with ^G)
(erlastic@127.0.0.1)1> erlastic_search:create_index(<<"index_name">>).
{ok, [{<<"ok">>,true},{<<"acknowledged">>,true}]}
(erlastic@127.0.0.1)2> erlastic_search:index_doc(<<"index_name">>, <<"type">>, [{<<"key1">>, <<"value1">>}]).
{ok,[{<<"ok">>,true},
     {<<"_index">>,<<"index_name">>},
     {<<"_type">>,<<"type">>},
     {<<"_id">>,<<"T-EzM_yeTkOEHPL9cN5B2g">>},
     {<<"_version">>,1}]}
(erlastic@127.0.0.1)3> erlastic_search:index_doc_with_id(<<"index_name">>, <<"type">>, <<"id1">>, [{<<"key1">>, <<"value1">>}]).
{ok,[{<<"ok">>,true},
     {<<"_index">>,<<"index_name">>},
     {<<"_type">>,<<"type">>},
     {<<"_id">>,<<"id1">>},
     {<<"_version">>,2}]}
(erlastic@127.0.0.1)4> erlastic_search:search(<<"index_name">>, <<"type">>, <<"key1:value1">>).
{ok,[{<<"took">>,6},
     {<<"timed_out">>,false},
     {<<"_shards">>,
      [{<<"total">>,5},{<<"successful">>,5},{<<"failed">>,0}]},
     {<<"hits">>,
      [{<<"total">>,3},
       {<<"max_score">>,0.30685282},
       {<<"hits">>,
        [[{<<"_index">>,<<"index_name">>},
          {<<"_type">>,<<"type">>},
          {<<"_id">>,<<"T-EzM_yeTkOEHPL9cN5B2g">>},
          {<<"_score">>,0.30685282},
          {<<"_source">>,[{<<"key1">>,<<"value1">>}]}],
         [{<<"_index">>,<<"index_name">>},
          {<<"_type">>,<<"type">>},
          {<<"_id">>,<<"id1">>},
          {<<"_score">>,0.30685282},
          {<<"_source">>,[{<<"key1">>,<<"value1">>}]}],
         [{<<"_index">>,<<"index_name">>},
          {<<"_type">>,<<"type">>},
          {<<"_id">>,<<"MMNcfNHUQyeizDkniZD2bg">>},
          {<<"_score">>,0.30685282},
          {<<"_source">>,[{<<"key1">>,<<"value1">>}]}]]}]}]}
```

Testing
-------

First start a local Elasticsearch:

```bash
$ bin/elasticsearch
```

Run Common Test:

```bash
$ ./rebar3 ct
```

Using another JSON library than `jsx`
-------------------------------------

By default, we assume all the JSON erlang objects passed to us are in
[`jsx`](https://github.com/talentdeficit/jsx)'s representation.
And similarly, all of Elasticsearch's replies will be decoded with `jsx`.

However, you might already be using another JSON library in your project, which
might encode and decode JSONs from and to a different erlang representation.
For example, [`jiffy`](https://github.com/davisp/jiffy):
```
1> SimpleJson = <<"{\"key\":\"value\"}">>.
<<"{\"key\":\"value\"}">>
2> jiffy:decode(SimpleJson).
{[{<<"key">>,<<"value">>}]}
3> jsx:decode(SimpleJson).
[{<<"key">>,<<"value">>}]
```
In that case, you probably want `erlastic_search` to use your JSON
representation of choice instead of `jsx`'s.

You can do so by defining the `ERLASTIC_SEARCH_JSON_MODULE` environment
variable when compiling `erlastic_search`, for example:
```shell
export ERLASTIC_SEARCH_JSON_MODULE=jiffy
rebar compile
```

The only constraint is that `ERLASTIC_SEARCH_JSON_MODULE` should be the name
of a module, in your path, that defines the two following callbacks:

```erlang
-callback encode(erlastic_json()) -> binary().
-callback decode(binary()) -> erlastic_json().
```
where `erlastic_json()` is a type mapping to your JSON representation of choice.
