ErlasticSearch
======================================

An Erlang client for [ElasticSearch](http://www.elasticsearch.org/).

Build and Run
-------------

```shell
$ make

$ make shell
./rebar compile
==> mimetypes (compile)
==> hackney (compile)
==> jsx (compile)
==> erlastic_search (compile)
exec erl +K true -pa ebin -env ERL_LIBS deps -name erlastic@127.0.0.1 -s erlastic_search_app start_deps
Erlang R16B (erts-5.10.1) [source-05f1189] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:true]

Eshell V5.10.1  (abort with ^G)
(erlastic@127.0.0.1)1> erlastic_search:create_index(<<"index_name">>).
{ok,200, ...}
(erlastic@127.0.0.1)2> erlastic_search:index_doc(<<"index_name">>, <<"type">>, [{<<"key1">>, <<"value1">>}]).
{ok,201, ...}
(erlastic@127.0.0.1)3> erlastic_search:index_doc_with_id(<<"index_name">>, <<"type">>, <<"id1">>, [{<<"key1">>, <<"value1">>}]).
{ok,201, ...}
(erlastic@127.0.0.1)4> erlastic_search:search(<<"index_name">>, <<"type">>, <<"key1:value1">>)).
{ok,200, ...
    <<"{\"took\":27,\"timed_out\":false,\"_shards\":{\"total\":5,\"successful\":5,\"failed\":0},\"hits\":{\"total\":2,\"max_score\":0.30685282,\"hits\":[{\"_index\":\"index_name\",\"_type\":\"type\",\"_id\":\"id1\",\"_score\":0.30685282, \"_source\" : {\"key1\":\"value1\"}},{\"_index\":\"index_name\",\"_type\":\"type\",\"_id\":\"MMNcfNHUQyeizDkniZD2bg\",\"_score\":0.30685282, \"_source\" : {\"key1\":\"value1\"}}]}}">>
    ...}}
```
