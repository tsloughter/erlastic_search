#!/bin/sh
cd `dirname $0`
exec erl -name erlastic@127.0.0.1 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config sys.config -s erlastic_search

