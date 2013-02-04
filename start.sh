#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname node1 -boot start_sasl -s reloader -s enouk_cowboy_app
