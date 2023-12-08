#!/bin/sh

cd /webserver
erl -noshell -config config/sys -pa ./ebin -pa ./_build/default/lib/*/ebin -sname webserver@localhost -s webserver -s sync