APPNAME=webserver
REBAR=`which rebar3 || echo ./rebar3`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

ct:
	ct_run -dir ct/ -pa  ebin/ -pa include/ -pa ./_build/default/lib/*/ebin --suite test_SUITE

run:
	erl -config config/sys -pa ./ebin -pa ./_build/default/lib/*/ebin -sname $(APPNAME)@localhost -s $(APPNAME) -s sync	


.PHONY: all, deps, compile, ct
