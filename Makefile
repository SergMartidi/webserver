APPNAME=webserver
DOCKERNAME=webserver
DATABASENAME=erl
REBAR=`which rebar3 || echo ./rebar3`
# REBAR=rebar3

all: deps compile

deps:
	@( $(REBAR) get-deps )

create_db:
	sudo su - postgres -c "createdb $(DATABASENAME)"

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

ct:
	ct_run -dir ct/ -pa  ebin/ -pa include/ -pa ./_build/default/lib/*/ebin --suite test_SUITE

run:
	erl -config config/sys -pa ./ebin -pa ./_build/default/lib/*/ebin -sname $(APPNAME)@localhost -s $(APPNAME) -s sync

docker:
	docker build -t $(DOCKERNAME) .

docker_run:
	docker run $(DOCKERNAME)

.PHONY: all, deps, compile, ct, docker, docker_run
