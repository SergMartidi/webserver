APPNAME=webserver
DOCKERNAME=webserver
DATABASENAME=erl
USERSTAB=users
REBAR=`which rebar3 || echo ./rebar3`
# REBAR=rebar3

all: deps compile

deps:
	@( $(REBAR) get-deps )

create_db:
	sudo su - postgres -c "createdb $(DATABASENAME)"
	psql -d $(DATABASENAME) -c "DROP TABLE IF EXISTS $(USERSTAB);"
	psql -d $(DATABASENAME) -c "CREATE TABLE $(USERSTAB) ( \
		id serial NOT NULL PRIMARY KEY, \
		email varchar(256) NOT NULL, \
		pass varchar(64) NOT NULL, \
		fname varchar(256), \
		lname varchar(256), \
		active boolean NOT NULL DEFAULT FALSE, \
		created_at timestamp DEFAULT current_timestamp, \
		updated_at timestamp DEFAULT NULL, \
		deleted_at timestamp DEFAULT NULL \
	);"
	psql -d $(DATABASENAME) -c "CREATE INDEX users_email_active_idx ON $(USERSTAB)(email, active);"
	psql -d $(DATABASENAME) -c "CREATE INDEX users_email_pass_active_idx ON $(USERSTAB)(email, pass, active);"

clear_db:
	psql -d $(DATABASENAME) -c "DELETE FROM $(USERSTAB) *;"

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
	docker run --network "host" $(DOCKERNAME)

.PHONY: all, deps, compile, ct, docker, docker_run
