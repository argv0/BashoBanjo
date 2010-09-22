

all: deps compile

compile:
	./rebar compile
deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps

run: all
	erl \
	-name phone1@127.0.0.1 \
	-setcookie phone \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-eval "application:start(sasl)" \
	-eval "application:start(crypto)" \
	-eval "application:start(webmachine)" \
	-eval "application:start(riak_core)" \
	-eval "application:start(riakophone)"

run2: all
	erl \
	-name `whoami`@127.0.0.1 \
	-setcookie phone \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-eval "application:start(sasl)" \
	-eval "application:start(crypto)" \
	-eval "application:start(webmachine)" \
	-eval "application:start(riak_core)" \
	-eval "application:start(riakophone)" \
	-eval "riak_core_gossip:send_ring('phone1@127.0.0.1', node())"
