

all: deps compile

compile:
	./rebar compile
deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps

run: compile 
	erl \
	-name phone1@127.0.0.1 \
	-setcookie phone \
	-pa deps/*/ebin \
	-pa apps/*/ebin \
	-eval "application:start(sasl)" \
	-eval "application:start(crypto)" \
	-eval "application:start(webmachine)" \
	-eval "application:start(riak_core)" \
	-eval "application:start(riak_music)"

rel:
	./rebar compile generate
	chmod 755 rel/basho_banjo/bin/banjo

rellink:
	$(foreach app,$(wildcard apps/*), rm -rf rel/basho_banjo/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/basho_banjo/lib;)
	$(foreach dep,$(wildcard deps/*), rm -rf rel/basho_banjo/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) rel/basho_banjo/lib;)

