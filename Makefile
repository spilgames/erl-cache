.PHONY: clean test docs benchmark docsclean go quick dialyzer

all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile
	rebar skip_deps=true xref

quick:
	rebar skip_deps=true compile
	rebar skip_deps=true xref

clean:
	rebar clean
	rm -f erl_cache

test: compile
	rebar skip_deps=true eunit

docs: docsclean
	ln -s . doc/doc
	rebar skip_deps=true doc

docsclean:
	rm -f doc/*.html doc/*.css doc/erlang.png doc/edoc-info doc/doc

go:
	erl -name erl_cache -pa deps/*/ebin -pa ebin/ -s erl_cache start ${EXTRA_ARGS}

dialyzer:
	dialyzer -c ebin/ -Wunmatched_returns -Werror_handling -Wrace_conditions

erl_cache:
	REBAR_BENCH=1 rebar get-deps compile
	REBAR_BENCH=1 rebar escriptize skip_deps=true

benchmark: erl_cache
	./erl_cache priv/bench.conf
