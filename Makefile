REBAR ?= rebar3
ifneq ($(wildcard rebar),)
	REBAR := ./rebar3
endif

.PHONY: clean test docs docsclean go compile get-deps dialyzer

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile xref

clean:
	$(REBAR) clean
	rm -fr _build

test: compile
	$(REBAR) eunit --cover

docs: docsclean
	$(REBAR) edoc

docsclean:
	rm -f doc/*.html doc/*.css doc/erlang.png doc/edoc-info doc/doc

go:
	$(REBAR) shell

.erl_cache.plt:
	dialyzer --output_plt $@ --build_plt --apps erts kernel stdlib

dialyzer: .erl_cache.plt compile
	dialyzer --plt $< -c _build/default/deps/erl_cache/ebin/ -Wunknown -Wunmatched_returns
