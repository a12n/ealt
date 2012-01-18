
.PHONY: all app clean doc test shell start

ERL ?= erl
REBAR ?= ./rebar

all:
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc

shell:
	$(ERL) -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config -boot start_sasl

start:
	$(ERL) -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config -boot start_sasl -s ealt

test:
	$(REBAR) eunit
