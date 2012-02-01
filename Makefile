
.PHONY: all app arch clean doc test shell start

ERL ?= erl
REBAR ?= ./rebar

all:
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

arch:
	hg arch ealt-`cat vsn`.tar.gz -X ".hg*"

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc

shell:
	$(ERL) -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config

start:
	$(ERL) -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config -s ealt

test:
	$(REBAR) eunit
