DIALYZER ?= dialyzer
ERL ?= erl
REBAR ?= ./rebar

DIALYZER_PLT = .dialyzer_plt

.PHONY: all app arch clean distclean dlyze doc plt test shell start

all:
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

arch:
	hg arch ealt-`date +%Y%m%d`.tar.gz -X ".*"

clean:
	$(REBAR) clean

distclean: clean
	rm -f $(DIALYZER_PLT)

dlyze:
	$(DIALYZER) \
		--plt $(DIALYZER_PLT) \
		./ebin

doc:
	$(REBAR) doc

plt:
	$(DIALYZER) \
		--apps \
			./deps/cowboy/ebin \
			./deps/jsx/ebin \
			inets \
			kernel \
			stdlib \
		--build_plt \
		--output_plt $(DIALYZER_PLT) \
		--verbose \
		./src

shell:
	$(ERL) -smp -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config

start:
	$(ERL) -smp -pa ebin/ -pa deps/*/ebin/ -config priv/ealt.config -s ealt

test:
	$(REBAR) eunit
