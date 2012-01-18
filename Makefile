
.PHONY: all app clean doc rel test

REBAR ?= ./rebar

all:
	$(REBAR) compile

app:
	$(REBAR) compile skip_deps=true

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc

test:
	$(REBAR) eunit
