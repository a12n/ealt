
VSN := $(shell cat vsn)

# Phony targets

.PHONY: all app arch clean distclean docs dump_vars

all: app

app: ebin/ealt.app
	erl -make

arch:
	hg arch ealt-$(VSN).tar.gz

clean:
	rm -f ebin/*.beam

distclean: clean
	rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info doc/overview.edoc ebin/ealt.app rel/ealt.boot rel/ealt.rel rel/ealt.script rel/ealt.tar.gz

docs: doc/overview.edoc
	erl -pa ebin/ -noshell -run edoc_run application 'ealt' '"."' '[]'

dump_vars:
	@echo VSN = $(VSN)

# File targets

ebin/ealt.app: ebin/ealt.app.src vsn
	sed 's,%VSN%,$(VSN),g' $< > $@

rel/ealt.rel: rel/ealt.rel.src vsn
	sed 's,%VSN%,$(VSN),g' $< > $@

doc/overview.edoc: doc/overview.edoc.src vsn
	sed 's,%VSN%,$(VSN),g' $< > $@
