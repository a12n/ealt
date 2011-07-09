
VSN := $(shell cat vsn)

# Phony targets

.PHONY: all arch clean configure distclean docs dump_vars

all:
	erl -make

arch:
	hg arch ealt-$(VSN).tar.gz

clean:
	rm -f ebin/*.beam

configure:
	sed 's,%VSN%,$(VSN),g' ebin/ealt.app.src > ebin/ealt.app
	sed 's,%VSN%,$(VSN),g' rel/ealt.rel.src > rel/ealt.rel
	sed 's,%VSN%,$(VSN),g' doc/overview.edoc.src > doc/overview.edoc

distclean: clean
	rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info doc/overview.edoc ebin/ealt.app rel/ealt.boot rel/ealt.rel rel/ealt.script rel/ealt.tar.gz

docs:
	erl -pa ebin/ -noshell -run edoc_run application 'ealt' '"."' '[]'

dump_vars:
	@echo VSN = $(VSN)
