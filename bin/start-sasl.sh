#!/bin/sh

exec /usr/bin/erl -pa ebin/ -boot start_sasl $*
