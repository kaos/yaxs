#!/bin/sh

erl \
    -name yaxs@localhost \
    -pa ebin \
    -s make all \
    -boot start_sasl \
    -eval "application:start(yaxs)"

