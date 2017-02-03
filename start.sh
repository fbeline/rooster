#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname rooster_dev \
    -s rooster \
    -s reloader
