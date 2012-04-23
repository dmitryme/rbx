#!/bin/bash

mkdir -p log

erl -boot start_sasl \
    -config start.config \
    -eval "application:start(log_viewer)"
