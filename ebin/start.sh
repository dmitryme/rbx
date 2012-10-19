#!/bin/bash

mkdir -p log

erl -sname ebin \
    -boot start_sasl \
    -config start.config \
    -setcookie 123 \
    -eval "application:start(rbx)"
