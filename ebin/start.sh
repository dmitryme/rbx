#!/bin/bash

mkdir -p log

erl -boot start_sasl \
    -config start.config \
    -sname ebin \
    -setcookie 123 \
    -eval "application:start(rbx)"
