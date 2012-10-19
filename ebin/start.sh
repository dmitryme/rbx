#!/bin/bash

mkdir -p log

erl -boot start_sasl \
    -config start.config \
    -name ebin@192.168.1.136 \
    -setcookie 123 \
    -eval "application:start(rbx)"
