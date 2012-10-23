#!/bin/bash

mkdir -p log

erl -boot start_sasl \
    -config start.config \
    -name ebin@$HOST \
    -setcookie 123 \
    -eval "application:start(rbx)"
