#!/bin/sh

for i in $(seq 10); do
    echo O $i >&1
    echo E $i >&2
#    sleep 0.1
done
