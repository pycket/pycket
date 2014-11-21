#!/bin/bash

printf "Running $1.. "
result=$(./pycket-c-c-nojit $1 2>&1 >/dev/null | awk '/Error/{y=1;next;}y')
if [ ! -z "$result" -a "$result" != " " ]; then
    printf "failed:\n$result\n"
    exit 1
else
    printf "passed\n"
fi

exit 0
