#!/bin/bash

tests=( "pycket/test/struct-test.rkt" )

printf "\n*** Racket Tests ***\n"

for test in "${tests[@]}"
do
    printf "Running $test.. "
    result=$(./pycket-c-c-nojit $test 2>&1 >/dev/null | awk '/Errors/{y=1;next;}y')
    if [ ! -z "$result" -a "$result" != " " ]; then
        printf "failed:\n$result\n"
        exit 1
    else
        printf "passed\n"
    fi
done

exit 0
