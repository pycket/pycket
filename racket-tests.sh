#!/bin/bash

tests=( "pycket/test/struct-test.rkt" )

for test in "${tests[@]}"
do
    echo "Running $test.."
    eval read $testResult <<< $(./pycket-c-nojit $test | awk 'BEGIN { RS = ""; FS = "\n"; } /Errors were:/ {print $0}')
    testResult=$(echo $testResult | xargs)
    echo $testResult
    if [ ! -z "$testResult" -a "$testResult" != " " ]; then
        echo $testResult
        exit 1
    fi
    echo -e
done

exit 0
