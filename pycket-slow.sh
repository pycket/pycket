#! /bin/sh

# This script executes a racket-script using pycket.
# It takes one parameter: the script to be executed.
# Currently the racket-code has to be expanded to an AST (represented in json)
# with racket.

# You can set the environment variable PYTHON to indicate which
# python-executable to use. Otherwise the default pypy will be used to
# run pycket

# Check the PYTHON environment variable to determine the python executable.
# Use a default if necessary
if [ -z $PYTHON ]; then
    PYTHON=pypy
fi


# Now run the actual program
$PYTHON -m pycket "$@"
