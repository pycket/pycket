#! /bin/sh

# This script executes a racket-script using pycket.
# It takes one parameter: the script to be executed.
# Currently the racket-code has to be expanded to an AST (represented in json)
# with racket.

# You can set the environment variable PYCKET to indicate which
# pycket-executable to use. Otherwise the default pycket-c will be used.
# This can be used to experiment with different versions of pycket.

# Check the PYCKET environment variable to determine the pycket executable.
# Use a default if necessary
if [ -z $PYCKET ]; then
    PYCKET=pycket-c
fi

PYCKET=./$PYCKET
if [ ! -x $PYCKET ]; then
    echo "File does not exist or is not executable: $PYCKET"
    echo "To build the pycket-executable, execute the following:"
    echo "  rpython -Ojit targetpycket.py"
    exit 1
fi

# Now run the actual program
$PYCKET "$@"
