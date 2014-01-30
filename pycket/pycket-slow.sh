#! /bin/sh

# This script executes a racket-script using pycket run under python, instead of compiled.
# It takes one parameter: the script to be executed.
# Currently the racket-code has to be expanded to an AST (represented in json) before invoking pycket.
# This step is also done by this script.

if [ "$#" -ne 1 ]; then
	echo "Need one parameter: the racket script to be executed with pycket."
	exit 1
fi

scriptfile=$1
jsonfile=${scriptfile}.json

if [ ! -r $scriptfile ]; then
	echo "File does not exist or cannot be opened: $scriptfile"
	exit 1
fi

if [ \( ! -r "$jsonfile" \) -o \( "$scriptfile" -nt "$jsonfile" \) ]; then
	# Either the json-file does not exist or the script-file is newer
	# => (Re)create the json-file
	racket expand_racket.rkt --output $jsonfile $scriptfile
fi

# Check the PYTHON environment variable to determine the python executable.
# Use a default if necessary
if [ -z $PYTHON ]; then
	PYTHON=pypy
fi


# Now run the actual program
$PYTHON runpycket.py $jsonfile
