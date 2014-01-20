#! /bin/sh

# This script executes a racket-script using pycket.
# It takes one parameter: the script to be executed.
# Currently the racket-code has to be expanded to an AST (represented in json) before invoking pycket.
# This step is also done by this script.
# You can set the environment variable PYCKET to indicate which pycket-executable to use.
# Otherwise the default targetpycket-c will be used. This can be used to experiment with different versions of pycket.

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
	python expand_tofile.py $scriptfile $jsonfile
fi

# Check the PYCKET environment variable to determine the pycket executable.
# Use a default if necessary
if [ -z $PYCKET ]; then
	PYCKET=targetpycket-c
fi

PYCKET=./$PYCKET
if [ ! -x $PYCKET ]; then
	echo "File does not exist or is not executable: $PYCKET"
	echo "To build the pycket-executable, execute the following:"
	echo "  rpython -Ojit targetpycket.py"
	exit 1
fi

# Now run the actual program
$PYCKET $jsonfile
