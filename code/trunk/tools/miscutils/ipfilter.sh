#!/bin/bash

# Check the number of parameters
NARGS=2

if [ $# -ne "$NARGS" ]
then
        echo "Usage: `basename $0` data_file nodelist_file"
        exit $E_BADARGS
fi

if [ ! -e "$1" ] || [ ! -e "$2" ] ; then
	echo "On of input files doesn't exist" 
	exit 5
fi

NODELIST=`cat "$2"` 

INPUT="$1"

PREFIX="ip"

for node in $NODELIST ; do
	echo "Processing point $node"
#	awk '$1 ~ /$node/ { print $0 }' results.tay > ip$node.dat
	prog='$1 ~ /^'$node'$/ { print $0 }' 
	awk "$prog" $INPUT > $PREFIX$node.dat 
done


