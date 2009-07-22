#!/bin/bash

UTILDIR="$HOME/TEXEVOL"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"

LOGFILE="runlocation.log"

NARGS=1  # One arg to script expected.
E_BADARGS=20
E_BADLOC=21
E_PBSERR=150


echo "Execution of runlocation script"

echo "Starting $1" >>  "$LOGFILE"

# Check number of parameters
if [ $# -ne "$NARGS" ]
then
        echo "Usage: `basename $0` path_to_location"
        exit $E_BADARGS
fi

# Check if location is valid directory, 
if [ ! -d "$1" ] ; then
        echo "Argument $1 doesn't point to valid directory"
        exit $E_BADLOC
fi

if [ "$TESTMODE" == "1" ] ; then
	echo "Doing TRICK"
	cp fac2sep.par "$1"
	INFO=$?
	cd "$1"
else
	# Normal execution
	# Enter selected location 
	cd "$1"
	"$SCRIPTDIR"/texupdate.sh
	INFO=$?
fi

if [ ! "$INFO" == "0" ] ;
then
	echo Cannot execute texture update simulation
else
	# Grab the results...
	echo Texture simulation completed successfuly
fi
# Go back to execution directory
cd -
# 
exit $INFO


