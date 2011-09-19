#!/bin/bash
#
# $Id$
#
NARGS=1  # Two args to script expected.
SLEEPTIME="5s" # polling interval 

E_BADARGS=20
E_BADJOBNAME=21

# Check number of parameters
if [ $# -ne "$NARGS" ]
then
	echo "Usage: `basename $0` jobname"
	exit $E_BADARGS
fi

echo "Waiting for all tasks: $1" 

exit 0


