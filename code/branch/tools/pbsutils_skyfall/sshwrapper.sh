#!/bin/bash
#
# $Id$
#

QSUBCMD="qsub"

LIMIT=10
COUNT=0
DELAY="30s"
VERBOSE=1

E_BADSTART=170

#
if [ "$VERBOSE" -ge "1" ] ; then
	echo "Executing on `hostname`"
	echo "Command wrapper: $*"
fi
#
# Start qsub
CONDITION="next"
while [ "$CONDITION" == "next" ] ;
do
	# Increment couner
	COUNT=$((COUNT + 1))
	# Try to start 
	$QSUBCMD $*
	if [ "$?" == 0 ] ; then 
		CONDITION="ok"
	else
		if [ "$COUNT" -ge "$LIMIT" ] ; then
			[ "$VERBOSE" -ge "1" ] &&  echo "ssh-wrapped qsub from `hostname` failed $COUNT time(s)."
			CONDITION="error"
		else
			# Wait some time before next attempt
			sleep "$DELAY"
		fi
	fi

done       

if [ "$CONDITION" == "ok" ] ; then
	exit 0
else
	exit $E_BADSTART
fi

