#!/bin/bash
echo "Executing on `hostname`"
echo "Command wrapper: $*"

#for arg in $* ; do
#	echo "$arg"
#done	
QSUBCMD="qsub"
CONDITION="next"
LIMIT=5
COUNT=0

E_BADSTART=170

while [ "$CONDITION" == "next" ] ;
do
	# Increment couner
	COUNT=$((COUNT + 1))
	# Try start 
	$QSUBCMD $*
	if [ "$?" == 0 ] ; then 
		CONDITION="ok"
	else
		if [ "$COUNT" -ge "$LIMIT" ] ; then
			CONDITION="error"
		fi
	fi

done       

if [ "$CONDITION" == "ok" ] ; then
	exit 0
else
	exit $E_BADSTART
fi

