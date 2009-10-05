#!/bin/bash
# Check number of parameters
if [ "$#" -lt "1" ]
then
	echo "Usage: `basename $0` jobname"
	exit 10
fi

qstat | grep "$1" | cut -f1 -d\  
qstat | grep "$1" | cut -f1 -d\  | xargs qdel

