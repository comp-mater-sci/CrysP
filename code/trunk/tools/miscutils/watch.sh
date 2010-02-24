#!/bin/bash

# Param 1: filename 
# Param 2: string to watch for

if [ "$#" -lt "2" ] ; then
	echo `basename $0`  filename \"pattern\"
	exit 1
fi

if [ ! -f "$1" ] ; then
	echo "The file $1 does not exist."
	exit 2 
fi

INPFILE=$(readlink -f $1)
PATTERN="$2"

MAILADDR="jerzy.gawad@cs.kuleuven.be"
HOSTNAME=$(hostname)
STARTTIME=$(date)

while read theString
do
	MATCH=$(echo "$theString" | grep "$PATTERN")
	if [ -n "$MATCH" ] ; then
		# Perform the action
		echo -e "The string\n\n${theString} \n\nmatches the pattern:\n\n${PATTERN}" | mail -s "Watch for ${PATTERN} on $HOSTNAME" "$MAILADDR" 
		echo "Match $MATCH"
	fi
#done < <( cat "$INPFILE" | grep "$PATTERN" )
done < <( tailf "$INPFILE"  )
#
# It seems that the following doesnt work...  
#   done < <( tailf "$INPFILE" | grep "$PATTERN" )o
## WHY???
