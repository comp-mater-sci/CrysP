#!/bin/bash



NARGS=1  # Two args to script expected.

# Settings suitable for parallel verision of software

E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt" 
QUERYCMD="qstat"

VERBOSE=1


MYNAME=`basename $0`
# Check number of parameters
if [ $# -ne "$NARGS" ]
then
	echo "Usage: $MYNAME jobname"
	exit $E_BADARGS
fi

JOBS=`qstat  | grep "$1" | awk '{ if ( $5 == "Q") { print $1  } }'`


# check if the name of script is different from name of requested job
# check if the current process is also a PBS job. If so, the list of active jobs should be filtered;

for job in $JOBS ; do
	jobid="${job%%$PBSSTRING}"
#        echo "this is PBS job, my id: $PBS_JOBID , my num: $JOBID" 
	echo $job $jobid	
	ESTAT=`qstat -f $jobid | grep 'exit_status'`
	echo $ESTAT

	qstat -f $jobid

done

exit 0

