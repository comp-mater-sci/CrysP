#!/bin/bash



NARGS=1  # Two args to script expected.
SLEEPTIME="15s" # polling interval 

E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt" 
QUERYCMD="qstat"

VERBOSE=1

NPROCS=-1

getNumProcs () {
	QUERYPROCS=`$QUERYCMD` 
	if [ ! "$?" == 0 ] ; then
		# cannot determine number of processes
		# possibly communication with pbs server is temporarily lost?
		NPROCS=-1

	else
		NPROCS=`echo "$QUERYPROCS"  | grep "$1" | wc -l`
		[ "$VERBOSE" == "1" ] &&  echo "The number of processes left:  $NPROCS" 
	fi
	return $NPROCS
}



MYNAME=`basename $0`
# Check number of parameters
if [ $# -ne "$NARGS" ]
then
	echo "Usage: $MYNAME jobname"
	exit $E_BADARGS
fi

# check if the name of script is different from name of requested job
# check if the current process is also a PBS job. If so, the list of active jobs should be filtered;
if [[ -n "$PBS_JOBID" && "$MYNAME" == "$1"  ]]
then
#	JOBID="${PBS_JOBID%%$PBSSTRING}"
#        echo "this is PBS job, my id: $PBS_JOBID , my num: $JOBID" 
	echo "The job in PBS queue cannot monitor another job with the same name."
	exit "$E_BADJOBNAME"
fi

#JOBLIST=`$QUERYCMD  | grep runfacet | awk '{ print $1 }'`
#echo "Current jobs"
#for jobid in $JOBLIST ; do
#	echo $jobid
#done

# Set initial condition for loop
getNumProcs "$1"

while (( NPROCS != 0  ))
do
	sleep "$SLEEPTIME"
	getNumProcs "$1" 
done

exit 0

