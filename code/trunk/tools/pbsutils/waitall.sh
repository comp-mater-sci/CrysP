#!/bin/bash



NARGS=1  # Two args to script expected.

# Settings suitable for parallel verision of software
SLEEPTIME="20" # polling interval, it will be overridden 
SLEEPINIT="315"
SLEEPMIN="30"
NTSTEP=1
WAITTIME=0
WAITTHRESHOLD=310   # set 5:10 min as threshold

### Settings for serial version of the facet software
#SLEEPTIME="15" # polling interval 
#SLEEPINIT="180"
#SLEEPMIN="15"
#NTSTEP=1
#WAITTIME=0
#WAITTHRESHOLD=900   # set 15min as threshold


E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt" 
QUERYCMD="qstat"
#
# Prefix of pbsjob, useful if name must be changed manually
JOBPREFIX=""
#
#
VERBOSE=1
#
NPROCS=-1
#
getNumProcs () {
	QUERYPROCS=`$QUERYCMD` 
	if [ ! "$?" == 0 ] ; then
		# cannot determine number of processes
		# possibly communication with pbs server is temporarily lost?
		NPROCS=-1
	else
		NPROCS=`echo "$QUERYPROCS"  | grep "$1" | awk ' BEGIN {sum=0} { if ( $5 != "E") {sum++;} } END{print sum;}' `
		[ "$VERBOSE" -ge "2" ] &&  echo "The number of processes left:  $NPROCS" 
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
#
PROCNAME="${JOBPREFIX}$1"
# Set initial condition for loop
getNumProcs "$PROCNAME"
 [ "$VERBOSE" -ge "1" ] &&  echo "The number of processes to wait for:  $NPROCS" 

while (( NPROCS != 0  ))
do
	(( NTSTEP += 1 ))
	# Estimate sleeptime
	if [ "$WAITTIME" -lt "$WAITTHRESHOLD"  ] ; then
		SLEEPTIME=$SLEEPINIT
	else
		SLEEPTIME=$SLEEPMIN
	fi
	[ "$VERBOSE" -ge "2" ] &&  echo "Sleeping for $SLEEPTIME"
	sleep "${SLEEPTIME}s"
	(( WAITTIME += SLEEPTIME ))
	[ "$VERBOSE" -ge "2" ] &&  echo "Waiting for $WAITTIME"
	getNumProcs "$PROCNAME" 
done

 [ "$VERBOSE" -ge "1" ] &&  echo "Total waittime: ${WAITTIME}s"

exit 0

