#!/bin/bash



NARGS=1  # Two args to script expected.
SLEEPTIME="5s" # polling interval 

E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt"
QUERYCMD="qstat"


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

#NLIST=`$QUERYCMD`

NPROCS=`$QUERYCMD | grep "$1" | wc -l`
echo "There are $NPROCS processes left" 
while (( NPROCS != 0  ))
do
	sleep "$SLEEPTIME"
	NPROCS=`$QUERYCMD | grep "$1" | wc -l`
	echo "There are $NPROCS processes left" 
done

