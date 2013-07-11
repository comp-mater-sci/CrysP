#!/bin/bash
#
# $Id$
#
######################################################################
##### Configuration of the script
#
# Place where the scripts reside
SCRIPTDIR="${HMS_ROOT}/scripts"
#SCRIPTDIR="$HOME/experimental"
#
# Absolute path to the script that will be submitted to PBS queue.
RUNFILE="$SCRIPTDIR/texupdate.sh"
#
# Name of result file. It must be removed at startup.
RESULTFILE="elem.Q00"
#
#
# Name of logfile. If empty, log will not be used.
# TODO: implement logging (low importance...)
LOGFILE="runlocation.log"
#
# Verbosity level 0..3
VERBOSE=1
#
# This option influences failover behaviour of the script. 
# If set to 1, the script will not return until is submits the job (it may last "forever").
# It set to 0, the script will return if successful or if severe error has occured.
PERSISTENTMODE=1
#
######################################################################
##### Advanced configuration parameters - rarely changed
# The command that submits job to queue
QSUBCMD="qsub"
# The command that inquiries about job state
QSTATCMD="qstat"
#
## List of available queues (the order of appearance on the list is important!)
## See the limits related to each queue: qstat -Q -f
## Remark: if you include qdef queue in the list, then the jobs will be redirected
## to other queues if these on the list are full, e.g. to qlong.
PBSQUEUES="qshort
qreg"
#PBSQUEUES="qshort
#qreg
#qdef"
#
# Canonical name of wrapper script
WRAPPERCMD="$SCRIPTDIR/sshwrapper.sh"
#WRAPPERCMD="$SCRIPTDIR/dmsshwrapper.sh"
# The command that starts wrapper script
STARTCMD="ssh login2-ib1"
# Limit of retrials that undertaken on error
LIMIT=5
# Interval between consecutive retrials
DELAY="5s"
# Default value of sleeptime used by guessSleepTime  
# The value depends on estimated execution time of RUNFILE.
DEFSLEEPTIME="240"
# Correction factor used by guessSleepTime  
SLEEPTIMECF=5
#
NARGS=2  # Two arguments for the script are mandatory. More arguments may be provided. 
# Argument 1 : path to directory to start the script inside
# Argument 2 : name of job
#
# Prefix of pbsjob
JOBPREFIX=""
#
#### Values of exit status, don't modify
E_BADARGS=20
E_BADLOC=21
E_PBSERR=150
E_BADQSTART=171
#
ERRCODE=0
#### Return codes, don't modify
R_NEXT="1"
R_OK="0"
R_FAILURE="2"
# Error code
R_ERROR="255"
#
######################################################################
#########  FUNCTIONS
# 
## Function that starts the command $1 in queue $2
##
# Argument $1 - runcmd without queue name
# Argument $2 - queue name to try

runInQueue() {
# Construct runcmd
RUNCOMMAND="-q $2 $1"
# Start the script using qsub
local cond="$R_NEXT"
local errc="$R_OK"
while [ "$cond" == "$R_NEXT" ] ;
do
      # Increment couner
      COUNT=$((COUNT + 1))
	# Try to start 
	JOBID=`$QSUBCMD ${RUNCOMMAND} 2> /dev/null`
	errc=$?
	case "$errc" in
	0 )	# OK, submitted
		cond="$R_OK"
		;;
	226 )	# Queue is full
		[ "$VERBOSE" -ge "2" ] &&  echo "Queue $2 is full."
		cond="$R_FAILURE"
		;;
	* )	# Anything else	
		[ "$VERBOSE" -ge "1" ] &&  echo "qsub from `hostname` failed $COUNT time(s)."
        	if [ "$COUNT" -ge "$LIMIT" ] ; then
               		 cond="$R_ERROR"
          	else
                       	# Wait some time before next attempt
                       	sleep "$DELAY"
            fi
		;;
	esac
done       
return $cond
}

########## 
## Function that starts the command $1 in one of the queues ($2 $3 ...)
##
# Argument $1 - runcmd without queue name
# Argument $2 - $# - queue names to try
#
startJobInQueue() {
# Construct runcmd
BARECMD="$1"
shift  	# $1 is now queue name
# Try to place the job in the queue 
local cond="$R_NEXT"
local errc="$R_OK"
until [ -z "$1" ] || [ "$cond" -ne "$R_NEXT" ] ; 
do	
	runInQueue "$BARECMD" "$1"
	errc=$?
	if [ "$errc" == "$R_OK" ] ; then 
		cond="$R_OK"
	else
		if [ "$errc" == "$R_FAILURE" ] ; then 
			shift
			cond="$R_NEXT"
		else
			cond="$R_ERROR"
		fi
	fi
done       
return $errc
}
# end of function runInQueue

#### 
## Function estimateNextSlot: 
## Parameters: $1 - name of task
## Function will set SLEEPTIME variable
#estimateNextSlot () {
#DEFAULTTIME=30
#	RJOBS=`${QSTATCMD} "-r"`
#	if [ "$?" == "0" ] && [ -z "$RJOBS"  ] ; 
#	then
#		 NPROCS=`echo "$RJOBS"  | grep "$1" `	
#	else
#		SLEEPTIME="$DEFAULTTIME"
#	fi
#}
# End of function extimateNextSlot

# The function estimates the time that remains to complete the most advanced task.
# Parameters: $1 - name of task
guessSleepTime () {
# Variable DEFSLEEPTIME is defined in script header.
# Get job list
RJOBS=`${QSTATCMD} "-u $USER -r"`
if [ "$?" -ne "0" ] ;
then
        SLEEPTIME="$DEFSLEEPTIME"
        return "$DEFSLEEPTIME"
fi
#echo "$JOBLIST"
(( VALUE= DEFSLEEPTIME / 60 ))
# The program below returns estimated time to end of the most advanced job (in minutes!)
AWKPROL=" BEGIN { min=$VALUE; } "
AWKEPL='{\
  split($11,a,":"); \
  split($9,b,":"); \
  if (a[1] != "--") {dist=(b[2]-a[2])+60*(b[1]-a[1]); if (dist < min) {min = dist;} } }\
  END {print min } '
AWKPROG="$AWKPROL$AWKEPL"
RES=`echo "$JOBLIST"  | grep "$1" |  awk "$AWKPROG"`
echo "Time:  $RES"
if [ -n "$RES" ]  && [ "$RES" -gt "0" ] ;
then
	(( SLEEPTIME = ( $RES * 60 ) +  SLEEPTIMECF ))
else
        SLEEPTIME="$DEFSLEEPTIME"
fi
return "$SLEEPTIME"
}
### End of function guessSleepTime 
##
#####################################################################
#########   MAIN
####
#
if [ "$VERBOSE" -gt "1" ] ; then
	echo "Execution of runlocation script on node `hostname`"
	echo "user: $USER  `who am I`"
	date
	echo "pbs_logname: $PBS_O_LOGNAME"
fi
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	echo "Usage: `basename $0` path_to_location jobname"
	exit $E_BADARGS
fi
# Check if location is valid directory, 
if [ ! -d "$1" ] ; then
	echo "Argument $1 doesn't point to valid directory"
	exit $E_BADLOC
fi
#
# Remove resultfile. It also should be done by texupdate.sh, 
# but it is more reliable to preform this action in the context of master script.
#
rm -f "$1/$RESULTFILE"
#
# Construct runcmd - but _without_ queue name
RUNCMD="-d $1 $RUNFILE"
JOBNAME="${JOBPREFIX}$2"
if [ ! -z "$2" ] ; then
	RUNCMD+=" -N $JOBNAME"
fi
#echo $RUNCMD
# Initialize some variables
SLEEPTIME=$DEFSLEEPTIME
#
# Main loop : try to start a job
CONDITION="$R_NEXT"
while [ "$CONDITION" == "$R_NEXT" ] ; 
do
	# Use Method 1 - try to place the job into one of the queues
	startJobInQueue "$RUNCMD" $PBSQUEUES 
	ERRCODE="$?"
	case "$ERRCODE" in
	"$R_OK" )	
		CONDITION="$R_OK"
		;;
	"$R_ERROR" ) # Severe error, possible reasons:
			#  * cannot start job from computenode,
			#  * cannot connect to PBS server
		### Use Method 2: Start using SSH-wrapper 
		[ "$VERBOSE" -ge "1" ] &&  echo "Cannot start job from node ${PBS_O_HOST}"
		# Note: no queue is specified, so default one will be used!
		JOBID=`$STARTCMD "$WRAPPERCMD" "$RUNCMD"`  
	     	if [ "$?" == 0 ] ; then 
        	      CONDITION="$R_OK"
		else
			if [ "$PERSISTENTMODE" == "1" ] ; then  
				# Next iteration 
				CONDITION="$R_NEXT"
		    	else
				# Let the caller deal with the problem...   
				CONDITION="$R_ERROR"
		      fi
		fi
		;;
	"$R_FAILURE" )  # Tempor#ary failure (e.g. all queues are full), lets determine time to wait and try again.
		guessSleepTime "$JOBNAME"
		#	SLEEPTIME=120	# This is a stub, should be implemented	
		[ "$VERBOSE" -ge "1" ] &&  echo "Temporary failure, waiting for ${SLEEPTIME}s"
		sleep "${SLEEPTIME}s"
		CONDITION="$R_NEXT"
		;;
	* )   # Anomaly 
		echo "Unhandled error code  $ERRCODE in main loop!!!"
		;;
	esac
done
# end of main loop
#
#
# Check the result, if CONDITION is different than "ok", then both methods failed.
#
if [ "$CONDITION" == "$R_OK" ] ; then
	echo "job ID=$JOBID has started for location $1"
else
	echo "Cannot start $RUNFILE in location $1"
	exit $E_PBSERR 
fi
#
# Normal exit
exit 0


