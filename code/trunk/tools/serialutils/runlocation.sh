#!/bin/bash
#
# $Id$
#
UTILDIR="${GMETEX_WORKDIR}"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"
# Absolute path to the script to be started
RUNFILE="$SCRIPTDIR/texupdate.sh"
#
RESULTFILE="elem.Q00"
#RESULTFILE="fac2sep.par"

TESTMODE="0"

# Verbosity level 0..3
VERBOSE=1
#
# This option influences failover behaviour of the script. 
# If set to 1, the script will not return until is submits the job (it may last "forever").
# It set to 0, the script will return if successful or if severe error has occured.
PERSISTENTMODE=0
#
LOGFILE="$UTILDIR/runlocation.log"

NARGS=2  # One arg to script expected.

#### Values of exit status, don't modify
E_BADARGS=20
E_BADLOC=21
E_PBSERR=150
E_BADQSTART=171
E_SIMERR=100
#
ERRCODE=0
#### Return codes, don't modify
R_NEXT="1"
R_OK="0"
R_FAILURE="2"
# Error code
R_ERROR="255"
#


### Function that starts the command $1 in directory $2
##
# Argument $1 - path to executable
# Argument $2 - 
#
runLocal() {
local errc="$E_SIMERR"
local cwd="$(pwd)"
cd $2
[ "$VERBOSE" -ge "2" ] &&  echo "Executing in " `pwd`
# Call the external simulation
if [ "$VERBOSE" -ge "2" ] ; then
	 eval $1
	 errc="$?"
else
	 eval $1 > /dev/null
	 errc="$?"
fi	
if [ "$errc" == "0" ] ; then
	errc="$R_OK"
else
	errc="$R_ERROR"
fi
cd "$cwd"
# echo $cwd 	
return $errc	
}
####

echo "Execution of runlocation script"

echo "Starting $1" >>  "$LOGFILE"

# Check number of parameters
if [ "$#" -lt "$NARGS" ]
then
        echo "Usage: `basename $0` path_to_location barrier_object "
        exit $E_BADARGS
fi

# Check if location is valid directory, 
if [ ! -d "$1" ] ; then
        echo "Argument $1 doesn't point to valid directory"
        exit $E_BADLOC
fi

#
# This branch of the control flow is aimed at quick testing.
# Instead of starting a simulation, the result file is copied.
if [ "$TESTMODE" == "1" ] ; then
	echo "Doing TRICK"
	cp -f "$RESULTFILE"  "$1"
	INFO=$?
	cd "$1"
	exit $INFO
fi
#
# Normal execution
#
# Main loop : try to start a job
CONDITION="$R_NEXT"
while [ "$CONDITION" == "$R_NEXT" ] ; 
do
	# Start simulation
	runLocal $RUNFILE $1
	ERRCODE="$?"
	case "$ERRCODE" in
	"$R_OK" )	
		CONDITION="$R_OK"
		;;
	"$R_ERROR" ) # Severe error, possible reasons:
		[ "$VERBOSE" -ge "1" ] &&  echo "Cannot start job from node ${PBS_O_HOST}"
		if [ "$PERSISTENTMODE" == "1" ] ; then  
			# Next iteration 
			CONDITION="$R_NEXT"
	    	else
			# Let the caller deal with the problem...   
			CONDITION="$R_ERROR"
	        fi
		;;
	"$R_FAILURE" )  # Temporary failure 
		[ "$VERBOSE" -ge "1" ] &&  echo "Temporary failure, waiting for ${SLEEPTIME}s"
		sleep "${SLEEPTIME}s"
		CONDITION="$R_NEXT"
		;;
	* )   # Anomaly 
		echo "Unhandled error code  $ERRCODE in main loop!!!"
		CONDITION="$R_NEXT"
		;;
	esac
done
# end of main loop
#
#
# Check the result, if CONDITION is different than "ok", then both methods failed.
#
if [ "$CONDITION" == "$R_OK" ] ; then
	echo Texture simulation completed successfuly
else
	echo "Cannot start $RUNFILE in location $1"
	exit $E_PBSERR 
fi
#
# Normal exit
exit 0
# 


