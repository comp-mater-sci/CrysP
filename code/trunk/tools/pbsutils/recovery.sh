#!/bin/bash
######################################################################
##### Configuration of the script
#
### Global configuration section
UTILDIR="$HOME/TEXEVOL"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"
DATADIR="$UTILDIR/data"
#
# Name of result file 
RESULTFILE="elem.Q00"
#
#
DIRTYMARK="unclean"
#
SNAPPREF="snap_"
#
DEFFILE="defdata.dat"
OUTPREFIX="texout"
TEXFILE="${OUTPREFIX}.cub"
#
SNAPFILELIST="${TEXFILE}"
#
# Place where the scripts reside
SCRIPTDIR="$HOME/TEXEVOL/scripts"
#
# Name of logfile. If empty, log will not be used.
# TODO: implement logging (low importance...)
LOGFILE="recovery.log"
#
# Verbosity level 0..3
VERBOSE=2
#
#
NARGS=5  # Two arguments for the script are mandatory. More arguments may be provided. 
# Argument 1 : path to directory to start the script inside
# Argument 2 -3 : identifier of current transaction
# Argument 4 -5 : identifier of previous transaction
#
#### Values of exit status, don't modify
E_BADARGS=20
E_BADLOC=21
E_OK=0
E_NOFILE=33
#
ERRCODE=0
#### Return codes, don't modify
R_NEXT="1"
R_OK="0"
R_FAILURE="2"
R_ERROR="-1"
#
######################################################################
#########  FUNCTIONS
clean () {
      rm -f "$FDIRTYMARK" 
}
# 
#
#####################################################################
#########   MAIN
####
#
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	echo "Usage: `basename $0` path_to_location cstep cseq pstep pseq"
	exit $E_BADARGS
fi
# Check if location is valid directory, 
if [ ! -d "$1" ] ; then
	echo "Argument $1 doesn't point to valid directory"
	exit $E_BADLOC
fi
LOCATION="$1"
CSTEP="$2"
CSEQ="$3"
PSTEP="$4"
PSEQ="$5"
#
[ "$VERBOSE" -ge "1" ] && echo "Recovery for $LOCATION started."
#
FDEFFILE="${LOCATION}/${DEFFILE}"
FTEXFILE="${LOCATION}/${TEXFILE}"
FDIRTYMARK="${LOCATION}/${DIRTYMARK}"
FRESULTFILE="$LOCATION/$RESULTFILE"
#
# Check if input files are present
#
if [ ! -f "$FDEFFILE" ] ; then
	echo "Cannot find $DEFFILE"
      # This is unrecoverable error
	exit $E_NOFILE
fi
#
# Read deffile into variables
#
read STEPID SEQNID < $FDEFFILE 
#
[ "$VERBOSE" -ge "1" ] && echo "Recovering transaction for ${STEPID} ${SEQNID}. Target: $CSTEP $CSEQ , source: $PSTEP $PSEQ"
#
#
if  [ "$STEPID" -ne "$CSTEP" ]; then
      echo "File $DEFFILE contains different step number: $STEPID, should be $CSTEP - recovery failed." 
	exit $E_BADARGS
fi
#
# Report presence of files
#
if [ "$VERBOSE" -ge "2" ] ;
then
	echo -n "Location contains: "  
	[ -e "$FDEFFILE" ] && echo -n "$DEFFILE " 
	[ -e "$FTEXFILE" ] && echo -n "$TEXFILE "
	[ -e "$FDIRTYMARK" ] && echo -n "$DIRTYMARK "
	[ -e "$FRESULTFILE" ] && echo -n "$RESULTFILE "
	echo ""
fi
#
#
ECODE="$E_NOFILE"
ACTION="Failed - missing file."
#
# Cleaning
#
rm -f "$FRESULTFILE"
#
if [ "$PSTEP" -ne "0" ] ; 
then
      # Determine the name of snapshot file
      SNAPSHOT=$(ls -1 -t ${LOCATION}/${SNAPPREF}${PSTEP}_${PSEQ}* | head -1)
      [ "$VERBOSE" -ge "1" ] && echo "Snapshot for ${PSTEP} ${PSEQ} : $SNAPSHOT"
	# Examine the content of the location  
	if    [ -n "$SNAPSHOT" ] \
	   &&	[ -s  "$FTEXFILE" ] \
	   && [ "$FTEXFILE" -ot "$FDEFFILE" ] \
         && [ -s "$SNAPSHOT" ] \
	   && [ "$FTEXFILE" -ot "$SNAPSHOT" ] ;
 	then
		ACTION="Location seems to be OK".
		ECODE="$E_OK"
		clean
      else
		# Recover from snapshot
		if [ -n "$SNAPSHOT" ] ; then
      	      tar xzf "$SNAPSHOT" -C "$LOCATION" "$SNAPFILELIST"
           		 if [ "$?" == "0" ] ; then
                 		ECODE="$E_OK"
                  	ACTION="Snapshot ${PSTEP} ${PSEQ} restored."
				clean
            	fi
	    fi
      fi
else
      # There is no need to restore anything, just restart from initial state 
	# Make sure that there is no TEXFILE - texupdate will start from it if can find it.
	if [ -e "$FTEXFILE" ] ; 
	then
		  rm -f "$FTEXFILE" 
	fi
	# Clean
	clean
	ACTION="Cleaning only."
      ECODE="$E_OK"
fi

[ "$VERBOSE" -ge "1" ] && echo "Recovery for $LOCATION completed. $ACTION"

exit $ECODE

