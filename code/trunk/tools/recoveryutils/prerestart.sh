#!/bin/bash
#
# $Id$
#
NARGS=3  # Three args are expected.

DRYRUN=1

E_BADARGS=20
E_BADJOBNAME=21


VERBOSE=0

LOCPREFIX="location_"
LOCSEP="_"

SNAPPREF="snap_"

OLDPREFIX="old_"


MYNAME=`basename $0`

# Counters
nproc=0
nrecov=0
nunrecov=0
ncrecov=0
nretract=0
nfailed=0
nmissing=0
nsuperfl=0
# List of failed locations:
lfailedops=""
#
#####
retract () {
# $1 location dir
# $2 value
# $3 dry-run mode
#
# Compose awk program
AWKPROL=" {if ($2 "
AWKREST=' < $2)  print $2;}'
AWKPROG="$AWKPROL$AWKREST"
#
#
local isdryrun=${3:-"1"}
local snapname
local snapstep
local lnretract
lnretract=0
#
for snap in $1/snap_* ; do
	snapname=$(basename $snap)  
	snapstep=$(echo $snapname | awk --field-separator _ "$AWKPROG") 
	if [ -n "$snapstep" ] ; 
	then
		  ((nretract++))
		  ((lnretract++))
		  if [ "$isdryrun" == "1" ]
		  then
			  [ "$VERBOSE" -ge "1" ] && echo "To be retracted: $snapstep $snap"
		  else
			  [ "$VERBOSE" -ge "1" ] && echo "Retracting $snap"  
			  rm -f "$snap" 
		  fi
	fi
done
#
if [ "$lnretract" -gt "0" ] ;
then
	  echo "Location $1: $lnretract snapshot(s) are to retract."
fi

}

#####

# Check number of parameters
if [ $# -lt "$NARGS" ]
then
       echo -e "\nUsage: $MYNAME [dryrun|update|fast] statefile <FNG|Quantic> [verbose]\n"
       exit $E_BADARGS
fi
#
MODE=$1
INPUT=$2
TYPE=$3
#
NORETRACT=1
#
case "$MODE" in 
"dryrun")
		DRYRUN=1
		;;
"update")
		DRYRUN=0
		NORETRACT=0
		;;
"fast")
		DRYRUN=0
		NORETRACT=1
		;;
	*)
		echo "Invalid mode"
		exit $E_BADARGS
		;;
esac
#
case "$TYPE" in
"FNG")
	RESFILE="elem.fac"
	;;
"Quantic")
	RESFILE="elem.Q00"
	;;
esac
#
CUBFILE="texout.cub"
RESFILE="elem.fac"
DEFFILE="defdata.dat"
# List of the files to extract from snapshot
SNAPRECOVER="$CUBFILE $RESFILE $DEFFILE"
#
if [ "$4" == "verbose" ] ;
then
	  VERBOSE=1
fi
#
if [ ! -e "$INPUT" ] ; 
then
	  echo "Cannot find input file $INPUT"
	  exit $E_NOFILE
fi
#
# Read header of input file into array "header"
#
declare -a header 
header=( `head -3 $INPUT` )
# The header consists of 3 lines, so the first dataline is 4
FIRSTDATA=4
#
NLOC=${header[0]}
NSEQ=${header[1]} 
NSTEP=${header[2]} 
echo "Number of locations: $NLOC" 
echo "Sequence number: $NSEQ" 
echo "Step number: $NSTEP" 
#
# Create temporary files
#
TMPLOCS=`mktemp  prerestart.XXXXX` || exit 1
TMPLOCF=`mktemp  prerestart.XXXXX` || exit 1
#
declare -a outloop
#
# Gather information about state in the filesystem
#
ls -1d ${LOCPREFIX}* | sort > "$TMPLOCF"
#
# Process input file
#
index=0
while read loc1 loc2 tmp cstep cseq pstep pseq
do
	if [ -z "$loc1" ] || [ -z "$loc2" ] ; 
	then
		  echo "Warning: empty line, ignored."
		  continue
	fi

      ((index++))
	  
	#	    outloop[$index]="$line"
	LOCATION="${LOCPREFIX}${loc1}${LOCSEP}${loc2}"
	if [ -d "$LOCATION" ] ; 
	then
		echo "$LOCATION" >> "$TMPLOCS"  
#		if [ "$pstep" -ne "0" ] ; 
#		then 
#			# Check previous snapshot  
#			PSNAPSHOT=$(ls -1 -t ${LOCATION}/${SNAPPREF}${pstep}_${pseq}* 2> /dev/null | head -1 )
#			if [ -z "$PSNAPSHOT" ] || [ ! -e "$PSNAPSHOT" ] ; 
#			then
#				  echo "$LOCATION : missing snapshot ${pstep}_${pseq}"
#			fi
#		fi
#	      #
		# Stage 1: determine if recoverable
		#
		CSNAPSHOT=$(ls -1 -t ${LOCATION}/${SNAPPREF}${cstep}_${cseq}* 2> /dev/null | head -1 )
		if [ -n "$CSNAPSHOT" ] && [ -s "$CSNAPSHOT" ] ; 
		then
		      ((nrecov++))
			[ "$VERBOSE" -ge "1" ] && echo "Snapshot $nrecov: $CSNAPSHOT"
			if [ "$DRYRUN" == "0" ] ; then
				  echo -n "$LOCATION"
				  tar xzf "$CSNAPSHOT" -C "$LOCATION" $SNAPRECOVER
				  if [ "$?" -ne "0" ] ; then
				    	echo " -> snapshot operation failed: $CSNAPSHOT"
					lfailedops="${lfailedops} ${LOCATION}"
					((nfailed++))
				  fi
			  	  FDEFFILE="$LOCATION/$DEFFILE"
				  read dstep dseq  < <( head -1 "$FDEFFILE" )
				  if [ "$dstep" -eq "$cstep" ] && [ "$dseq" -eq "$cseq" ] ; then
				  	echo " -> recovered."
			  	  else
				    	echo " -> failed."
					((nfailed++))
				  fi
			fi
		else
			echo -n "$LOCATION : missing snapshot ${cstep} ${cseq}"
			# Attempt partial match
			PMSNAPSHOT=$(ls -1 -t ${LOCATION}/${SNAPPREF}${cstep}_* 2> /dev/null | head -1 )
			if [ -n "$PMSNAPSHOT" ] && [ -s "$PMSNAPSHOT" ] ;  
			then
				echo " but conditional recovery is possible (partial match ${PMSNAPSHOT} is found)."
				((ncrecov++))
			else
				echo " no partial match."
			fi
			((nunrecov++))
		fi
		#
		#
		retract "$LOCATION" "$cstep" "$NORETRACT"
	else
		echo "$LOCATION not found"
		((nunrecov++))
		((nmissing++))
	fi
#	echo $loc1 $loc2 $cstep $cseq $pstep $pseq
done < <( tail -n "+$FIRSTDATA" $INPUT )
#
# Stage 2: analyze filesystem, compare it with content of input file
# Difference between filesystem and input file is superfluous (incoherent)
#
INCOH=$(cat "$TMPLOCS" "$TMPLOCF" | sort | uniq -u )
nsuperfl=$(echo $INCOH | wc -w)
if [ -n "$nsuperfl" ] && [  "$nsuperfl" -gt "0" ] ;
then
	echo List of locations to remove: 
	echo "----"
	echo $INCOH
	echo "----"
	#
	if [ "$DRYRUN" == "0" ] ;
	then
		echo -n "Removing incoherent locations..."  
		rm -rf $INCOH
		echo "Done."
	fi
fi
# Remove temporary files
rm -f "$TMPLOCS" "$TMPLOCF" 
#
# Remove dirtymarks (they are not harmful, but...)
if [ "$DRYRUN" == "0" ] ;
then
	  echo -n "Removing dirtymarks..."
	  find . -name "unclean" -exec rm {} \;
	  echo "Done."
fi	  
#
echo -e "\n ****    Report    **** \n"
echo Locations processed: $index
echo Recoverable: $nrecov
echo Conditionally recoverable: $nunrecov
echo Unrecoverable: $nunrecov
echo Retractable: $nretract
echo Superfluous: $nsuperfl
echo Failed: $nfailed
echo Missing: $nmissing
if [ -n "${lfailedops}" ] ; then
	echo Locations with recovery failure:
	echo "${lfailedops}"
fi
echo -e  " **** End of Report **** \n"
