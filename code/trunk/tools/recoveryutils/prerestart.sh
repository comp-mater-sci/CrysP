#!/bin/bash

NARGS=2  # Two args to script expected.

DRYRUN=1

E_BADARGS=20
E_BADJOBNAME=21


VERBOSE=0

LOCPREFIX="location_"
LOCSEP="_"

SNAPPREF="snap_"

OLDPREFIX="old_"

CUBFILE="texout.cub"
RESFILE="elem.Q00"
DEFFILE="defdata.dat"

MYNAME=`basename $0`

# Counters
nproc=0
nrecov=0
nunrecov=0
nretract=0
nfailed=0
nmissing=0
nsuperfl=0
#
#####
retract () {
# $1 location dir
# $2 value
#
# Compose awk program
AWKPROL=" {if ($2 "
AWKREST=' < $2)  print $2;}'
AWKPROG="$AWKPROL$AWKREST"
#
#
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
		  if [ "$DRYRUN" == "1" ]
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
       echo -e "\nUsage: $MYNAME [dryrun|update] statefile [verbose]\n"
       exit $E_BADARGS
fi
#
MODE=$1
INPUT=$2
#
case "$MODE" in 
"dryrun")
		DRYRUN=1
		;;
"update")
		DRYRUN=0
		;;
	*)
		echo "Invalid mode"
		exit $E_BADARGS
		;;
esac
#
if [ "$3" == "verbose" ] ;
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
			if [ "$DRYRUN" == "0" ] ;then
				  tar xzf "$CSNAPSHOT" -C "$LOCATION"
			  	  FDEFFILE="$LOCATION/$DEFFILE"
				  read dstep dseq  < <( head -1 "$FDEFFILE" )
				  if [ "$dstep" -eq "$cstep" ] && [ "$dseq" -eq "$cseq" ] ; then
				  	echo "$LOCATION -> recovered."
			  	  else
				    	echo " -> failed."
					((nfailed++))
				  fi
			fi
		else
			echo -n "$LOCATION : missing snapshot ${cstep} ${cseq}"
			((nunrecov++))
		fi
		#
		#
		retract "$LOCATION" "$cstep"
	else
		echo "$LOCATION not found"
		((nunrecov++))
		((nmissing++))
	fi
#	echo $loc1 $loc2 $cstep $cseq $pstep $pseq
done < <( tail -n $NLOC $INPUT )
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
#
echo -e "\n ****    Report    **** \n"
echo Locations processed: $index
echo Recoverable: $nrecov
echo Unrecoverable: $nunrecov
echo Retractable: $nretract
echo Superfluous: $nsuperfl
echo Failed: $nfailed
echo Missing: $nmissing
echo -e  " **** End of Report **** \n"
