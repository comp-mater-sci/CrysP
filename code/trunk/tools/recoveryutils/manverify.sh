#!/bin/bash


NARGS=2  # Two args to script expected.


E_BADARGS=20
E_BADJOBNAME=21


VERBOSE=1

LOCPREFIX="location_"
CTLFILE="MAIN1.CTL"
CUBFILE="texout.cub"
RESFILE="elem.Q00"
DEFFILE="defdata.dat"
SNAPPREF="snap_"
DIRTYMARK="unclean"

MYNAME=`basename $0`
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	 echo "The utility builds the list of locations that appear to be in incorrect state"
	 echo "The symptoms:" 
	 echo " * there is no deffile"
	 echo " * there is no cubfile or is empty"
	 echo " * result file (facet/quantic) is older than cubfile"
	 echo " * there is no snapshot file that correspond to deffile"
       echo -e "\nUsage: $MYNAME directory  outlistfile"
       exit $E_BADARGS
fi

#  find "$1" -name "MAIN1.CTL" -size 0  -execdir pwd \; > "$2"
echo -n '' > $2

n=0



while read LOCATION
do
	BADLOC=0  
	RSNAP="-"
	RTEX="-"
	RRES="-"
	RUNCL="-"
	RTIME="-"
      FDEFFILE="$LOCATION/$DEFFILE"
	if [ ! -s "$FDEFFILE" ] ;
	then
		  echo "Error: no deffile in location $LOCATION" 
		  continue
	fi
	# Check presence od dirtymark
	if [ -e "$LOCATION/$DIRTYMARK" ] ;
	then
		  RUNCL="U"
		  BADLOC=1
	fi
	# Check if snapshot corresponds to contents of deffile
	read cstep cseq  < <( head -1 "$FDEFFILE" )
	SNAPSHOT=$(ls -1 -t ${LOCATION}/${SNAPPREF}${cstep}_${cseq}* 2> /dev/null | head -1 )
	if [ -z "$SNAPSHOT" ] || [ ! -s "$SNAPSHOT" ] ;
	then
		RSNAP="S"
		BADLOC=1
      fi
	# Check if result file exists and nonempty
	FRESFILE="$LOCATION/$RESFILE" 
	if [ ! -s "$FRESFILE" ] ;
	then
		  RRES="R"
		  BADLOC=1
	fi
	# Check if texture representation is ok
	FTEXFILE="$LOCATION/$CUBFILE" 
	if [ ! -s "$FTEXFILE" ] ;
	then
		  RTEX="C"
		  BADLOC=1
	fi
	# Check time relations between files
	## WARNING: these tests are buggy because if any of the files doesn't exist, the time check always fails!!!!
	#
	# Time check: part 1
	if    [ "$FTEXFILE" -ot "$FDEFFILE" ] \
	   || [ "$FRESFILE" -ot "$FDEFFILE" ] ;
 	then
		RTIME="t"
		BADLOC=1
  	fi
	# Time check: part 2, requires presence of snapshot
	if [ "$RSNAP" != "S" ] ;
	then
	  	if    [ "$FTEXFILE" -nt "$SNAPSHOT" ] \
	   	   || [ "$FRESFILE" -nt "$SNAPSHOT" ] \
	         || [ "$FDEFFILE" -nt "$SNAPSHOT" ] ;
 		then
			RTIME="t"
			BADLOC=1
    		fi
  	fi
	#
	# Report 
	if [ "$BADLOC" -ne "0" ] ;
	then
		echo "${RSNAP}${RTEX}${RRES}${RUNCL}${RTIME} $LOCATION"
      	echo $LOCATION >> "$2"
		(( n++ ))
	fi
done < <(find "$1" -name "$LOCPREFIX*" -type d -print )

echo "Number of suspicious locations: $n"

exit 0

