#!/bin/bash


NARGS=2  # Two args to script expected.


E_BADARGS=20
E_BADJOBNAME=21


VERBOSE=1

LOCPREFIX="location_"
CTLFILE="MAIN1.CTL"
CUBFILE="texout.cub"
RESFILE="elem.Q00"

MYNAME=`basename $0`
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	 echo "The utility builds the list of locations that appear to be in incorrect state"
	 echo "The symptoms:" 
	 echo " * location contains a zero-sized MAIN1.CTL file"
	 echo " * there is no cubfile or is empty"
	 echo " * result file (facet/quantic) is older than cubfile"
	 echo " * MAIN1.CTL is newer than cubfile"
       echo -e "\nUsage: $MYNAME directory  outlistfile"
       exit $E_BADARGS
fi

#  find "$1" -name "MAIN1.CTL" -size 0  -execdir pwd \; > "$2"
echo -n '' > $2

n=0

find "$1" -name "$LOCPREFIX*" -type d -print | 
while read LOCATION
do
	  if [ ! -e "$LOCATION/$CUBFILE" ] \
		    ||  [ ! -s "$LOCATION/$CUBFILE" ] \
		    ||  [ ! -s "$LOCATION/$CTLFILE"  ] \
		    || [ "$LOCATION/$RESFILE" -ot "$LOCATION/$CUBFILE"  ] \
		    || [ "$LOCATION/$CTLFILE" -nt "$LOCATION/$CUBFILE"  ] ; 
	  then
		 echo $LOCATION >> "$2"
		(( n += 1 ))
		echo "$n"
	  fi
done

exit 0

