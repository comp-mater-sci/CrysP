#!/bin/bash



NARGS=1  # Two args to script expected.


E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

RUNLOCATION="/data/home/u0061564/lu/Abaqus/cd_gm_tevol/runlocation.sh"

RJOBNAME="recovery"

VERBOSE=1


MYNAME=`basename $0`
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	echo "The utility performs recovery calculations for locations specified in listfile"
	echo "The listfile contains the paths (one per line) to directories that will be subjected to recovery process"
	echo "The results are placed in directory 'recovery'"
	echo -e "\nUsage: $MYNAME  listfile"
	echo -e "\nSee also: manverify.sh, manupd.sh\n"
	exit $E_BADARGS
fi


WORKDIR="`pwd`/recovery"

cat $1 |
while read LOCATION   # As long as there is another line to read ...
do
	RDIR="$WORKDIR/`basename $LOCATION`" 
	echo "$LOCATION $RDIR"  
	mkdir -p "$RDIR"
	cp -f "${LOCATION}/defdata.dat" "$RDIR"
	cp -f "${LOCATION}/texout.cub"  "$RDIR"

	RDIRFULL="`dirname $RDIR`/`basename $RDIR`"
	echo "$RDIRFULL"
	"$RUNLOCATION" "$RDIRFULL" "$RJOBNAME"
done

exit 0


