#!/bin/bash



NARGS=1  # Two args to script expected.


E_BADARGS=20
E_BADJOBNAME=21



RJOBNAME="recovery"

VERBOSE=1


MYNAME=`basename $0`
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	echo "The utility updates the locations specified in listfile with corresponding results of recovery process."
	echo "The listfile contains the original paths (one per line) of directories that were subjected to recovery"
	echo "Utility uses the contents of directory 'recovery'"
	echo -e "\nUsage: $MYNAME  listfile"
	echo -e "\nSee also: manverify.sh, fmanrun.sh\n"
	exit $E_BADARGS
fi


WORKDIR="`pwd`/recovery"

cat $1 |
while read LOCATION   # As long as there is another line to read ...
do
	RDIR="$WORKDIR/`basename $LOCATION`" 
	echo "$RDIR" ' => ' "$LOCATION"
	cp -f -p ${RDIR}/* "$LOCATION"     
done

exit 0


