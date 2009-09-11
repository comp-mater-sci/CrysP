#!/bin/bash



NARGS=2  # Two args to script expected.


E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt" 
QUERYCMD="qstat"

RUNLOCATION="/data/home/u0061564/lu/Abaqus/cd_gm_tevol/runlocation.sh"

RJOBNAME="recovery"

VERBOSE=1


MYNAME=`basename $0`
# Check number of parameters
if [ $# -nt "$NARGS" ]
then
	echo "Usage: $MYNAME directory <list of locations>"
	exit $E_BADARGS
fi

ROOTDIR="$1"

shift

WORKDIR="`pwd`/recovery"

for job in $* ; do
	RDIR="$WORKDIR/`basename $job`" 
	TARDIR="${ROOTDIR}/${job}"

	echo "$RDIR" "$TARDIR"
	cp -f ${RDIR}/* "$TARDIR"	
done

exit 0

