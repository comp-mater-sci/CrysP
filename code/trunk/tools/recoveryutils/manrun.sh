#!/bin/bash



NARGS=2  # Two args to script expected.

# Settings suitable for parallel verision of software

E_BADARGS=20
E_BADJOBNAME=21

PBSSTRING=".beopbs-c"

#QUERYCMD="cat qstatlist.txt" 
QUERYCMD="qstat"

RUNLOCATION="/data/home/u0061564/lu/Abaqus/cd_gm_tevol/runlocation.sh"

RJOBNAME="recovery"

VERBOSE=1

PREFIX="./"

MYNAME=`basename $0`
# Check number of parameters
if [ $# -lt "$NARGS" ]
then
	echo "Usage: $MYNAME directory <list of locations>"
	exit $E_BADARGS
fi

ROOTDIR="$1"

shift

WORKDIR="`pwd`/recovery"

for job in $* ; do
	RDIR="$WORKDIR/`basename $job`" 
	mkdir -p "$RDIR"
	cp -f "${ROOTDIR}/${job}/defdata.dat" "$RDIR"
	cp -f "${ROOTDIR}/${job}/texout.cub"  "$RDIR"
	RDIRFULL="`dirname $RDIR`/`basename $RDIR`"
	echo "$RDIRFULL"
	"$RUNLOCATION" "$RDIRFULL" "$RJOBNAME"
done

exit 0

