#!/bin/bash
#PBS -l ncpus=1
#PBS -l walltime=0:50:00

WAITCMD="./waitall.sh runfacet.sh"

RUNSCRIPT="./runlocation.sh"
QSUBCMD="qsub"
QUEUE="qshort"
CWD=`pwd`

echo "metatask is working" >> meta.log

DIRLIST="location_0_3
location_0_5
location_1_1
location_1_3
location_1_4
location_2_2"

for loc in $DIRLIST ; do
	echo $loc
#	"$QSUBCMD" -q "$QUEUE"  -d "$CWD" "$RUNSCRIPT  $loc"
	echo "$RUNSCRIPT"  "$loc"
done

#$WAITCMD

