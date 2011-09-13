#!/bin/bash
# $Id$

if [ "$#" -lt 2 ] ; then
	echo $(basename $0) source_dir output_dir
	echo The script will extract current texture files from locations at source_dir
	echo The CUB files will be converted into CUR format.
	exit 1
fi

SRCDIR=$1
OUTDIR=$2

CUB2CUR=~/TEXEVOL/bin/cub2cur
CUBNAME=texout.cub


LOCPREFIX=location_

mkdir -p "$OUTDIR"

for dir in $SRCDIR/$LOCPREFIX* ; do 
	if [ ! -d "$dir" ] ; then
		echo Structure of source_dir is incorrect.
		exit 1
	fi
	loc="$(basename $dir)"
	shortloc=${loc#$LOCPREFIX}
	echo $dir $loc $shortloc
	"$CUB2CUR" "$dir/$CUBNAME" "$OUTDIR/${shortloc}.cur" "$loc" > /dev/null
	if [ "$?" == 0 ] ; then
		echo $loc OK
	else
		echo $loc FAILED
	fi
done

