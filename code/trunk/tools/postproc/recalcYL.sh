#!/bin/bash

# Purpose of the utility: a fast recalculation of the material evolution provided that the some of the inputs are preimposed (are available in the snapshots).
# The outcome of the utility is a recalculated set of snapshots.
#
# The input data for the utility is a set of snapshots. Depending on the run mode, the kind of data extracted from the snapshot may vary.
#
# The following run modes are available:
# * ylp		: calculation of yield surface if the texture is known 
#      The utility will grab the CUB file and will convert it to the SMT format. 
#      This file will be used as an input in the multilevel model calculations. 
#  * ylpmmm	:  calculation of the yield surface if the multilevel model results are known. 
#	The utility will retreive the MMM file from the snapshots, Then it will recalculate facet expression. Note: the user shoul consider if the postprocessor utility is more convinent to achieve the same result. 
#  * 
# - texylp	: the utility will retrieve from the snapshot: the deformation data and the CUB file.  	
# *** 




import() {
	if [ ! -f "$1" ] ; then
		echo "Import failed: cannot find $1" 
		exit 1
	else
		 . "$1"
	fi
}

import "locproc.sh"
import "config.sh"


markProgress() {
	if [ -z "$1" ] ; then
		echo -n "." 
	else
		echo -n "x"
	fi
}



runYlpCalc() {
local LOCDIR="$1"
local YLPCFG="$2"
local INPCUB="$3"
#
# NOTE: TINPLIST should be built up according to the content of YLPCFG
#
for file in $TINPLIST ; do
	cp -f "${DATADIR}/${file}" "$LOCDIR"
done
$CUB2SMTCMD $INPCUB texout.smt bare		
$YLPEVOLCMD "$YLPCFG" "2"  
}

makeSnapshot() {
local STEPID="$1"
local SEQNID="$2"
local OUTDIR="$3"
local SNAPFILELIST="$4"
local SNAPSHOTFILE="${OUTDIR}/snap_${STEPID}_${SEQNID}_`date "+%Y%m%d_%H%M%S"`.tgz"
#
if [ -n "$SNAPFILELIST" ] ; then
        #[ "$VERBOSE" -ge "2" ] && echo "Creating snapshot $SNAPSHOTFILE"
        tar czf "$SNAPSHOTFILE" $SNAPFILELIST
fi
#
}




HELPMSG="
	`basename "$0"` runway snapdir outdir outprefix Facet_config [configfile]

Parameters:
	runway -  ylp 
	snapdir - directory that contains snapshots to process
	outdir - output directory
	prefix - prefix for filenames
	Facet_config - Facet configuration file
	configfile - utility configuration file
\n
Remarks:
* The requsitions on the snapshot content depend on the runway parameter. In general, the snapshots must contain files of the following types: MMM, CUB, defdata.dat\n"

if [ "$#" -lt 2 ] ; then
	echo -e "$HELPMSG"
	exit 1
fi

RUNWAY="$1"
SNAPDIR="$2"
OUTDIR="$3"
PREFIX="$4"
YLPCONFIG="$5"
CONFIG="$6" 

#import "$CONFIG"

# Sanitize the input
if [ ! -d "$SNAPDIR" ] ; then
	echo "Directory $SNAPDIR does not exist"
	exit 1
fi
# 
if [ ! -f "$YLPCONFIG" ] ; then
	echo "Facet config file does not exist"
	exit 1
fi

# Canonize paths 
LOCATION=$(readlink -f "$SNAPDIR")

CYLPCONFIG=$(readlink -f "$YLPCONFIG")

mkdir -p $OUTDIR
COUTDIR=$(readlink -f "$OUTDIR")

echo "Input  dir: $LOCATION"
echo "Output dir: $COUTDIR"

# TODO: use getopt/getopts instead.

case "$RUNWAY" in
	ylp)	FROMSNAP="${DEFFILE} ${TEXFILE} " 
		INPTEX=${TEXFILE}
		TINPLIST="bcc.dat
			bcc.pre
			micro1.smt
			mod402o.par"

		;;
	*)	echo "Unknown runway"
		;;	
esac


#### 
# Outline of the script:
#  - create temporary directory, enter temporary it
#  - enter Main loop:
#	* select snapshot	
#  	* according to the runway, extract the necessary data to the temporary
#	* start the appropriate function 
#	* create a snapshot and place it in the output direcory 	

LOGFILE="${COUTDIR}/${PREFIX}.log"
SNAPPATTERN="snap_[1-9]*_[1-9]*.tgz"
NSNAPS=$(countSnapshots "$LOCATION" "$SNAPPATTERN")

if [ "$NSNAPS" == "0" ] ; then
	echo "Error: Location $LOCATION does not contain any snapshot."
	return 1
fi

echo "Input data in location: $LOCATION" 
echo "Number of snapshots to process: $NSNAPS" | tee -a "${LOGFILE}"

SNAPLIST=$(sortSnapshots $LOCATION $SNAPPREFIX)

# Create temporary directory, feed it with input
TMPDIR=$(mktemp -d "$SCRATCH") 
CTMPDIR=$(readlink -f "$TMPDIR")

echo $TMPDIR
echo $CTMPDIR

cp "$CYLPCONFIG" "$TMPDIR"
YLPCFG=$(basename "$YLPCONFIG") 

CWD=$(pwd)
cd "$TMPDIR"
## Clean output files if present

defstep=0
for  snap in $SNAPLIST ; do
	((defstep++))
	step=$(basename $snap | awk -F_ '{print $2;}')
	echo "Processing step $defstep ($step) from $snap" | tee -a "$LOGFILE" 
	#
	# Extract requested datafiles
	tar -xzf $snap -C "$CTMPDIR" $FROMSNAP 
	## Valid for ylp runway
	case "$RUNWAY" in
	ylp)	runYlpCalc "$CTMPDIR" "$YLPCFG" "$INPTEX" 
		;;
	*)	
		;;	
	esac
	## Stage
	#	
	makeSnapshot "$step" "$defstep" "$COUTDIR" "$SNAPFILELIST"
	# Execute facet identification		
	# enter temporary directory
	#$YLPEVOLCMD "$YLPCFG"  >> "$LOGFILE" &> /dev/null
	if [ "$?" == "0" ] ; then
		markProgress
	fi	
	# Return to previous directory
	markProgress

	echo "Done."
done

cd "$CWD"
rm -rf "$TMPDIR"
	

