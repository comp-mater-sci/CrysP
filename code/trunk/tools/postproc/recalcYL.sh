#!/bin/bash

printHelp()
{
cat <<End-of-help
 Utility name: recalcYL 

 Purpose of the utility: a fast recalculation of the material evolution, provided 
 that some of the inputs can be preimposed (i.e. they are available in the snapshots).
 The outcome of the utility is a recalculated set of snapshots.

 The utility will search for a configuration file: 'recalcYL.conf'. 	

 The input data for the utility is a directory containing a set of snapshots. 
 Depending on the run mode, the kind of data extracted from the snapshot may vary.
 Some run modes may require additional arguments. 

 The following run modes are available:
  * ylp		: calculation of yield surface if the texture is known 
      The utility will grab the CUB file and will convert it to the SMT format. 
      This file will be used as an input in the multilevel model calculations. 
  * texylp	: the utility will retrieve the deformation data from the snapshot. 
		The utilty will start texupdate script, which must be provided by the user.  	 

 To be (possibly) implemented in future releases: 
  * ylpmmm	:  calculation of the yield surface if the multilevel model results are known. 
	The utility will retreive the MMM file from the snapshots, then it will recalculate facet expression. 
	Note: the user should consider whether the postprocessor utility is more convinent to 
	      for this task. Basically, the same result should be achieved. 
End-of-help
}


# shortcut for help option
if [ "$1" == "help" ] ; then
	printHelp
	exit 0;
fi


import() {
	if [ ! -f "$1" ] ; then
		echo "Import failed: cannot find $1" 
		exit 1
	else
		 . "$1"
	fi
}

import "${HOME}/experimental/snapmangle/locproc.sh"
import "recalcYL.conf"


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
$YLPEVOLCMD "$YLPCFG" "${NPROC-2}"  
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



runTexYlpCalc() {
local STEP="$1"
local DEFSTEP="$2"
local CTMPDIR="$3" 
local OUTDIR="$4" 
local TEXUPDATECMD="$5"

# Simply run the texupdate command.
eval $TEXUPDATECMD 
# Check if the command has produced the snapshot
local snapfile=$(ls -1 ${SNAPPREFIX}${STEP}* 2> /dev/null)
if [ -n "$snapfile" ] ; then
	mv "$snapfile" "$OUTDIR"
else
	echo "Cannot find snapshot for step $step" 
fi
}



HELPMSG="
	`basename "$0"` runway snapdir outdir outprefix [Facet_config | texupdate] [configfile]
	or
	`basename "$0"` help

Parameters:
	runway -  ylp, texylp 
	snapdir - directory that contains snapshots to process
	outdir - output directory
	prefix - prefix for filenames
	Facet_config - Facet configuration file (if runway is ylp)
	texupdate - path to texupdate executable (if runway is texylp)	
	configfile - utility configuration file
\n
Remarks:
* The requsitions on the snapshot content depend on the runway parameter. 
  In general, the snapshots must contain files of the following types: MMM, CUB, defdata.dat\n"

if [ "$#" -lt 2 ] ; then
	echo -e "$HELPMSG"
	exit 1
fi

RUNMODE="$1"
SNAPDIR="$2"
OUTDIR="$3"
PREFIX="$4"
CONFIG="$6" 
# verify runmode
case "$RUNMODE" in
	ylp)	
		;;
	texylp)
		;;
	help)
		# shortcut for help option
		printHelp
		exit 0;
		;;
	*)	
		echo "Unknown runway"
		exit 1;
		;;	
esac

# Overrvide default config settings
if [ -n "$CONFIG" && -f "$CONFIG" ] ; then
	import "$CONFIG"
fi

# Sanitize the input
if [ ! -d "$SNAPDIR" ] ; then
	echo "Directory $SNAPDIR does not exist"
	exit 1
fi
# 

# Canonize paths 
LOCATION=$(readlink -f "$SNAPDIR")


mkdir -p $OUTDIR
COUTDIR=$(readlink -f "$OUTDIR")

echo "Input  dir: $LOCATION"
echo "Output dir: $COUTDIR"

# TODO: use getopt/getopts instead.

case "$RUNMODE" in
	ylp)	
		YLPCONFIG="$5"
		if [ ! -f "$YLPCONFIG" ] ; then
			echo "Facet config file does not exist"
			exit 1
		fi
		# Canonical form 
		CYLPCONFIG=$(readlink -f "$YLPCONFIG")
		# 
		FROMSNAP="${DEFFILE} ${TEXFILE} " 
		INPTEX=${TEXFILE}
		TINPLIST="bcc.dat
			bcc.pre
			micro1.smt
			mod402o.par"

		;;

	texylp)
		TEXUPDATE="$5"
		if [ ! -f "$TEXUPDATE" ] || [ ! -x  "$TEXUPDATE" ] ; then
			echo "texupdate file $5 does not exist or is not executable"
			exit 1 
		fi
		# Cannonize the path
		CTEXUPDATE=$(readlink -f "$TEXUPDATE")
		FROMSNAP="${DEFFILE}"
		;;
	*)	
		echo "Unknown runway"
		exit 1
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

if [ "$RUNMODE" == "ylp" ] ; then
	cp "$CYLPCONFIG" "$TMPDIR"
	YLPCFG=$(basename "$YLPCONFIG") 
fi

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
	case "$RUNMODE" in
	ylp)	runYlpCalc "$CTMPDIR" "$YLPCFG" "$INPTEX" 
		## Stage
		#	
		makeSnapshot "$step" "$defstep" "$COUTDIR" "$SNAPFILELIST"
		;;

	texylp)	
		runTexYlpCalc "$step" "$defstep" "$CTMPDIR" "$COUTDIR" "$CTEXUPDATE"
		;;
	*)	
		;;	
	esac
	if [ "$?" == "0" ] ; then
		markProgress
	fi	
	# Return to previous directory
	markProgress

	echo "Done."
done

cd "$CWD"
rm -rf "$TMPDIR"
	

