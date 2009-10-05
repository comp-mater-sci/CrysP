#!/bin/bash
#PBS -l nodes=1:ppn=2
#PBS -l walltime=0:07:00
#PBS -r n
#
#*PBS -m a
#*PBS -M jerzy.gawad@cs.kuleuven.be
#*PBS -e /dev/null
#*PBS -o /dev/null
#
### This is PBS script
###
#### Mark the start of job
DIRTYMARK="unclean"
###
builtin echo "1" > ${DIRTYMARK}
###
##
## Global configuration section
UTILDIR="$HOME/TEXEVOL"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"
DATADIR="$UTILDIR/data"
#
# Name of result file 
RESULTFILE="elem.Q00"
#
# Configuration section for ALAMEL
ALAMELCMD="$BINDIR/alamel" 
# Name of ALAMEL config file to be built
CTLFILE="MAIN1.CTL"
DEFFILE="defdata.dat"
OUTPREFIX="texout"
INPUTPREFIX="texinp"
TEXFILE="${OUTPREFIX}.cub"
SMTFILE="${OUTPREFIX}.smt"
#
#
# Configuration section for Facet
FACETBIN="$BINDIR/facetpar"
FACETCONF="Facetpar.par"
TMPLDIR="$DATADIR"
# Number of processors used by Facet (note: it should be in connection with "ppn" resource specification if runs under PBS.
FACETNPROCS=2
#
# Configuration of Snapshot
SNAPFILELIST="${DEFFILE} ${TEXFILE} ${RESULTFILE}"
#
# Special testmode: some actions are skipped
TESTMODE=0
# Set verbosity of output to stdout
VERBOSE=1
#
### Error codes 
E_OK=0
E_SIMERR=100
E_NOFILE=33
#
##############################################################
STEPID=0
SEQNID=0

#
remarkTestMode () {
	echo "***** Test mode, action $1 skipped *****" 
}
#
#
#
##########################################################
runAlamel () {
# Read deffile into variable
#
DEFTENS=`tail -n 3 $DEFFILE`
#
# Check runway: start from CUB or SMT
# 
if [ -e "$TEXFILE"  ] ; then
	[ "$VERBOSE" -ge "1" ] && echo "Starting from CUB file"
	# Rename the file: change filename into inputprefix.cub
	# Remark: if script fails, next time it will start from _INITIAL_ SMT
	# 
	INPUT="$INPUTPREFIX.cub"
	mv -f "$TEXFILE"  "$INPUT"
	RUNWAY="CUB"
	RUNMODE="3"
else
	INPUT="$DATADIR/$INPUTPREFIX.smt"
	if [ ! -e "$INPUT" ] ; then
		echo "Input SMT file ${INPUT} doesn't exist"
		exit "$E_NOFILE"
	fi	 
	[ "$VERBOSE" -ge "2" ] && echo "Starting from SMT file"
	RUNWAY="SMT"
	RUNMODE="1"
fi
#
# Do the actual work
#
cat >"$CTLFILE" <<End-of-CTL-File
$OUTPREFIX                                Name of output files (give no extension)
$DATADIR/bcc.pre                             Slip system file
   16                  No. of lines with tau-crit values (Stored in FK1):
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0
    1     (Main1) NBLOC
$DATADIR/micro1.smt                              NAME OF MICROSTRUCTURE FILE
    1     (SIMUL) NLIST (Make an output listing 0 or 1)
    1     (SIMUL) NFILE (Make output files 0 or 1)
    0     (SIMUL) NFILTW (Make output files 0 or 1)
    1     (SIMUL) NTEN (Print distortion tensor 0 or 1)
    1     (SIMUL) NSYM If =0: TAUC are set to 1; if=1: values from FK1 used.
    0     (SIMUL) IGLIJ  0 or 1 (a print switch. Only for very short runs!)
    0     (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
0.0                            Eta-Factor: Stress concentration fact. on non-deforming particle
0.0                            Attenuation factor (on the stress concentration)
1.0       0.0       0.0       F_Microstructure
0.0       1.0       0.0       F_Microstructure
0.0       0.0       1.0       F_Microstructure
micros
1.486     2.476     8.357       VOCE TAU-III-1, TAU-III-S, T-IV-S
2.75      0.55                  VOCE THETA-1, THETA-T
    $RUNMODE     (Leesor) Type of data set for input texture (1 for SMT-file)
$INPUT                                                    NAME OF INPUT TEXTURE FILE
    1     (Leesor) Chosen Block (in input data set)
    0     (Main) If =1: output for this block is required   INITIALISATION OF SG0
0.025     0.0       0.0       Displacement gradient for this block
0.0       0.0       0.0
0.0       0.0       -0.025
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL     (INITIALISATION OF SG0)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2 (INITIALISATION OF SG0)
    $RUNMODE     (Leesor) Type of data set for input texture (1 for SMT-file)
$INPUT                                                    NAME OF INPUT TEXTURE FILE
    1     (Leesor) Chosen Block (in input data set)
    1     (Main) If =1: output for this block is required.
$DEFTENS 
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (Fake call of SIMUL - for output only)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2  (Fake call of SIMUL - for output only)
End-of-CTL-File
#
# Check if CTL file was written to the disk.
# Remark: in case of disasterous error, this check may not be done.
INFOCODE="$E_NOFILE"
if [ -s "$CTLFILE" ] ; then
      # Run ALAMEL code
      #
      "$ALAMELCMD" > /dev/null
      INFOCODE=$?
fi
#
# Check post-conditons 
if [ "$INFOCODE" == "0" ] \
   && [ -s "$TEXFILE"  ] \
   && [ -s "$SMTFILE" ] \
   && [ -s "$CTLFILE"  ] \
   && [ "$CTLFILE" -ot "$TEXFILE"  ] ;
then
	INFOCODE="$E_OK"
else
	echo "ALAMEL run post-conditions failed"
      INFOCODE="$E_SIMERR"
fi
#
# Return info code
#
return $INFOCODE
}
# End of function runAlamel
##########################################################

############################################################
# copylist accepts 3 parameters:
# $1  -- source directory
# $2  -- list of files that will be copied form source direcory 
#	 to destination
# $3  -- destination (should be directory, otherwise destination 
#	 is overwritten)
############################################################
copylist ()
{
	local ERRCODE=0
	for fname in $2 ; do
		SRC=$1/$fname
		#echo "$SRC => $3"
		if [ -e "$SRC"  ] ; then
			cp -p -f "$SRC" "$3"
		else
			# Emit warning message
			echo "Cannot copy $SRC: file not found"
			ERRCODE="$E_NOFILE"
		fi
	done
	return $ERRCODE
}
# End of function copylist
############################################################
###
prepareExecution () {
local retcode="$E_NOFILE"
# Check if input files are present
#
if [ ! -f "$DEFFILE" ] ; then
	echo "Cannot find $DEFFILE"
	exit $E_NOFILE
fi
# Store current location
TARGETDIR=`pwd`
#
INPDIR="$TARGETDIR"
# List of files that must be copied to scrach location from template
TINPLIST="${FACETCONF}
bcc.dat      
bcc.pre      
micro1.smt   
mod402o.par"
#
# List of files that must be copied to scrach  from location
MINPLIST="$DEFFILE"
# List of files that may be copied to scratch from location
OINPLIST="${TEXFILE}"
#
# Prepare execution
[ "$VERBOSE" -ge "2" ] && echo "Target dir: $TARGETDIR"
# Create temporary on scratch
#
TMPDIR=$(mktemp -d /scratch/facet.XXXXXX)
if [ "$?" -ne "0" ] || [ ! -d "$TMPDIR" ] ;
then
	echo "Cannot create scratch"
	if  [ "$VERBOSE" -ge "1" ] ;
	then
		builtin echo "Scratch error on `/bin/hostname` " > "$DIAGERR"
		/bin/df /scratch  >> "$DIAGERR"
		/bin/ls -d /scratch/facet* >> "$DIAGERR"
		#exit 1
	fi	
	return "$E_NOFILE"
fi
#
# Copy from template
copylist "$TMPLDIR" "$TINPLIST" "$TMPDIR" 
# Copy from workdir - mandatory step
copylist "$TARGETDIR" "$MINPLIST" "$TMPDIR"
# Copy from workdir - optional step
if [ -e "$TEXFILE"  ] ; then
	copylist "$TARGETDIR" "$OINPLIST" "$TMPDIR"
fi
#
# Jump into scratch location
#
cd $TMPDIR
#
if [ "$VERBOSE" -ge "2" ] ; then
	echo "Executing in scrach dir: $TMPDIR"
	ls -x
fi
#
# Extract transaction data from the deffile
#
read STEPID SEQNID < $DEFFILE 
if [ "$?" == "0" ] ; then
	echo "Transaction $STEPID $SEQNID"
	retcode="$E_OK"
else
	retcode="$E_NOFILE"
fi
return $retcode
#
}
##################################################################
##
runFacet () {
# Call simulation 
if [ "$TESTMODE" == 0 ] ; then
	"$FACETBIN"  "$FACETCONF" "$FACETNPROCS"
else
	remarkTestMode "$FACETBIN $FACETCONF"	
	touch "$OUTLST"
fi
##
if [ "$?" -ne "0"  ] ; then
	echo "Simulation exited with non-zero exit code"
	return  $E_SIMERR 
fi

if [ "$VERBOSE" -ge "2" ] ; then
	echo "Current dir :" `pwd`
	ls -x `pwd`
	echo $TMPDIR
fi
return "$E_OK"
}
############################################################
# Finalize execution
finalizeExecution () {
local retcode="$E_NOFILE"
# Name of snapshot file
SNAPSHOTFILE="snap_${STEPID}_${SEQNID}_`date "+%Y%m%d_%H%M%S"`.tgz"
## List of files that must be transferred from scratch dir to target location
OUTLST="$RESULTFILE 
$TEXFILE  
$SNAPSHOTFILE"
# Check if  all required results are in place.
if    [ -s "$TEXFILE" ] \
   && [ -s "$CTLFILE"  ] \
   && [ -s "$RESULTFILE" ] \
   && [ "$RESULTFILE" -nt "$TEXFILE"  ] \
   && [ "$CTLFILE" -ot "$TEXFILE"  ] ;
then
      # It is OK to make snapshot, the results appear to be correct.
	# SNAPFILELIST is defined in header
	if [ -n "$SNAPFILELIST" ] ; then
	      [ "$VERBOSE" -ge "2" ] && echo "Creating snapshot $SNAPSHOTFILE"
		tar czf "$SNAPSHOTFILE" $SNAPFILELIST
	fi
fi
#
# Transport the results
copylist "$TMPDIR" "$OUTLST" "$TARGETDIR"
if [ ! "$?" == "0" ] ; then
	echo "Transport of facet results failed; see messages above."
	return $E_NOFILE
else
	retcode="$E_OK"  
fi	
## Go back to initial directory
cd "$TARGETDIR"
# Perform sanity
if [ "$TESTMODE" == 0 ] ; then
	rm -rf $TMPDIR
else
	remarkTestMode "rm -rf $TMPDIR"	
fi
#
return "$retcode"
}  
# End of function runFacet
############################################################
#
############################################################
# Body of the script 
#
## Print some diagnostic info
#
[ "$VERBOSE" -ge "1" ] && echo "Starting: `basename $0` on node `hostname`"
# Remove previous result file
rm -f ${RESULTFILE}
RETCODE="$E_NOFILE"
##
# Special diagnostic file
#
if [ -n "$PBS_O_WORKDIR" ] ;
then
	DIAGERR="$PBS_O_WORKDIR/error_$PBS_JOBID"  
else
	DIAGERR="`pwd`/error_$$"  
fi
#
prepareExecution
if [ ! "$?" == "0" ] ; then
	echo "prepareExecution finished with error"
	exit $E_SIMERR 
fi
#
[ "$VERBOSE" -ge "2" ] && echo "Entering runAlamel" 
runAlamel 
#
if [ ! "$?" == "0" ] ; then
	echo "Execution of runAlamel finished with error"
	exit $E_SIMERR 
fi
#
[ "$VERBOSE" -ge "2" ] && echo "Entering runFacet" 
runFacet
#
if [ ! "$?" == "0" ] ; then
	echo "Execution of runFacet finished with error"
	exit $E_SIMERR 
fi
#
finalizeExecution
# 
# Now we are in TARGET dir.
# Check the conditions to mark location "non-dirty"
#  -> remark: minimize number of checks, its a network file system.
if    [ "$?" == "$E_OK" ] \
   &&	[ -s "$TEXFILE" ] \
   && [ -s "$RESULTFILE" ] \
   && [ -s "$SNAPSHOTFILE" ] ;
then
      # remove mark      
      rm -f ${DIRTYMARK}
	RETCODE="$E_OK"
fi
echo "Completed."
#
exit $RETCODE

