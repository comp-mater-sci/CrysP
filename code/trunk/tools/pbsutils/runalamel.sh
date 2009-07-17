#!/bin/bash

DEFFILE="defdata.dat"

ALAMELCMD="../src/alamel" 

CTLFILE="MAIN1.CTL"

OUTPREFIX="texout"

INPUTPREFIX="texinp"

E_NOFILE=33

#
# Check if input files are present
#
if [ ! -f "$DEFFILE" ] ; then
	echo "Cannot find $DEFFILE"
	exit $E_NOFILE
fi
#
# Read deffile into variable
#
DEFTENS=`cat $DEFFILE`
#echo "$DEFTENS"
#
# Check runway: start from CUR or SMT
#
if [ -e "$OUTPREFIX.CUR"  ] ; then
	echo "Starting from CUR file"
	# Rename the file: change extension into .cur
	# Remark: if script fails, next time it will start from SMT
	# 
	mv -f "$OUTPREFIX.CUR"  "$INPUTPREFIX.cur"
	INPUT="$INPUTPREFIX.cur"
	RUNWAY="CUR"
	RUNMODE="2"
else
	if [ ! -e "$INPUTPREFIX.smt" ] ; then
		echo "Input SMT file doesn't exist"
		exit "$E_NOFILE"
	fi	 
	echo "Starting from SMT file"
	RUNWAY="SMT"
	INPUT="$INPUTPREFIX.smt"
	RUNMODE="1"
fi

#
# Do the actual work
#

cat >"$CTLFILE" <<End-of-CTL-File
$OUTPREFIX                                Name of output files (give no extension)
bcc.pre                             Slip system file
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
micro1.smt                                                     NAME OF MICROSTRUCTURE FILE
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
# Run ALAMEL code
#
"$ALAMELCMD"
#
# Check error code. If non-zero, try recover from error
#
if [ ! "$?" == 0  ] ; then
	echo Nonzero code from ALAMEL,
	rm -f "$OUTPREFIX.CUR"  "$INPUTPREFIX.cur" 
	NILINES=`wc -l "$INPUTPREFIX.smt"`
	NOLINES=`wc -l "$OUTPREFIX.smt"`
	echo "$NILINES  $NOLINES"  
	if [ "$NILINES" == "$NOLINES"  ] ; then
		echo trying output smt
	fi
fi

#
# Return info code
#
exit $?

