#!/bin/bash

DEFFILE="defdata.dat"

ALAMELCMD="../src/alamel" 

CTLFILE="MAIN1.CTL"

OUTPREFIX="texout"

INPUTPREFIX="texinp"

E_NOFILE=33

##
# Remark for future development: it is possible to write almost entire MAIN1.CTL file using
# "Here-document" feature. Note that macro substitution is sufficient for this purpose...


#
# Check if input files are present
#
if [ ! -f "$DEFFILE" ] ; then
	echo "Cannot find $DEFFILE"
	exit $E_NOFILE
fi


#
# Define two functions
#

function writeCURLines ()
{
cat >>"$CTLFILE" <<End-of-CUR-Line
    2     (Leesor) Type of data set for input texture (1 for SMT-file)
$INPUTPREFIX.cur                                                    NAME OF INPUT TEXTURE FILE
End-of-CUR-Line
}

function writeSMTLines ()
{
cat >>"$CTLFILE" <<End-of-SMT-Line
    1     (Leesor) Type of data set for input texture (1 for SMT-file)
$INPUTPREFIX.smt                                                    NAME OF INPUT TEXTURE FILE
End-of-SMT-Line

}

#
# Check runway: start from CUR or SMT
#
if [ -e "$OUTPREFIX.CUR"  ] ; then
	echo "Starting from CUR file"
	# Rename the file: change extension into .cur
	# Remark: if script fails, next time it will start from SMT
	# 
	mv -f "$OUTPREFIX.CUR"  "$INPUTPREFIX.cur"
	RUNWAY="CUR"
else
	if [ ! -e "$INPUTPREFIX.smt" ] ; then
		echo "Input SMT file doesn't exist"
		exit "$E_NOFILE"
	fi	 
	echo "Starting from SMT file"
	RUNWAY="SMT"
fi



#
# Write prologue
#
cat >"$CTLFILE" <<End-of-prologue
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
End-of-prologue
#
# Write CUR/SMT for the first time  
#
if [ "$RUNWAY" == "CUR" ] ; 
then
	writeCURLines
else
	writeSMTLines
fi
#
#  Write mid-part1
#
cat >>"$CTLFILE" <<End-of-Mid1
    1     (Leesor) Chosen Block (in input data set)
    0     (Main) If =1: output for this block is required   INITIALISATION OF SG0
0.025     0.0       0.0       Displacement gradient for this block
0.0       0.0       0.0
0.0       0.0       -0.025
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL     (INITIALISATION OF SG0)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2 (INITIALISATION OF SG0)
End-of-Mid1
#
# Write CUR/SMT for the second time  
#
if [ "$RUNWAY" == "CUR" ] ; 
then
	writeCURLines
else
	writeSMTLines
fi
#
#  Write mid-part2
#
cat >>"$CTLFILE" <<End-of-Mid2
    1     (Leesor) Chosen Block (in input data set)
    1     (Main) If =1: output for this block is required.
End-of-Mid2
#
#  Write deformation data 
#
cat "$DEFFILE" >> "$CTLFILE" 
#
# Write epilogue
#
cat >>"$CTLFILE" <<End-of-epilogue
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (Fake call of SIMUL - for output only)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2  (Fake call of SIMUL - for output only)
End-of-epilogue

#
# Run ALAMEL code
#
"$ALAMELCMD"
#
# Return info code
#
exit $?

