#!/bin/bash
#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -m abe
#PBS -M jerzy.gawad@cs.kuleuven.be
#PBS -r n
##################################################################
# $Id$
#
PROCESSNAME=tensileTest_parametric
SCRIPT="run_long.pbs"
INPFILE="simulations.txt"

CLEANSCRIPT="$HOME/TEXEVOL/scripts/clean.sh" 

[ -e "$INPFILE" ] || { echo "File $INPFILE does not exist"; exit 1; } 

DIRLIST=$(cat "${INPFILE}")

PIDS=""
SEQ=1
CWD=$(pwd)
for dir in $DIRLIST ; do
	sleep 5s
	echo $(readlink -f $CWD/$dir)
	execdir=$(readlink -f "${dir}")
	#execdir="$PWD/$dir" 
	echo Executing simulation in $execdir
	cd $execdir
	# prepare execution
	${CLEANSCRIPT} "${PROCESSNAME}"
	#
	runfile="${execdir}/${SCRIPT}"
	${runfile} 
	cd ${CWD}
done

