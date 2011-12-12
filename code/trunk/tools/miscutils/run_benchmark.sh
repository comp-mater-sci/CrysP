#!/bin/bash
#PBS -l nodes=1:ppn=12
#PBS -l walltime=24:00:00
#PBS -m abe
#PBS -M jerzy.gawad@cs.kuleuven.be
#PBS -r n
##################################################################
# $Id$

PROCESSNAME=CupDrawing_parametric
SCRIPT="run_long.pbs"
INPFILE="simulations.txt"

CLEANSCRIPT="$HMS_ROOT/scripts/clean.sh" 

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
	${runfile} ${PROCESSNAME} &
	pid=$!
	echo Simulation $SEQ has PID= $pid
	(( SEQ++ ))	
	PIDS="$PIDS $pid"	
	echo $PIDS
	cd ${CWD}
done

echo Waiting for $PIDS

for pid in $PIDS ; do
	echo Waiting for $pid
	wait ${pid}
	echo Process $pid has finished.
done

