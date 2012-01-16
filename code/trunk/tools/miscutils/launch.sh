#!/bin/bash
#
# $Id$
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Levuen)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#

MINARGS=4
if [ $# -lt "$MINARGS" ] ; then
cat <<EOFHELP
The script starts process "exec" with argument "arg" in directories 
listed in file "dir_list", each time preceded by starting "pre-exec"
Syntax:
$(basename $0) <--par|--seq> <exec> <arg> <dir_list> [pre-proc]"

Options:
 --par     - start the processes in parallel
 --seq     - start the processes sequentially
 exec      - name of executable. Unless an absolute file name is provided, the script
             will assume that "exec" is located in the execution directory.
 arg       - argument
 dir_list  - name of a file containing the list of working directories
 pre-exec  - (optional) name of executable to be executed prior to "exec".
             To determine the location of "pre-exec" executable, the same convention 
             as for "exec" is used.

Environment variables
SLEEPTIME  - time period between starting the processes. See "man sleep"
             to check proper time formats.

EOFHELP
	exit 1
fi

MODE="$1"
EXEC="$2"
PROCESSNAME="$3"
INPFILE="$4"
PREEXEC="$5"

case "${MODE}" in
--par|--seq)
	;;
*)
	echo Incorrect parameter: ${MODE}
	exit 1
	;;
esac

[ -e "$INPFILE" ] || { echo "File $INPFILE does not exist"; exit 1; } 

ABS_EXEC=$(expr match "${EXEC}" "^/")
ABS_PREEXEC=$(expr match "${PREEXEC}" "^/")

if [ "${ABS_EXEC}" == "1" ] ; then
	[[ -x "$EXEC"  ]] || { echo "Exec file $EXEC not found"; exit 1; }
fi
if [ "${ABS_PREEXEC}" == "1" ] ; then
	[ -x "${PREEXEC}"  ] || { echo "Exec file $PREEXEC not found"; exit 1; }
fi



DIRLIST=$(cat "${INPFILE}")


PIDS=""
SEQ=1
CWD=$(pwd)
for dir in $DIRLIST ; do
	cd "${CWD}"
	execdir=$(readlink -e "${dir}")
	[ -z "${execdir}" ] && { echo "Warning: non-existing dir $execdir is skipped" ; continue; }
	echo Executing in $execdir
	cd $execdir
	# prepare execution
	if [ -n "${PREEXEC}"  ] ; then
		if [ "${ABS_PREEXEC}" == "0" ]; then
			prerunfile="${execdir}/${PREEXEC}" 
			[ -x "${prerunfile}" ] || { echo "Warning: executable $PREEXEC not found for dir $dir"; continue; }
		else
			prerunfile="${PREEXEC}"
		fi
		"${prerunfile}" "${PROCESSNAME}"
	fi
	# Attempt execution
	if [ "${ABS_EXEC}" == "0" ] ; then
		runfile="${execdir}/${EXEC}"
		[ -x "${runfile}" ] || { echo "Warning: executable $EXEC not found for dir $dir"; continue; }
	else
		runfile="${EXEC}"
	fi
	#
	case "${MODE}" in
	"--seq")
		${runfile} ${PROCESSNAME}
		;;
	"--par")
		${runfile} ${PROCESSNAME} &
		pid=$!
		echo "Process ${SEQ} has PID= ${pid}"
		(( SEQ++ ))	
		PIDS="${PIDS} ${pid}"	
		# echo $PIDS
		;;
	esac
	cd ${CWD}
	[ -n "${SLEEPTIME}" ] && sleep "${SLEEPTIME}"
done
# Wait for background processes
if [ "${MODE}" == "--par" ] ; then
	[ -n "${PIDS}" ] && echo "Waiting for ${PIDS}"
	# Note: the loop below could be replaced by:
	# wait ${PIDS}
	# but there would be no possibility to write the progress messages
	for pid in $PIDS ; do
		echo "Waiting for ${pid}"
		wait ${pid}
		echo "Process ${pid} has finished."
	done
fi

