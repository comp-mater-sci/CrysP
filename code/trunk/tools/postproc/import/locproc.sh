#!/bin/bash
#
# $Id$
#
##############################################################################################################
#
# The function does a search for duplicated snapshots.
#
# Preconditions: The directory must contain at least one snapshot
# A snapshot is considered a duplicate if there is at least one other snapshot that has he same step number.
# The script echoes the list of steps that have duplicated snapshots.
# Parameters:
# $1 - directory name
# $2 - snapshot prefix
# 
detectDuplicates() {
	duplist=$(for snap in  $1/$2*  ; do 
		echo `basename $snap | awk -F_ '{print $2;}'` 
		done | sort | uniq -d)
	echo $duplist 
}

## Example
#
# SNAPPREFIX="snap_"
# for loc in location_* ; do
# 	duplicates=$(detectDuplicates $loc $SNAPPREFIX)
# 	if [ -n "$duplicates" ] ; then
# 		echo $loc `echo $duplicates | wc -w`  duplicates: $duplicates 
# 	fi
# done

##############################################################################################################
#
# The function sorts the snapshots in the step ordering
#
# Preconditions: The directory must contain at least one snapshot
# Parameters:
# $1 - directory name
# $2 - snapshot prefix
# 
sortSnapshots () {
	sortlist=$(for snap in  $1/$2*  ; do 
		echo `basename $snap | awk -F_ '{print $2;}'` 
		done | sort -n )
	for step in  $sortlist ; do
		ls -1 $1/$2${step}_* 
	done
}
#
## Example
#
#SNAPPREFIX="snap_"
# snaplist=$(sortSnapshots $1 $SNAPPREFIX)
# for snap in $snaplist ; do
# 	echo snap: $snap
# done

##############################################################################################################
#
# The function calculates the number of snapshots in the location.
#
# Parameters:
# $1 - directory name
# $2 - snapshot pattern (note: this must be a bash file pattern, not a regexp), see Example.
# 
countSnapshots () {
	ls -1 $1/$2 2> /dev/null | wc -l	
}
## Example
#
# SNAPPATTERN="snap_[1-9]*_[1-9]*.tgz"
# NSNAPS=$(countSnapshots $1 $SNAPPATTERN)
# echo "Number of snapshots: $NSNAPS" 
#
