#!/bin/bash
# Colon-separed list of import directories
POSTTEX_IMPORT="${POSTTEX_ROOT}/import"
# Colon-separed list of imported files 
POSTTEX_IMPORTED="init.sh"
#
# The function implement a basic "import" capability. 
# Parameter $1: name of file to import
# The function keeps track of imported files to avoid doubled imports. 
import() {
	local idir=""
	local inotfound="1"
	# Check if already in POSTTEX_IMPORTED
	[ -z  $(echo "$POSTTEX_IMPORTED" | grep "$1") ] || return 0
	# Otherwise, search and append
	for idir in $(echo ${POSTTEX_IMPORT} | tr ":" "\n" ) ;
	do
	        if [ -f "${idir}/${1}" ] ; then
                	. "${idir}/${1}"
			if [ -z ${POSTTEX_IMPORTED} ] ; then
				POSTTEX_IMPORTED=${1}
			else
				POSTTEX_IMPORTED=${POSTTEX_IMPORTED}:${1}

			fi
			return "0"
        	fi
	done
	echo "Error: cannot import $1"
	exit 1
}


