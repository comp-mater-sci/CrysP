#!/bin/bash
#
# $Id$
#

if [ $# < 1 ] ; then
	echo $(basename $0) lang
	echo Supported values for lang: c++, f90
	exit
fi

SNIPPETDIR=${HOME}/jgprojects/TWRMTMProject/code/trunk/misc/snippets
case "$1" in
	"c++")
		SNIPPET="${SNIPPETDIR}/info_header.cpp"
		MASK="*.c *.cpp *.h *.hpp" 
		;;
	"f90")
		SNIPPET="${SNIPPETDIR}/info_header.f90"
		MASK="*.f90 *.fpp" 
		;;
	* )
		echo "Unsupported language, no snippet available"
		exit
		;;
esac

echo $SNIPPET

for file in $(ls $MASK) ; do

	# reldate=$(svn log -q ${file} | tac | head -n 2 | tail -1 | gawk -F\| '{print $3}' | gawk '{print $1}')	
	reldate=$(svn log -q ${file} | tail -2 | head -1 | gawk -F\| '{print $3}' | gawk '{print $1}')	

	echo ${file} $reldate

	sed -e "s/<FILENAME>/${file}/" ${SNIPPET} | sed -e "s/<FIRST_RELEASE_DATE>/${reldate}/" > ${file}.hdr	

	cat ${file}.hdr  > ${file}.tmp
	tail +3 ${file} >> ${file}.tmp
	mv  ${file}.tmp ${file}
	rm -f  ${file}.tmp  ${file}.hdr
done

