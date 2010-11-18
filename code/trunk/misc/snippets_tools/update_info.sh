#!/bin/bash
#
# $Id$
#

SNIPPET=${HOME}/jgprojects/TWRMTMProject/code/trunk/misc/snippets/info_header.cpp

for file in *.c *.cpp *.h *.hpp ; do

	# reldate=$(svn log -q ${file} | tac | head -n 2 | tail -1 | gawk -F\| '{print $3}' | gawk '{print $1}')	
	reldate=$(svn log -q ${file} | tail -2 | head -1 | gawk -F\| '{print $3}' | gawk '{print $1}')	

	echo ${file} $reldate

	sed -e "s/<FILENAME>/${file}/" ${SNIPPET} | sed -e "s/<FIRST_RELEASE_DATE>/${reldate}/" > ${file}.hdr	

	cat ${file}.hdr ${file} > ${file}.tmp
	mv  ${file}.tmp ${file}
	rm -f  ${file}.tmp  ${file}.hdr
done

