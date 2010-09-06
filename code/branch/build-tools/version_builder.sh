#!/bin/bash
#
# $Id$
#
# SVN Revision info - Fortran specific output format
SVNINFO=$(mktemp)
TMPINFO=$(mktemp)

svn info > ${SVNINFO}

if [ $? == 0 ] ; then

	svn stat -v ${*} > ${TMPINFO}

	REPOROOT=$(grep "^Repository Root:" ${SVNINFO}  | sed -e 's/^Repository Root: //')
	#echo $REPOROOT
	URL=$(grep "^URL:" ${SVNINFO} | sed -e 's/^URL: //') 
	WCREV=$(grep "^Revision:" ${SVNINFO} | sed -e 's/^Revision: //') 
	RELROOT=${URL##${REPOROOT}/}
	REVISION=$(cat ${TMPINFO} |  awk 'BEGIN{FIELDWIDTHS = "1 8 9 9"} {if ($4 > vmax) vmax = $4; if ($1 =="M") suff="M";} END {printf "%d%c\n" ,vmax,suff;}')
	echo "#define SVNREVISION  '${REVISION}'"
	echo "#define SVNWCREV  '${WCREV}'"
	echo "#define SVNROOT      '${RELROOT}'"

	echo "970  format('SVN revision:',T20,'${REVISION}')"
	echo "971  format('SVN WC:',T20,'${RELROOT}')"
	echo "972  format('SVN WCrev:',T20,'${WCREV}')"
	# Detailed info:
	echo "990  format('Detailed SVN info:', / ,  &"
	awk -v quote="'" 'BEGIN{FIELDWIDTHS = "1 19 255"} {printf "%c%c", quote, $1; printf "%s%c%s\n", $3, quote, ", / , &";}' < ${TMPINFO}
	echo "'End of detailed SVN info.')"
else
	# Not SVN revision available
	echo "#define SVNREVISION  'Non-SVN'"
	echo "#define SVNWCREV  'Non-SVN'"
	echo "#define SVNROOT      'Non-SVN'"
	WCREV='None'
	RELROOT='None'
	echo "970  format('SVN revision:',T20,'${REVISION}')"
	echo "971  format('SVN WC:',T20,'${RELROOT}')"
	echo "972  format('SVN WCrev:',T20,'${WCREV}')"

	echo "990  format('Detailed SVN info:',T20,'None', / ,  &"
	echo "'End of detailed SVN info.')"

fi

rm -f ${SVNINFO} ${TMPINFO}

