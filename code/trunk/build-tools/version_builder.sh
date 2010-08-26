#!/bin/bash

TMPINFO=$(mktemp)

svn info > ${TMPINFO}
if [ $? == 0 ] ; then

	REPOROOT=$(grep "^Repository Root:" ${TMPINFO}  | sed -e 's/^Repository Root: //')
	#echo $REPOROOT
	URL=$(grep "^URL:" ${TMPINFO} | sed -e 's/^URL: //') 
	REVISION=$(grep "^Revision:" ${TMPINFO} | sed -e 's/^Revision: //') 
	RELROOT=${URL##${REPOROOT}/}

	echo "#define SVNREVISION  '${REVISION}'"

	echo "970  format('SVN revision:',T20,'${REVISION}')"
	echo "971  format('SVN location:',T20,'${RELROOT}')"
	# Detailed info:
	echo "990  format('Detailed SVN info:', / ,  &"
	for verfile in $* ; do
		echo "'$(svn stat -v ${verfile})', / , &"
	done
	echo "'End of detailed SVN info.')"
else
	# Not SVN revision available
	echo "#define SVNREVISION  'Non-SVN'"
	REVISION='None'
	RELROOT='None'
	echo "970  format('SVN revision:',T20,'${REVISION}')"
	echo "971  format('SVN location:',T20,'${RELROOT}')"

	echo "990  format('Detailed SVN info:',T20,'None', / ,  &"
	echo "'End of detailed SVN info.')"

fi

rm -f ${TMPINFO}

