#!/bin/bash
# $Id$

# Initialize PostTex features.
if [ -n  "${POSTTEX_ROOT}" ] ; then   
	. "${POSTTEX_ROOT}/conf/init.sh" 
else
	echo "Error: POSTTEX_ROOT variable undefined."
	exit 1
fi

INPUT="$1"

LOCROOT="$2"

OUTPREFIX="$3"

PREFIX="$4"

TEXLEVEL="$5"

MMMLEVEL="$6"

YLPCFG="$7"

POSTPROCESS="${POSTTEX_ROOT}/postprocess.sh"

HELPMSG="\ninputfile - a comma-separed file describing the locations to process. 
Every line is a record, consisting of: 
token,point,integration point number, comment\n\nExample:\n
9,8,1232,A 0 to RD
"  

if [ "$#" -lt 6 ] ; then
        echo -e "\n" `basename "$0"` inputfile snapdir_root outdir_pre prefix texture_extraction_level mmm_extraction_level Facet_configfile
        echo -e "$HELPMSG"
        exit 1
fi


while read token point ipid  comment  
do
	short=$(echo "$comment" | sed -e 's/ //g')
	snapdir="${LOCROOT}/location_${token}_${point}"
	outdir="${OUTPREFIX}${short}"
	echo $token $point $short $comment
	echo "Output written to $outdir"
	#./postprocess.sh snapdir outdir outprefix Facet_config texture_extraction plot_title [initial_qdata]
	"${POSTPROCESS}" "$snapdir" "$outdir" "$PREFIX" "$YLPCFG" "$TEXLEVEL" "$MMMLEVEL" "$comment" 
done < <( awk -F, '{print $1 " " $2 " " $3 " " $4  }'  $INPUT )
# The loop above can be implemented in much easier way as long as unlimited comment field (4) is at the end of the line


