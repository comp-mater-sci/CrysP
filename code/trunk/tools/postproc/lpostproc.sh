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

HARDLEVEL=0

POSTPROCESS="${POSTTEX_ROOT}/postprocess.sh"

HELPMSG="\ninputfile - a comma-separed file describing the locations to process. 
Every line is a record, consisting of: 
token,point,integration point number, comment,rotation angle (optional)\n\nExample:\n
9,8,1232,A 0 to RD
12,17,1649,B 45 to RD,45.0
"  

if [ "$#" -lt 6 ] ; then
        echo -e "\n" `basename "$0"` inputfile snapdir_root outdir_pre prefix texture_extraction_level mmm_extraction_level aniso_extraction_type
        echo -e "$HELPMSG"
        exit 1
fi


while IFS=, read token point ipid comment angle
do
	short=$(echo "$comment" | sed -e 's/ //g')
	snapdir="${LOCROOT}/location_${token}_${point}"
	outdir="${OUTPREFIX}${short}"
	rotation=${angle:-0.0}
	echo $token $point
	echo Short comment: \"$short\" Full comment \"$comment\" 
	echo Rotation: $rotation
	echo "Output written to $outdir"
	#./postprocess.sh snapdir outdir outprefix Facet_config texture_extraction plot_title [initial_qdata]
	PHI2="$rotation" "${POSTPROCESS}" "$snapdir" "$outdir" "$PREFIX" "$YLPCFG" "$TEXLEVEL" "$MMMLEVEL" "$HARDLEVEL" "$comment" 
done < <( cat $INPUT )
# The loop above can be implemented in much easier way as long as unlimited comment field (4) is at the end of the line


