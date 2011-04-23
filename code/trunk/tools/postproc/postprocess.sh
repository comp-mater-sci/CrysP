#!/bin/bash
#
# $Id$
#
# Initialize PostTex features.
if [ -n  "${POSTTEX_ROOT}" ] ; then   
	. "${POSTTEX_ROOT}/conf/init.sh" 
else
	echo "Error: POSTTEX_ROOT variable undefined."
	exit 1
fi

import "locproc.sh"
import "utils.sh"
import "postprocess.conf"


# Parameters
# $1 - location directory name
# $2 - output directory name
# $3 - output file prefix
# $4 - facet config file
# $5 - plotfile
# $6 - plot 3D data file
# $7 - extract curfile flag (level: 1 - evolution; 2 - for every step)
#
postprocessLocation() {

local LOCATION=$1
local OUTDIR=$2
local PREFIX=$3
local YLPCONFIG="$4"
local PLOTFILE="$5"
local SPLOTFILE="$6"
local EXTRACTCUR="$7"
local BIAXFILE="$8"

FROMSNAP="texout.cub ${PREFIX}.MMM ${DEFFILE}"
NQVALS=$(grep "^(P08)" ${YLPCONFIG} | sed -e 's/^.*:://' | awk '{print $3}' )
(( NQLINES=NQVALS+4 ))

OPFILE="$OUTDIR/$PREFIX" 
local LOGFILE="${OUTDIR}/${PREFIX}.log"


SNAPPATTERN="snap_[1-9]*_[1-9]*.tgz"
NSNAPS=$(countSnapshots "$LOCATION" "$SNAPPATTERN")

echo "Input data in location: $LOCATION" 
echo "Number of snapshots to process: $NSNAPS" | tee -a "${LOGFILE}"
#
if [ "$NSNAPS" == "0" ] ; then
	echo "Error: Location $LOCATION does not contain any snapshot."
	return 1
fi

SNAPLIST=$(sortSnapshots $LOCATION $SNAPPREFIX)

# Create temporary directory, feed it with input
TMPDIR=$(mktemp -d postLoc.XXXXXX) 
cp "$YLPCONFIG" "$TMPDIR"
YLPCFG=$(basename "$YLPCONFIG") 

# Initialize output cur file (write a title)
OUTCURFILE="${OPFILE}.CUR"
if [ "$EXTRACTCUR" -ge 1 ] ; then
	echo $OUTCURFILE
	echo "$LOCATION" > ${OUTCURFILE}
fi
# Initialize output file for residual values
local ERRPLOT="${OUTDIR}/${PREFIX}_err.dat"
local AVRRES=""
local MAXRES=""
if [ "$CALCULATEANISO" == "1" ] ; then
	echo "#step  Ravr  Rmax" >  "${ERRPLOT}"
	echo "Using ${YLPEVOLCMD}"
fi
local defstep=0
local acc_strain=0.0
local strain=0.0
for  snap in $SNAPLIST ; do
	((defstep++))
	step=$(basename $snap | awk -F_ '{print $2;}')
	echo $defstep $step $snap >> "${OPFILE}_map.txt" 
	echo "Processing step $defstep ($step) from $snap" | tee -a "$LOGFILE" 
	#
	# Extract requested datafiles
	tar -xzf $snap -C "$TMPDIR" $FROMSNAP
	## Stage
	markProgress
	if [ "$EXTRACTCUR" -ge "1" ] ; then
	  	
		# Convert CUB file into CUR format, merge it into one file.
		title="step $defstep"
		curname="${PREFIX}_${defstep}.cur"
		stepcur="$TMPDIR/$curname"
		NORIENT=$(cub2cur "$TMPDIR/texout.cub" "$stepcur" "$title"  | grep "crystallites" | sed -e 's/^.*crystallites//' )
		echo "Discrete texture consists of $NORIENT orientations" >> "$LOGFILE"
		tail -n +2 "$stepcur" >> ${OUTCURFILE}
		markProgress
		[ "$EXTRACTCUR" -ge "2" ] && mv "$stepcur" "$OUTDIR/$curname" && markProgress
		#  Extract SMT files
		if [ "$EXTRACTCUR" -ge "3" ] ; then
			local smtname="${PREFIX}_${defstep}.smt"
			cub2smt "$TMPDIR/texout.cub" "$OUTDIR/$smtname" plain "$title" > /dev/null
			markProgress
		fi
	fi
	#	
	CWD=$(pwd)
	# enter temporary directory
	cd "$TMPDIR"
	markProgress
	# Calculate strain from defdata
	strain=$(tail -3 defdata.dat | gawk 'BEGIN{ddot=0.0}{ddot += $1*$1 + $2*$2 +$3*$3}END{print sqrt(ddot)}')
	acc_strain=$(echo $strain $acc_strain | gawk '{sm=$1+$2}END{print sm}')
	echo $defstep $step $strain $acc_strain  >> "${OPFILE}_defmap.txt" 
	# Run YLP calculation if requested
	if [ "$CALCULATEANISO" == "1" ] ; then

		# Execute facet identification		
		$YLPEVOLCMD "$YLPCFG"  >> "$LOGFILE" &> /dev/null
		if [ "$?" == "0" ] ; then
			markProgress
		fi	
		# Process qrs values, prepare output files,
		# build 3D evolution plot
		DATAFILE="${PREFIX}_${defstep}.qrs"
		# Form the AWK program. Only defstep variable is substituted here.
		AWKPROG='{print '"$defstep"' " " $1 " " $3; }'
		# Write header
		head -n 13  ${PREFIX}.LS3 | tail -n 1 | awk '{print "#"$4 " " $5 " " $6 " " $7 }' >  "$DATAFILE" 
		# Write data to datafile and to 3d-plot file.
		tail -n ${NQLINES} ${PREFIX}.LS3 | head -n +${NQVALS} |  awk '{print  $4 " " $5 " " $6 " " $7;}'  | tee -a "$DATAFILE" | awk "$AWKPROG" >> "$SPLOTFILE"
		echo " " >>  "$SPLOTFILE"
		mv "$DATAFILE" "$OUTDIR"
		# 
		markProgress
		#
		# Extract the residual values
		AVRRES="$(grep -a "^Average residual (square norm)" ${PREFIX}.LS1 | cut -d\) -f 2)"
		MAXRES="$(grep -a "^  Maximal residual (magnitude)" ${PREFIX}.LS1 | cut -d\) -f 2)"
		echo "$defstep $AVRRES $MAXRES"  >>  "${ERRPLOT}"
		markProgress
		#
		# TODO: implement it in different way
	#	for elem in elem.{LS1,LS3,Q00,F00} 
	#	do
	#		mv $elem "${OUTDIR}/${PREFIX}_step_${defstep}_${elem}"
	#	done
		#rm -f elem.LS3 elem.MMM
		#
		if [ -n "$PLOTFILE" ] ; then
			echo -n  "'$DATAFILE' using 1:3  title 'step $step' " >> "$PLOTFILE"
			[ "$defstep" -lt "$NSNAPS" ] &&	echo ", \\" >> "$PLOTFILE"
		fi
		#
		# Prepare output for 3D plots in biaxial state of stress
		OUTBIAXDATA="${PREFIX}_${defstep}.DSQ"
		mv "${PREFIX}.DSQ" "${OUTDIR}/$OUTBIAXDATA" 
		
		echo "set output \"${PREFIX}_${defstep}.pdf\"" >> ${BIAXFILE}
		echo "splot './$OUTBIAXDATA' with pm3d nocontour, './$OUTBIAXDATA' with lines palette nosurface;" >> ${BIAXFILE}
		echo "set output"  >> ${BIAXFILE}
	fi
	#
	# Return to previous directory
	cd "$CWD"
	markProgress
	echo "Done."
done

rm -rf "$TMPDIR"
	
}



# Parameters:
# $1 - plot file name
# $2 - plot title
# $3 - reference plot file
# $4 - yrange (for q-values)
#
initPlotfile() {

PLOTFILE="$1"
PLOTTITLE="$2"
REFPLOT="$3"
YRANGE="$4"

[ -z "$YRANGE" ] &&  	YRANGE="[0.5:0.8]"

# gnuplot settings

cat >"$PLOTFILE" <<End-of-CTL-File
reset
set title "$PLOTTITLE"
set key outside vertical box;
set style data linespoints;
set xrange [0:180]
set yrange $YRANGE
set xlabel "Angle,deg"
set ylabel "q-value"
set xtics 15
set grid

plot \\
End-of-CTL-File

# Write an extra line 
[ -n "$REFPLOT" ] && 	echo  "'$REFPLOT' using 4:6  with lines  title 'Initial', \\" >> "$PLOTFILE"

}


init3DPlotfile() {
local PLOTFILE="$1"
local PLOTTITLE="$2"
local PLOTDATA=$(basename $3)
cat >"$PLOTFILE" <<End-of-CTL-File
reset
set title "$PLOTTITLE"
unset key
set yrange [0:180]
set ytics 15

set cntrparam bspline
set cntrparam levels auto 20
set contour base
set view 65, 70, 1.0, 1.0
set palette color
set palette model RGB
set palette defined ( 0 "navy", 1 "blue", 2 "sea-green",  3 "green", 4 "yellow", 5 "orange", 6 "red", 7 "magenta"  )

splot '$PLOTDATA' with pm3d

End-of-CTL-File

}

init3DBiaxPlotfile() {
local PLOTFILE="$1"
local PLOTTITLE="$2"
cat >"$PLOTFILE" <<End-of-CTL-File
reset
set title "$PLOTTITLE"

set palette color model RGB; 
set palette rgbformulae 22,13,-36;
set cntrparam bspline; set cntrparam points 8;
set contour base;set surface;set hidden3d;
unset key;set view 70, 160, 1.0, 1.0;
set xlabel "Angle to RD, deg"  offset 0,-2
set ylabel "{/Symbol s}_t / {/Symbol s}_r" offset 0,-1.7
set zlabel "q-value" rotate by 90;set ztics out;
set xtics 45 out border offset 0.0,-0.5; set mxtics 3;
set ytics out border offset 0.5,0.0
set xrange [  0.00:180.00] reverse;

# Update scale for Z axis if needed
zmin=0.3
zmax=1.0
dz=0.1
set ytics 0.1
set zrange [zmin:zmax]
set cbrange [zmin:zmax]
set cntrparam levels incremental zmin, dz, zmax

# set terminal gif enhanced font "Arial, 18"  animate delay 15 optimize size 1200,800
set terminal pdfcairo enhanced font "Arial,14" color size 22cm,22cm

End-of-CTL-File

}


HELPMSG="Parameters:
	snapdir - directory that contains snapshots to process
	outdir - output directory
	prefix - prefix for filenames
	Facet_config - Facet configuration file or '-' to disable calculations of anisotropic characteristics
	texture_extraction (0 - no extraction, 1 - overall evolution, 2 - details for every step, 3 - also SMT file for every step)
	plot_title - title to be put on the plot
	config_file - local config file to override the global settings
\n
Remarks:
* snapshots must contain MMM file
* Facet_config must permit run from MMM file\n"



if [ "$#" -lt 2 ] ; then
	echo -e "\n" `basename "$0"` snapdir outdir outprefix Facet_config texture_extraction plot_title [config_file] "\n"
	echo -e "$HELPMSG"
	exit 1
fi

SNAPDIR="$1"
OUTDIR="$2"
OUTPREFIX="$3"
YLPCONFIG="$4"
TEXLEVEL="$5"
PLOTTITLE="$6"
# 
# Read config file if specified in command line:
[ -n "$7" ] && [ -f "$7" ] && . "$7"

# Sanitize the input
if [ ! -d "$SNAPDIR" ] ; then
	echo "Directory $SNAPDIR does not exist"
	exit 1
fi

if [ "$YLPCONFIG"  == "-" ] ; then
	CALCULATEANISO=0
else
	if [ ! -f "$YLPCONFIG" ] ; then
		echo "Facet config file does not exist"
		exit 1
	fi
	CALCULATEANISO=1
fi

# Canonize paths 
CSNAPDIR=$(readlink -f "$SNAPDIR")

CYLPCONFIG=$(readlink -f "$YLPCONFIG")

mkdir -p $OUTDIR
COUTDIR=$(readlink -f "$OUTDIR")

echo "Input  dir: $CSNAPDIR"
echo "Output dir: $COUTDIR"

## Initalize plot 
# $1 - plot file name
# $2 - plot title
# $3 - reference plot file
# $4 - yrange (for q-values)
#

if [ "$CALCULATEANISO" == "1" ] ; then
	PLOTFILE="${COUTDIR}/${OUTPREFIX}_q.plt"
	initPlotfile "$PLOTFILE" "$PLOTTITLE" "$REFQPLOT"

	SPLOTFILE="$COUTDIR/$OUTPREFIX.q3d" 
	PLOT3DFILE="${COUTDIR}/${OUTPREFIX}_q3D.plt"
	init3DPlotfile $PLOT3DFILE "$PLOTTITLE" "$SPLOTFILE"

	PLOT3DBIAXFILE="${COUTDIR}/${OUTPREFIX}_biax3D.plt"
	init3DBiaxPlotfile "$PLOT3DBIAXFILE" "$PLOTTITLE" 
fi
#exit 0
postprocessLocation "$CSNAPDIR" "$COUTDIR" "${OUTPREFIX}" "${CYLPCONFIG}" "$PLOTFILE" "$SPLOTFILE" "$TEXLEVEL" "$PLOT3DBIAXFILE"

if [ "$?" == "0" ] ; then
	echo "Finished."
else
	echo "Finished with error(s)."
fi

exit 0




