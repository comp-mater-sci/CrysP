#!/bin/bash
#
HELPMSG="Parameters:
	snapdir - directory that contains snapshots to process
	Facet_config - Facet configuration file
	plot_title - title to be put on the plot
	initial_qdata - raw format of qrs data\n
Remarks:
* snapshots must contain MMM file
* Facet_config must permit run from MMM file\n"


PLOTFILE="plot.plt"

if [ "$#" -lt 2 ] ; then
	echo -e "\n" `basename "$0"` snapdir Facet_config plot_title [initial_qdata] "\n"
	echo -e "$HELPMSG"
	exit 1
fi

SNAPDIR="$1"

YLPEVOLCMD="$HOME/jgprojects/TWRMTMProject/MTM/branches/facet-ALAMEL/facetpar"
YLPCONFIG="$2"
PLOTTITLE="$3"
# gnuplot settings

cat >"$PLOTFILE" <<End-of-CTL-File
set title "$PLOTTITLE"
set key outside vertical box;
set style data linespoints;
set xrange [0:180]
set yrange [0.5:0.8]
set xlabel "Angle,deg"
set ylabel "q-value"
set xtics 15
set grid

plot \\
End-of-CTL-File


if [ "$#" -ge 4 ] ; then
	echo  "'$4' using 4:6  with lines  title 'Initial', \\" >> "$PLOTFILE"
	#echo  " ,\\" >> "$PLOTFILE"
fi


NSTEPS=$(ls -1 ${SNAPDIR}/snap* | wc -l)

if [ "$NSTEPS" == "0" ] ; then
	echo "No snapshots in directory."
	exit 1
fi


((NSTEPS--))

SNAPFILES=$(ls -1 --sort=time ${SNAPDIR}/snap* | tac)

for snap in ${SNAPFILES}
do
	# Extract data
	
	step=$(basename $snap | awk -F_ '{print $3;}')
	echo "Step $step data from $snap"
	# Extract necessary data 
	tar xzf "$snap"  elem.MMM
	# Execute facet identification		
	$YLPEVOLCMD "$YLPCONFIG"
	DATAFILE="qrsvalues_${step}.dat"
	tail -n 365 elem.LS3 | head -n +361  > "$DATAFILE"
	for elem in elem.{LS1,LS3,Q00,F00} 
	do
		mv $elem "step_${step}_${elem}"
	done
	#rm -f elem.LS3 elem.MMM
	#
	echo -n  "'$DATAFILE' using 4:6  title 'step $step' " >> "$PLOTFILE"
	if [ "$step" -lt "$NSTEPS" ] ; then
		echo ", \\" >> "$PLOTFILE"
	fi
done

exit 0


