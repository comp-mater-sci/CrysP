#!/bin/bash


PLOTFILE="plot.plt"



# gnuplot settings

cat >"$PLOTFILE" <<End-of-CTL-File
set key outside vertical box;
set style data linespoints;
set xrange [0:180]
set xlabel "Angle,deg"
set ylabel "q-value"
set xtics 15
set grid

plot \\
End-of-CTL-File


if [ "$#" -ge 1 ] ; then
	echo -n  "'$1' using 4:6  with lines  title 'Initial'" >> "$PLOTFILE"
fi


NSTEPS=$(ls -1 snap* | wc -l)

if [ "$NSTEPS" == "0" ] ; then
	echo "No snapshots in directory."
	exit 1
fi

echo  " ,\\" >> "$PLOTFILE"

((NSTEPS--))

SNAPFILES=$(ls -1 --sort=time snap_* | tac)

for snap in ${SNAPFILES}
do
	step=$(echo $snap | awk -F_ '{print $3;}')
	echo $step
	tar xzf $snap elem.LS3
	DATAFILE="qrsvalues_${step}.dat"
	tail -n 365 elem.LS3 | head -n +361  > "$DATAFILE"
	rm -f elem.LS3
	#
	echo -n  "'$DATAFILE' using 4:6  title 'step $step' " >> "$PLOTFILE"
	if [ "$step" -lt "$NSTEPS" ] ; then
		echo ", \\" >> "$PLOTFILE"
	fi
done

exit 0

cat >>"$PLOTFILE" <<End-of-CTL1-File

pause mouse any 'Any key or button will terminate'
End-of-CTL1-File

