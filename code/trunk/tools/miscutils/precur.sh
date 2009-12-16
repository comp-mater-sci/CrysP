#!/bin/bash
#
# The snapshot files must contain CUR files. Otherwise cub2cur utility must be used.
#
HELPMSG="Parameters:
 	norient - number of orientations in each CUR file that will be processed by the tool
	outputfile - name of output file
	title - title of output  file (it will be placed in output CUR file)\n"

#
if [ "$#" -lt 3 ] ; then
	echo -e "\n" `basename "$0"` norient outputfile title "\n"
	echo -e "$HELPMSG"
	exit 1
fi


NORIENT="$1"
OUTFILE="$2"
TITLE="$3"

CURFILE="texout.CUR"

((NLINES=NORIENT+4))


NSTEPS=$(ls -1 snap* | wc -l) 

if [ "$NSTEPS" == "0" ] ; then
        echo "No snapshots in directory."
        exit 1
fi


((NSTEPS--))

SNAPFILES=$(ls -1 --sort=time snap_* | tac)

echo "$TITLE" > "$OUTFILE"

for snap in ${SNAPFILES}
do
	echo "Processing $snap" 
	tar xzf $snap "$CURFILE"
	tail -n "$NLINES" "$CURFILE" >> "$OUTFILE"
	rm -f "$CURFILE"
done

