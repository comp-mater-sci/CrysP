#!/bin/bash

cur="$(pwd)"
data="$cur/data"
in="$data/in"
out="$data/out"
alamDMC="$cur/../VEF/release/bin/alamDMC"
conf="$cur/conf"
conf_file="$cur/config.cfg"


#setup
cp $cur/../VEF/examples/sid1687f.smt $cur



for mode in ADP ASR EWC QRS UDSA YLD
do
	mode_lower=$(echo "$mode" | tr '[:upper:]' '[:lower:]')

	for algorithm in ALAMEL FCTaylor
	do
		for slip_system in fcc12 bcc24 bcc48
		do

			cat << EOF > $conf_file
out
True
2
sid1687f.smt
$algorithm
True
$slip_system
True
True
True
EOF

			cat $conf/"$mode".cfg >> $conf_file

			echo "Testing $mode, $algorithm, $slip_system"
			time $($alamDMC $mode $conf_file > /dev/null) 
			mv out.$mode_lower $out/"$mode"_"$algorithm"_"$slip_system".txt
			rm out.*
		done
	done
done


rm $cur/config.cfg
rm $cur/sid1687f.smt




