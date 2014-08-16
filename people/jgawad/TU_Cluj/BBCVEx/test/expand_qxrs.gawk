BEGIN { OFMT = "%16.9E" }
{
if (NR == 1) { print $0, "\t", NF ,    "\t", NF+1,   "\t", NF+2 ; }
if (NR == 2) { print $0, "\t", "q/q0", "\t", "r/r0", "\t", "s/s0"; } 
if (NR == 3) {q0 = $3; r0 = $4; s0 = $5; }
if (NR >= 3) { print $0, "\t", ($3 / q0), "\t", ($4 / r0), "\t", ($5 / s0) ; } 
}