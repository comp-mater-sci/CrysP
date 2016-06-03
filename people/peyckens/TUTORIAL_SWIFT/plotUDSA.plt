####OUTPUT TYPE:
set term windows
#960,640
#640,480
set term png font "arial,28" size 1000,1000;
set key off 

set xlabel "tensile strain"
set ylabel "tensile stress, [MPa]"

set xrange [0:0.25]
set xtics 0.05  

set yrange [0:300.]
set ytics 50.     

#key options: top bottom left right box
set key right bottom inside


set datafile commentschar "#%i"

# This sets up bounding boxes and may be required on some terminals
set size 1,1
set origin 0,0

set grid

###
set output "DX51D_uni0_exp.png"  
#set title ""
plot \
  "DX51D_uni0_exp.txt"	with points pt 5 ps 3.0 lw 2. title "Experimental fitting data" 
  
###
set output "DX51D_uni0_exp-fit.png"  
#set title ""
plot \
  "DX51D_uni0_exp.txt"	     with points pt 5 ps 3.0 lw 2. title "Experimental fitting data", \
  "sid1687f_udsa_SwiftS.uds" using 4:5 with l lw 3. title "VEF-UDSA verification simulation"


# remove all customization
reset  

replot
pause
