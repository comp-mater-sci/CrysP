#!/bin/bash
for i in `seq 0 50` ; 
do 
	#sed -e 's/%%BoundingBox: 0 0 980 424/%%BoundingBox: 0 0 500 424/' frame_${i}.eps > xframe_${i}.eps  
	sed -e 's/%%BoundingBox: 0 0 1013 523/%%BoundingBox: 0 0 500 522/' frame_${i}.eps > xframe_${i}.eps  
	ps2pdf -dEPSCrop xframe_$i.eps  frame_$i.pdf
	rm xframe_${i}.eps
done



