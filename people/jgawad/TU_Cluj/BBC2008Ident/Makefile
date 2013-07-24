

FC=ifort
FCFLAGS = -fast
bbc2008ident	: bbc2008_16_parameters.for  hybrd1_lmdif1_dp.inc
	$(FC) $(FCFLAGS) -o $@  $<


clean	: 
	-rm bbc2008ident	
