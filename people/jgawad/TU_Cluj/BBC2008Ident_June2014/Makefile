

FC=ifort
FCFLAGS = -fpp -fast -DDIAGNOSTIC_OUTPUT_FILE -DNEW_IDENT_SCHEME
bbc2008ident	: bbc2008_16_parameters.o  hybrd1_lmdif1_dp.o extendedYld.o
	$(FC) $(FCFLAGS) -o $@  $^

extendedYld.o	: extendedYld.f90
	$(FC) $(FCFLAGS) -c $< 

bbc2008_16_parameters.o: bbc2008_16_parameters.for extendedYld.o hybrd1_lmdif1_dp.o
	$(FC) $(FCFLAGS) -c $< 

hybrd1_lmdif1_dp.o: hybrd1_lmdif1_dp.for
	$(FC) $(FCFLAGS) -c $< 


clean	: 
	-rm bbc2008ident *.mod *.o	
