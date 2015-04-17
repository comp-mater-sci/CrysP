ss12_AL_1                          Name of output files (give no extension)
..\BCCbp1.pre                             Slip system file
    2 = N-lines: controls input of critical resoved shear stresses
1.0       1.0       1.0       1.0       1.0       1.0       N-lines for + sense
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0       N-lines for - sense
1.0       1.0       1.0       1.0       1.0       1.0
    2     (MainA1) NBLOC
..\micro1.smt                                                     NAME OF MICROSTRUCTURE FILE
    2     (SIMUL) NGR : 1 for a Taylor-calculation and 2 for a ALAMEL-calculation
    1     (SIMUL) NLIST (Make an output listing 0 or 1)
    1     (SIMUL) NFILE (Make output files 0 or 1)
    0     (SIMUL) NFILTW (Make output files 0 or 1)
    1     (SIMUL) KOST If =0: TAUC are set to 1; if=1: values from FK1 used.
    1     (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
1.0       0.0       0.0       F_Microstructure
0.0       1.0       0.0       F_Microstructure
0.0       0.0       1.0       F_Microstructure
DC06F250SM0_BP
10.00     100.00    100.00      TAU-III-1	TAU-III-s	TAU-IV-s
235.88    117.94                THETA-1	THETA-T
    1     (Leesor) Type of data set for input texture (1 for SMT-file)
..\4grains.SMT                                                    NAME OF INPUT TEXTURE FILE
    1     (Leesor) Chosen Block (in input data set)
    1     (MainA) If =1: output for this block is required.
 0.0000000 0.0000000 0.0000000
 0.0100000 0.0000000 0.0000000
 0.0000000 0.0000000 0.0000000
    2     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (MainA) If =1: output for this block is required.
 0.0100000 0.0000000 0.0000000
 0.0000000 0.0000000 0.0000000
 0.0000000 0.0000000 -0.010000
    3             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (Fake call of SIMUL - for output only)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2  (Fake call of SIMUL - for output only)
