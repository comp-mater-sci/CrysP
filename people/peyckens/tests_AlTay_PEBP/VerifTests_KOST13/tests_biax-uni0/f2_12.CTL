f2_12                          Name of output files (give no extension)
..\BCCbp2.pre                             Slip system file
    2 = N-lines: controls input of critical resoved shear stresses
1.0       1.0       1.0       1.0       1.0       1.0       N-lines for + sense
1.0       1.0       1.0       1.0       1.0       1.0
1.0       1.0       1.0       1.0       1.0       1.0       N-lines for - sense
1.0       1.0       1.0       1.0       1.0       1.0
    2     (MainA1) NBLOC
..\micro1.smt                                                     NAME OF MICROSTRUCTURE FILE
    2     (SIMUL) NGR : 1 for a Taylor-calculation and 2 for a ALAMEL-calculation
  1.0     (SIMUL) ENTA (fix to the config file: jg)
    1     (SIMUL) NLIST (Make an output listing 0 or 1)
    1     (SIMUL) NFILE (Make output files 0 or 1)
    0     (SIMUL) NFILTW (Make output files 0 or 1)
   12     (SIMUL) KOST If =0: TAUC are set to 1; if=1: values from FK1 used.
    0     (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
1.0       0.0       0.0       F_Microstructure
0.0       1.0       0.0       F_Microstructure
0.0       0.0       1.0       F_Microstructure
DC06F250SM0_BP
2.48E-10               b    [m]    Magnitude of burgers vector
8.16E4              G    [MPa]    Shear modulus
  0.20              alpha    [/]    Dislocation interaction parameter
  0.20               f    [/]    Volume fraction of CBBs (Cell Block Boundaries)
5.44E+01                  tau_0 [MPa]  Initial critical resolved shear stress on all
2.63E-02                  I     [/]    immobilization CBs
1.09E-09                  R     [m]    recovery CBs
6.06E-01                  I_wd  [/]    immobilization CBBs
7.64E-09                  R_wd  [m]    recovery CBBs
5.95E-09                  R_ncg [m]    recovery of old CBBs and polarity of old CBBs
1.42E+01                  beta_1[/]    1st coeff. micro shear band cut-through of old
1.77E-02                  beta_2[/]    2nd coeff. micro shear band cut-through of old
0.0                 I_wp    [/]    immobilization polarity CBBs
1.0E-15                 R_wp    [m]    recovery polarity CBBs
1.0E-15                 R_rev    [m]    recovery of polarity CBBs during bauschinger
1.0E-15                 R_2    [m]    recovery of CBs due to reversal polarity flux
    1     (Leesor) Type of data set for input texture (1 for SMT-file)
..\DC06F250.SMT                                                    NAME OF INPUT TEXTURE FILE
    1     (Leesor) Chosen Block (in input data set)
F    -1  xxx.BPM      (MainA) Flag: read state (L02); chosen block in the state file (I5); file name (A)
    1     (MainA) If =1: output for this block is required.
 0.0050000 0.0000000 0.0000000
 0.0000000 0.0050000 0.0000000
 0.0000000 0.0000000 -0.010000
   20     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (MainA) If =1: output for this block is required.
 0.0100000 0.0000000 0.0000000
 0.0000000 -0.005000 0.0000000
 0.0000000 0.0000000 -0.005000
   30             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (This is for a true simulation)
    1    1(SIMUL) 1: relaxation allowed, for relx 1 and 2  (This is for a true simulation)
    1     (SIMUL) NUMBER OF SIMULATION STEPS PER CALL      (Fake call of SIMUL - for output only)
    0    0(SIMUL) 1: relaxation allowed, for relx 1 and 2  (Fake call of SIMUL - for output only)
