elem                       Name of output files (give no extension)
1                             (Main) KSIMTYPE (if=0 then texture calcs, if=1 then Taylorfactor calcs.)
bcc.pre                       Slip system file
16                            No. of lines with tau-crit values (Stored in FK1):
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
1.0       1.0       1.0       1.0       1.0       1.0       
  201                         (Main1) NBLOC
micro1.smt                    NAME OF MICROSTRUCTURE FILE
1                             (SIMUL) NLIST (Make an output listing 0 or 1)
1                             (SIMUL) NFILE (Make output files 0 or 1)
0                             (SIMUL) NFILTW (Make output files 0 or 1)
1                             (SIMUL) NTEN (Print distortion tensor 0 or 1)
0                             (SIMUL) NSYM If =0: TAUC are set to 1; if=1: values from FK1 used.
0                             (SIMUL) IGLIJ  0 or 1 (a print switch. Only for very short runs!)
0                             (SIMUL) IPR  0-3 Print switch. All except Van Houtte must use 0
 3.00                         Eta-Factor: Stress concentration fact. on non-deforming particle
 0.50                         Attenuation factor (on the stress concentration)
1.0       0.0       0.0       F_Microstructure
0.0       1.0       0.0       F_Microstructure
0.0       0.0       1.0       F_Microstructure
sid1655 
1.486     2.476     8.357     VOCE TAU-III-1, TAU-III-S, T-IV-S
2.750     0.550               VOCE THETA-1, THETA-T
1                             (Leesor) Type of data set for input texture (1 for SMT-file)
elem.SMT                    NAME OF INPUT TEXTURE FILE
1                             (Leesor) Chosen Block (in input data set)
0                             (Main) If =1: output for this block is required   INITIALISATION OF SG0
0.025     0.0       0.0       Displacement gradient for this block
0.0       0.0       0.0
0.0       0.0       -0.025
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL     (INITIALISATION OF SG0)
0    1                        (SIMUL) 1: relaxation allowed, for relx 1 and 2 (INITIALISATION OF SG0)
1                             (Leesor) Type of data set for input texture (1 for SMT-file)
elem.SMT                    NAME OF INPUT TEXTURE FILE
1                             (Leesor) Chosen Block (in input data set)
0                             (Main) If =1: output for this block is required. 
 0.70711   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.70711   0.00000
 0.00000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.00000   0.00000  Displacement gradient for this block
 0.00000   0.40825   0.00000
 0.00000   0.00000  -0.81650
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.00000   0.00000  Displacement gradient for this block
 0.00000   0.00000   0.70711
 0.00000   0.70711   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.00000   0.70711  Displacement gradient for this block
 0.00000   0.00000   0.00000
 0.70711   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.70711   0.00000  Displacement gradient for this block
 0.70711   0.00000   0.00000
 0.00000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.78868   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.21132   0.00000
 0.00000   0.00000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.50000   0.50000
 0.00000   0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000   0.00000   0.50000  Displacement gradient for this block
 0.00000  -0.50000   0.00000
 0.50000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000   0.50000   0.00000  Displacement gradient for this block
 0.50000  -0.50000   0.00000
 0.00000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868   0.00000   0.00000  Displacement gradient for this block
 0.00000   0.28868   0.50000
 0.00000   0.50000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868   0.00000   0.50000  Displacement gradient for this block
 0.00000   0.28868   0.00000
 0.50000   0.00000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868   0.50000   0.00000  Displacement gradient for this block
 0.50000   0.28868   0.00000
 0.00000   0.00000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.00000   0.50000  Displacement gradient for this block
 0.00000   0.00000   0.50000
 0.50000   0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.50000   0.00000  Displacement gradient for this block
 0.50000   0.00000   0.50000
 0.00000   0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.50000   0.50000  Displacement gradient for this block
 0.50000   0.00000   0.00000
 0.50000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000  -0.50000   0.00000  Displacement gradient for this block
-0.50000  -0.50000   0.00000
 0.00000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868  -0.50000   0.00000  Displacement gradient for this block
-0.50000   0.28868   0.00000
 0.00000   0.00000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000  -0.50000   0.00000  Displacement gradient for this block
-0.50000   0.00000   0.50000
 0.00000   0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000  -0.50000   0.50000  Displacement gradient for this block
-0.50000   0.00000   0.00000
 0.50000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000   0.00000  -0.50000  Displacement gradient for this block
 0.00000  -0.50000   0.00000
-0.50000   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868   0.00000  -0.50000  Displacement gradient for this block
 0.00000   0.28868   0.00000
-0.50000   0.00000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.00000  -0.50000  Displacement gradient for this block
 0.00000   0.00000   0.50000
-0.50000   0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.50000   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.50000  -0.50000
 0.00000  -0.50000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.28868   0.00000   0.00000  Displacement gradient for this block
 0.00000   0.28868  -0.50000
 0.00000  -0.50000  -0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.21132   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.78868   0.00000
 0.00000   0.00000   0.57736
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.17255   0.40825
 0.00000   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395   0.00000   0.40825  Displacement gradient for this block
 0.00000  -0.17255   0.00000
 0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395   0.40825   0.00000  Displacement gradient for this block
 0.40825  -0.17255   0.00000
 0.00000   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.00000   0.40825  Displacement gradient for this block
 0.00000  -0.40825   0.40825
 0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.40825   0.00000  Displacement gradient for this block
 0.40825  -0.40825   0.40825
 0.00000   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.40825   0.40825  Displacement gradient for this block
 0.40825  -0.40825   0.00000
 0.40825   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.00000   0.40825  Displacement gradient for this block
 0.00000   0.23570   0.40825
 0.40825   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.40825   0.00000  Displacement gradient for this block
 0.40825   0.23570   0.40825
 0.00000   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.40825   0.40825  Displacement gradient for this block
 0.40825   0.23570   0.00000
 0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.40825   0.40825  Displacement gradient for this block
 0.40825   0.00000   0.40825
 0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395  -0.40825   0.00000  Displacement gradient for this block
-0.40825  -0.17255   0.00000
 0.00000   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825  -0.40825   0.00000  Displacement gradient for this block
-0.40825  -0.40825   0.40825
 0.00000   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825  -0.40825   0.40825  Displacement gradient for this block
-0.40825  -0.40825   0.00000
 0.40825   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570  -0.40825   0.00000  Displacement gradient for this block
-0.40825   0.23570   0.40825
 0.00000   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570  -0.40825   0.40825  Displacement gradient for this block
-0.40825   0.23570   0.00000
 0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000  -0.40825   0.40825  Displacement gradient for this block
-0.40825   0.00000   0.40825
 0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395   0.00000  -0.40825  Displacement gradient for this block
 0.00000  -0.17255   0.00000
-0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.00000  -0.40825  Displacement gradient for this block
 0.00000  -0.40825   0.40825
-0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.40825  -0.40825  Displacement gradient for this block
 0.40825  -0.40825   0.00000
-0.40825   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.00000  -0.40825  Displacement gradient for this block
 0.00000   0.23570   0.40825
-0.40825   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.40825  -0.40825  Displacement gradient for this block
 0.40825   0.23570   0.00000
-0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.40825  -0.40825  Displacement gradient for this block
 0.40825   0.00000   0.40825
-0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.64395   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.17255  -0.40825
 0.00000  -0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.00000   0.40825  Displacement gradient for this block
 0.00000  -0.40825  -0.40825
 0.40825  -0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.40825   0.40825   0.00000  Displacement gradient for this block
 0.40825  -0.40825  -0.40825
 0.00000  -0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.00000   0.40825  Displacement gradient for this block
 0.00000   0.23570  -0.40825
 0.40825  -0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.23570   0.40825   0.00000  Displacement gradient for this block
 0.40825   0.23570  -0.40825
 0.00000  -0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.00000   0.40825   0.40825  Displacement gradient for this block
 0.40825   0.00000  -0.40825
 0.40825  -0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.17255   0.00000   0.00000  Displacement gradient for this block
 0.00000  -0.64395   0.40825
 0.00000   0.40825   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.17255   0.00000   0.40825  Displacement gradient for this block
 0.00000  -0.64395   0.00000
 0.40825   0.00000   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.17255   0.40825   0.00000  Displacement gradient for this block
 0.40825  -0.64395   0.00000
 0.00000   0.00000   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.23570   0.00000   0.40825  Displacement gradient for this block
 0.00000  -0.23570   0.40825
 0.40825   0.40825   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.23570   0.40825   0.00000  Displacement gradient for this block
 0.40825  -0.23570   0.40825
 0.00000   0.40825   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.23570   0.40825   0.40825  Displacement gradient for this block
 0.40825  -0.23570   0.00000
 0.40825   0.00000   0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.17255   0.00000   0.00000  Displacement gradient for this block
 0.00000   0.64395   0.40825
 0.00000   0.40825  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.17255   0.00000   0.40825  Displacement gradient for this block
 0.00000   0.64395   0.00000
 0.40825   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.17255   0.40825   0.00000  Displacement gradient for this block
 0.40825   0.64395   0.00000
 0.00000   0.00000  -0.47140
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.40825   0.00000   0.40825  Displacement gradient for this block
 0.00000   0.40825   0.40825
 0.40825   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.40825   0.40825   0.00000  Displacement gradient for this block
 0.40825   0.40825   0.40825
 0.00000   0.40825   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.40825   0.40825   0.40825  Displacement gradient for this block
 0.40825   0.40825   0.00000
 0.40825   0.00000   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.00000   0.35355  Displacement gradient for this block
 0.00000  -0.14943   0.35355
 0.35355   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.35355   0.00000  Displacement gradient for this block
 0.35355  -0.14943   0.35355
 0.00000   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.35355   0.35355  Displacement gradient for this block
 0.35355  -0.14943   0.00000
 0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355   0.35355   0.35355  Displacement gradient for this block
 0.35355  -0.35355   0.35355
 0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412   0.35355   0.35355  Displacement gradient for this block
 0.35355   0.20412   0.35355
 0.35355   0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768  -0.35355   0.00000  Displacement gradient for this block
-0.35355  -0.14943   0.35355
 0.00000   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768  -0.35355   0.35355  Displacement gradient for this block
-0.35355  -0.14943   0.00000
 0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355  -0.35355   0.35355  Displacement gradient for this block
-0.35355  -0.35355   0.35355
 0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412  -0.35355   0.35355  Displacement gradient for this block
-0.35355   0.20412   0.35355
 0.35355   0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.00000  -0.35355  Displacement gradient for this block
 0.00000  -0.14943   0.35355
-0.35355   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.35355  -0.35355  Displacement gradient for this block
 0.35355  -0.14943   0.00000
-0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355   0.35355  -0.35355  Displacement gradient for this block
 0.35355  -0.35355   0.35355
-0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412   0.35355  -0.35355  Displacement gradient for this block
 0.35355   0.20412   0.35355
-0.35355   0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.00000   0.35355  Displacement gradient for this block
 0.00000  -0.14943  -0.35355
 0.35355  -0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.35355   0.00000  Displacement gradient for this block
 0.35355  -0.14943  -0.35355
 0.00000  -0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355   0.35355   0.35355  Displacement gradient for this block
 0.35355  -0.35355  -0.35355
 0.35355  -0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412   0.35355   0.35355  Displacement gradient for this block
 0.35355   0.20412  -0.35355
 0.35355  -0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943   0.00000   0.35355  Displacement gradient for this block
 0.00000  -0.55768   0.35355
 0.35355   0.35355   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943   0.35355   0.00000  Displacement gradient for this block
 0.35355  -0.55768   0.35355
 0.00000   0.35355   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943   0.35355   0.35355  Displacement gradient for this block
 0.35355  -0.55768   0.00000
 0.35355   0.00000   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.20412   0.35355   0.35355  Displacement gradient for this block
 0.35355  -0.20412   0.35355
 0.35355   0.35355   0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943   0.00000   0.35355  Displacement gradient for this block
 0.00000   0.55768   0.35355
 0.35355   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943   0.35355   0.00000  Displacement gradient for this block
 0.35355   0.55768   0.35355
 0.00000   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943   0.35355   0.35355  Displacement gradient for this block
 0.35355   0.55768   0.00000
 0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.35355   0.35355   0.35355  Displacement gradient for this block
 0.35355   0.35355   0.35355
 0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768  -0.35355  -0.35355  Displacement gradient for this block
-0.35355  -0.14943   0.00000
-0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355  -0.35355  -0.35355  Displacement gradient for this block
-0.35355  -0.35355   0.35355
-0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412  -0.35355  -0.35355  Displacement gradient for this block
-0.35355   0.20412   0.35355
-0.35355   0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768  -0.35355   0.00000  Displacement gradient for this block
-0.35355  -0.14943  -0.35355
 0.00000  -0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.35355  -0.35355   0.35355  Displacement gradient for this block
-0.35355  -0.35355  -0.35355
 0.35355  -0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.20412  -0.35355   0.35355  Displacement gradient for this block
-0.35355   0.20412  -0.35355
 0.35355  -0.35355  -0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943  -0.35355   0.00000  Displacement gradient for this block
-0.35355  -0.55768   0.35355
 0.00000   0.35355   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943  -0.35355   0.35355  Displacement gradient for this block
-0.35355  -0.55768   0.00000
 0.35355   0.00000   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.20412  -0.35355   0.35355  Displacement gradient for this block
-0.35355  -0.20412   0.35355
 0.35355   0.35355   0.40824
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943  -0.35355   0.00000  Displacement gradient for this block
-0.35355   0.55768   0.35355
 0.00000   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943  -0.35355   0.35355  Displacement gradient for this block
-0.35355   0.55768   0.00000
 0.35355   0.00000  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.35355  -0.35355   0.35355  Displacement gradient for this block
-0.35355   0.35355   0.35355
 0.35355   0.35355   0.00000
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.55768   0.00000  -0.35355  Displacement gradient for this block
 0.00000  -0.14943  -0.35355
-0.35355  -0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14943   0.00000  -0.35355  Displacement gradient for this block
 0.00000  -0.55768   0.35355
-0.35355   0.35355   0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14943   0.00000  -0.35355  Displacement gradient for this block
 0.00000   0.55768   0.35355
-0.35355   0.35355  -0.40825
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880   0.31623   0.31623  Displacement gradient for this block
 0.31623  -0.13365   0.31623
 0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880  -0.31623   0.31623  Displacement gradient for this block
-0.31623  -0.13365   0.31623
 0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880   0.31623  -0.31623  Displacement gradient for this block
 0.31623  -0.13365   0.31623
-0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880   0.31623   0.31623  Displacement gradient for this block
 0.31623  -0.13365  -0.31623
 0.31623  -0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.13365   0.31623   0.31623  Displacement gradient for this block
 0.31623  -0.49880   0.31623
 0.31623   0.31623   0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.13365   0.31623   0.31623  Displacement gradient for this block
 0.31623   0.49880   0.31623
 0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880  -0.31623  -0.31623  Displacement gradient for this block
-0.31623  -0.13365   0.31623
-0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880  -0.31623   0.31623  Displacement gradient for this block
-0.31623  -0.13365  -0.31623
 0.31623  -0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.13365  -0.31623   0.31623  Displacement gradient for this block
-0.31623  -0.49880   0.31623
 0.31623   0.31623   0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.13365  -0.31623   0.31623  Displacement gradient for this block
-0.31623   0.49880   0.31623
 0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.49880   0.31623  -0.31623  Displacement gradient for this block
 0.31623  -0.13365  -0.31623
-0.31623  -0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.13365   0.31623  -0.31623  Displacement gradient for this block
 0.31623  -0.49880   0.31623
-0.31623   0.31623   0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.13365   0.31623  -0.31623  Displacement gradient for this block
 0.31623   0.49880   0.31623
-0.31623   0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.13365   0.31623   0.31623  Displacement gradient for this block
 0.31623  -0.49880  -0.31623
 0.31623  -0.31623   0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.13365   0.31623   0.31623  Displacement gradient for this block
 0.31623   0.49880  -0.31623
 0.31623  -0.31623  -0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.49880   0.31623   0.31623  Displacement gradient for this block
 0.31623   0.13365   0.31623
 0.31623   0.31623   0.36515
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.58835   0.19612  Displacement gradient for this block
 0.58835  -0.08289   0.19612
 0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612   0.58835  Displacement gradient for this block
 0.19612  -0.08289   0.19612
 0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.08289   0.58835
 0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.14357   0.19612
 0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.47512   0.19612
 0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.58835   0.19612  Displacement gradient for this block
-0.58835  -0.08289   0.19612
 0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612   0.58835  Displacement gradient for this block
-0.19612  -0.08289   0.19612
 0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.08289   0.58835
 0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580  -0.19612   0.19612  Displacement gradient for this block
-0.19612   0.14357   0.19612
 0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.47512   0.19612
 0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.58835  -0.19612  Displacement gradient for this block
 0.58835  -0.08289   0.19612
-0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612  -0.58835  Displacement gradient for this block
 0.19612  -0.08289   0.19612
-0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.08289   0.58835
-0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580   0.19612  -0.19612  Displacement gradient for this block
 0.19612   0.14357   0.19612
-0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.47512   0.19612
-0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.58835   0.19612  Displacement gradient for this block
 0.58835  -0.08289  -0.19612
 0.19612  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612   0.58835  Displacement gradient for this block
 0.19612  -0.08289  -0.19612
 0.58835  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.08289  -0.58835
 0.19612  -0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.14357  -0.19612
 0.19612  -0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.47512  -0.19612
 0.19612  -0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.58835   0.19612  Displacement gradient for this block
 0.58835  -0.30934   0.19612
 0.19612   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612   0.58835  Displacement gradient for this block
 0.19612  -0.30934   0.19612
 0.58835   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.30934   0.58835
 0.19612   0.58835   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14357   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.53580   0.19612
 0.19612   0.19612   0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.47512   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.70158   0.19612
 0.19612   0.19612   0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.58835   0.19612  Displacement gradient for this block
 0.58835   0.30934   0.19612
 0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612   0.58835  Displacement gradient for this block
 0.19612   0.30934   0.19612
 0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.30934   0.58835
 0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14357   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.53580   0.19612
 0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.47512   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.70158   0.19612
 0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.58835  -0.19612  Displacement gradient for this block
-0.58835  -0.08289   0.19612
-0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612  -0.58835  Displacement gradient for this block
-0.19612  -0.08289   0.19612
-0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612  -0.19612  Displacement gradient for this block
-0.19612  -0.08289   0.58835
-0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580  -0.19612  -0.19612  Displacement gradient for this block
-0.19612   0.14357   0.19612
-0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158  -0.19612  -0.19612  Displacement gradient for this block
-0.19612  -0.47512   0.19612
-0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.58835   0.19612  Displacement gradient for this block
-0.58835  -0.08289  -0.19612
 0.19612  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612   0.58835  Displacement gradient for this block
-0.19612  -0.08289  -0.19612
 0.58835  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.08289  -0.58835
 0.19612  -0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580  -0.19612   0.19612  Displacement gradient for this block
-0.19612   0.14357  -0.19612
 0.19612  -0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.47512  -0.19612
 0.19612  -0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289  -0.58835   0.19612  Displacement gradient for this block
-0.58835  -0.30934   0.19612
 0.19612   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289  -0.19612   0.58835  Displacement gradient for this block
-0.19612  -0.30934   0.19612
 0.58835   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.30934   0.58835
 0.19612   0.58835   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14357  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.53580   0.19612
 0.19612   0.19612   0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.47512  -0.19612   0.19612  Displacement gradient for this block
-0.19612  -0.70158   0.19612
 0.19612   0.19612   0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289  -0.58835   0.19612  Displacement gradient for this block
-0.58835   0.30934   0.19612
 0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289  -0.19612   0.58835  Displacement gradient for this block
-0.19612   0.30934   0.19612
 0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289  -0.19612   0.19612  Displacement gradient for this block
-0.19612   0.30934   0.58835
 0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14357  -0.19612   0.19612  Displacement gradient for this block
-0.19612   0.53580   0.19612
 0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.47512  -0.19612   0.19612  Displacement gradient for this block
-0.19612   0.70158   0.19612
 0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.58835  -0.19612  Displacement gradient for this block
 0.58835  -0.08289  -0.19612
-0.19612  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612  -0.58835  Displacement gradient for this block
 0.19612  -0.08289  -0.19612
-0.58835  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.30934   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.08289  -0.58835
-0.19612  -0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.53580   0.19612  -0.19612  Displacement gradient for this block
 0.19612   0.14357  -0.19612
-0.19612  -0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.70158   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.47512  -0.19612
-0.19612  -0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.58835  -0.19612  Displacement gradient for this block
 0.58835  -0.30934   0.19612
-0.19612   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612  -0.58835  Displacement gradient for this block
 0.19612  -0.30934   0.19612
-0.58835   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.30934   0.58835
-0.19612   0.58835   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14357   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.53580   0.19612
-0.19612   0.19612   0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.47512   0.19612  -0.19612  Displacement gradient for this block
 0.19612  -0.70158   0.19612
-0.19612   0.19612   0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.58835  -0.19612  Displacement gradient for this block
 0.58835   0.30934   0.19612
-0.19612   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612  -0.58835  Displacement gradient for this block
 0.19612   0.30934   0.19612
-0.58835   0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612  -0.19612  Displacement gradient for this block
 0.19612   0.30934   0.58835
-0.19612   0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14357   0.19612  -0.19612  Displacement gradient for this block
 0.19612   0.53580   0.19612
-0.19612   0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.47512   0.19612  -0.19612  Displacement gradient for this block
 0.19612   0.70158   0.19612
-0.19612   0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.58835   0.19612  Displacement gradient for this block
 0.58835  -0.30934  -0.19612
 0.19612  -0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612   0.58835  Displacement gradient for this block
 0.19612  -0.30934  -0.19612
 0.58835  -0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.08289   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.30934  -0.58835
 0.19612  -0.58835   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.14357   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.53580  -0.19612
 0.19612  -0.19612   0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.47512   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.70158  -0.19612
 0.19612  -0.19612   0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.58835   0.19612  Displacement gradient for this block
 0.58835   0.30934  -0.19612
 0.19612  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612   0.58835  Displacement gradient for this block
 0.19612   0.30934  -0.19612
 0.58835  -0.19612  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.08289   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.30934  -0.58835
 0.19612  -0.58835  -0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
 0.14357   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.53580  -0.19612
 0.19612  -0.19612  -0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.47512   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.70158  -0.19612
 0.19612  -0.19612  -0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.30934   0.58835   0.19612  Displacement gradient for this block
 0.58835   0.08289   0.19612
 0.19612   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.30934   0.19612   0.58835  Displacement gradient for this block
 0.19612   0.08289   0.19612
 0.58835   0.19612   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.30934   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.08289   0.58835
 0.19612   0.58835   0.22645
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.53580   0.19612   0.19612  Displacement gradient for this block
 0.19612  -0.14357   0.19612
 0.19612   0.19612   0.67937
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
0                             (Main) If =1: output for this block is required. 
-0.70158   0.19612   0.19612  Displacement gradient for this block
 0.19612   0.47512   0.19612
 0.19612   0.19612   0.22646
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL (This is for a true simulation)
0    0                        (SIMUL) 1: relaxation, allowed for relx 1 and 2 (This is for a true simulation)
1                             (SIMUL) NUMBER OF SIMULATION STEPS PER CALL     (Fake call of SIMUL, for output only)
0    0                        (SIMUL) 1: relaxation allowed, for relx 1 and 2 (Fake call of SIMUL, for output only)
