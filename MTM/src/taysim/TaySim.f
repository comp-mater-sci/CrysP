C*******************************************************************************
C
      PROGRAM TAYSIM
C
C     This program generates deformed texture (new set of C-coefficients) after 
C     appplying prescribed deformation on the initial texture (input C-coeffs)
C     according to the relaxations specified.
C      
C     Reads the parameter file: taysim.par.    
C
C
C
      IMPLICIT NONE
C
      CHARACTER parsimf*40              ! TAYSIM parameter filename 
      CHARACTER inppref*8               ! Prefix for output file names
      INTEGER crys                      ! Index for type of crystal structure 
      INTEGER typrel                    ! Index for type of relaxations 
      CHARACTER outpref*8               ! Prefix for output file names
      CHARACTER outinfo*40              ! Info to put in output files
	INTEGER imag, idn                 ! Sample symmetry parameters
	DOUBLE PRECISION deps             ! Delta epsilon: macroscopic strain 
      INTEGER nsteps                    ! Number of deformation steps
	DOUBLE PRECISION dfm(3,3)         ! Prescribed deformation
	INTEGER idm                       ! Symmetry parameter: 
C                                         (=4 for cubic; =6 for hexagonal metals) 
      DOUBLE PRECISION phi0             ! Gaussian spread for output texture
C                                         (default= 7.00 degrees)
C
	CHARACTER inputCf*40              ! Input C-coefficient file (bin)
	CHARACTER taybatf*40              ! Batch file to execute TAYLORS (ascii)
 	CHARACTER reabatf*40              ! Batch file to execute READTXT (ascii)
	CHARACTER opstr*1                 ! Reads operating system code
	INTEGER opcode                    ! Reads operating system code 
	CHARACTER cmdtxt*80               ! Command to execute batch file
C
	INTEGER lmax                      ! Order of series expansion  
      INTEGER nrun                      ! Number of TAYLOR runs (default: 1)
      INTEGER u0, u1, u2                ! Channels for file I/O
	PARAMETER (u0=900, u1=901, u2=902, nrun=1)
      INTEGER fsta
C
C     Checking the input argument for the program
C
      CALL GETARG(1,parsimf)
      IF (LEN_TRIM(parsimf) == 0) THEN
        PRINT '(A)', '  '
        PRINT '(A)', 'INPUT ERROR: PARAMETER FILE NAME NOT ENTERED'
        PRINT '(A)', '  '
        PRINT '(A)','USE: taysim <taysim parameter filename> <OS code>'
        PRINT '(A)', '  '
        PRINT '(A)', '     OS code =1 for Windows, =2 for Linux'
        PRINT '(A)', '  '
        PRINT '(A)','*** ABNORMAL PROGRAM TERMINATION ***' 
        PRINT*, ' '
        STOP
      END IF
	CALL GETARG(2,opstr)
      IF (LEN_TRIM(opstr) == 0) THEN
        PRINT '(A)', '  '
        PRINT '(A)', 'INPUT ERROR: PARAMETER FILE NAME NOT ENTERED'
        PRINT '(A)', '  '
        PRINT '(A)','USE: taysim <taysim parameter filename> <OS code>'
        PRINT '(A)', '  '
        PRINT '(A)', '     OS code =1 for Windows, =2 for Linux'
        PRINT '(A)', '  '
        PRINT '(A)','*** ABNORMAL PROGRAM TERMINATION ***' 
        PRINT*, ' '
        STOP
      END IF
	READ(opstr,'(I1)') opcode
C
C     Checking the existence of the parameter file of this program
C
      OPEN(u0,FILE=parsimf,ACTION='READ',STATUS='OLD',IOSTAT=fsta)
      IF (fsta /= 0) THEN
        PRINT*, ' '
        PRINT '(3A)', 'READ ERROR: FILE ', TRIM(parsimf),
     &                ' DOES NOT EXIST. PLEASE CHECK IF '
        PRINT '(2A)', 'THE FILENAME IS SPELLED CORRECTLY AND ',
     &                'IS PROPERLY CASED.'
        PRINT*,' ' 
        PRINT '(A)','*** ABNORMAL PROGRAM TERMINATION ***' 
        PRINT*, ' '
        STOP
      END IF
C
C     Reading the parameter file contents
C
      CALL RDLINES (u0, 6)
      READ (u0,'(62X,A8)') inppref
	READ (u0,'(62X,I2)') crys
	READ (u0,'(62X,I2)') typrel
      READ (u0,'(62X,A8)') outpref
      READ (u0,'(62X,A40)') outinfo
      READ (u0,'(62X,I2,2X,I2)') imag, idn
	READ (u0,'(62X,F8.5)') deps 
	READ (u0,'(62X,I2)') nsteps
      READ (u0,'(62X,3(F9.5,1X))') dfm(1,1), dfm(1,2), dfm(1,3)
	READ (u0,'(62X,3(F9.5,1X))') dfm(2,1), dfm(2,2), dfm(2,3)
	READ (u0,'(62X,3(F9.5,1X))') dfm(3,1), dfm(3,2), dfm(3,3)
      READ (u0,'(62X,I2,1X,F5.2)') idm, phi0
	CLOSE (u0) 
C
      inputCf = TRIM(inppref) // '.C'
      taybatf = TRIM(inppref) // '.bat'
      reabatf = TRIM(outpref) // '.bat'
C
C     Finding the lmax associated with the input C-coefficients
C     This defines the use of appropriate libraries for further steps.
C      
      CALL GETLMAXFEM (inputCf, lmax, opcode)
C
C     Preprocessing for TAYLORS simulation
C
      CALL PRETAYSIM (inppref,outpref,taybatf,outinfo,deps,dfm,
     &                crys,typrel,imag,idn,lmax,nrun,nsteps,opcode)
C
C     Execution of TAYLORS according to Operating system specified
C
      IF (opcode==1) THEN
	  cmdtxt = TRIM(taybatf) // ' > ' // TRIM(inppref) // '.log' 
	  CALL SYSTEM(cmdtxt)   
	  cmdtxt = 'del ' // TRIM(inppref) // '.log' 
        CALL SYSTEM(cmdtxt)   
      ELSE IF (opcode==2) THEN
        cmdtxt = 'chmod 777 ' // TRIM(taybatf) 
	  CALL SYSTEM(cmdtxt)   
	  cmdtxt = './' // TRIM(taybatf)//' > '// TRIM(inppref)//'.log' 
        CALL SYSTEM(cmdtxt)   
	  cmdtxt = 'rm -rf ' // TRIM(inppref) // '.log' 
        CALL SYSTEM(cmdtxt)   
	END IF  
C
C     Postprocessing of TAYLORS simulation
C
      CALL POSTAYSIM (outpref,reabatf,outinfo,imag,idn,lmax,
     &                      nrun,nsteps,idm,phi0,opcode)
C
C     Execution of READTXT, SMTODF according to Operating system specified
C
      IF (opcode==1) THEN
	  cmdtxt = TRIM(reabatf) // ' > ' // TRIM(outpref) // '.log' 
	  CALL SYSTEM(cmdtxt)   
	  cmdtxt = 'del ' // TRIM(outpref) // '.log' 
        CALL SYSTEM(cmdtxt)   
      ELSE IF (opcode==2) THEN
        cmdtxt = 'chmod 777 ' // TRIM(reabatf) 
	  CALL SYSTEM(cmdtxt)   
	  cmdtxt = './' // TRIM(reabatf)//' > '// TRIM(outpref)//'.log' 
        CALL SYSTEM(cmdtxt)   
	  cmdtxt = 'rm -rf ' // TRIM(outpref) // '.log' 
        CALL SYSTEM(cmdtxt)   
	END IF
C  
 	END PROGRAM TAYSIM
C
C*******************************************************************************
C
C********************* BEGIN OF SUBROUTINE GETLMAXFEM **************************
C
      SUBROUTINE GETLMAXFEM(texfile, lmax, opcode) 
C
C     This reads the LMAX associated with the given C-coefficients file.
C     This is required during discretisation procedure.
C
      CHARACTER texfile*40, cmdtxt*60, buffer*6
	INTEGER lmax, opcode, u0
	PARAMETER (u0=126)
C
      IF (opcode==1) 
     &  cmdtxt = 'PRINTC.EXE ' // TRIM(texfile) // ' > getlmax.log'
      IF (opcode==2) 
     &  cmdtxt = './PRINTC.EXE ' // TRIM(texfile) // ' > getlmax.log'
C
      CALL SYSTEM(cmdtxt)
	OPEN(u0,FILE='PRINTC.L01',ACTION='READ',STATUS='OLD')
        READ(u0,'(A6)') buffer
        READ(u0,'(A6,I6)') buffer, lmax
	CLOSE(u0)
C
      RETURN
	END SUBROUTINE GETLMAXFEM
C
C********************** END OF SUBROUTINE GETLMAXFEM ***************************
C
C********************* BEGIN OF SUBROUTINE PRETAYSIM ***************************
C
      SUBROUTINE PRETAYSIM (inppref,outpref,taybatf,outinfo,deps,dfm,
     &                  crys,typrel,imag,idn,lmax,nrun,nsteps,opcode)
C
C     Objective: Preprocessing before TAYLORS simulation
C
C         - Creates the TAYLOR control file
C         - Creates the needed files for discretisation 
C                (rottex.i01, pltodf_c.i01, odftay.i01)
C         - Creates the batch file for all executions
C
C         Here, the PATHS are removed and this means that .b01, .b02, and .b04 
C         files are to be placed in current working directory.
C
      IMPLICIT NONE
C
      CHARACTER inppref*8, outpref*8, taybatf*40, outinfo*40
      INTEGER crys, typrel, imag, idn, nrun, nsteps, lmax, opcode                      
	DOUBLE PRECISION deps, dfm(3,3)
C
      CHARACTER inputCf*40              ! Input C-Coefficients filename (bin)
	CHARACTER inpcoef*40              ! Input C-Coefficients filename (bin)
C                                  (after symmetry applied based on imag,idn)
	CHARACTER inpaodf*40              ! Input C-Coefficients AODF (ascii)
	CHARACTER inptx0f*40              ! Discretised Input texture TX0 (ascii)
	CHARACTER inpsmtf*40              ! Discretised Input texture SMT (ascii)
	CHARACTER tayctlf*40              ! Control file for TAYLORS CTL (ascii)
      CHARACTER inpi01f*40, inpl01f*40  ! .i01 & .l01 file for rottex.exe
      CHARACTER inpi02f*40, inpl02f*40  ! .i01 & .l01 file for odftay.exe
      CHARACTER inpi03f*40, inpl03f*40  ! .i01 & .l01 file for calcodf.exe
	CHARACTER outtx1f*40              ! Discretised output texture TX1 (bin)
C
      INTEGER u0, u1, u2, u3, u4        ! Channels for file I/O
      PARAMETER (u0=903, u1=904, u2=905, u3=906, u4=907)
C
C     Intialising all filenames
C
	tayctlf = 'TAYLORS.CTL'
      inputCf = TRIM(inppref) // '.C'
	inpcoef = TRIM(inppref) // '.sC'
      inpaodf = TRIM(inppref) // '.aof'
	inptx0f = TRIM(inppref) // '.tx0' 
	inpsmtf = TRIM(inppref) // '.smt'
      inpi01f = TRIM(inppref) // '.i01'
      inpi02f = TRIM(inppref) // '.i02'
      inpi03f = TRIM(inppref) // '.i03'
      inpl01f = TRIM(inppref) // '.l01'
      inpl02f = TRIM(inppref) // '.l02'
      inpl03f = TRIM(inppref) // '.l03'
	outtx1f = TRIM(outpref) // '.tx1'
C
C     Writing Control file for TAYLORS program
C
      OPEN(u0,FILE=tayctlf,STATUS='REPLACE',ACTION='WRITE')
      WRITE(u0,'(A,A)')'Put name of file that contains crystal', 
     &                ' data on next line:'
      IF(crys==1) WRITE(u0,'(A)')'bcc.dat' 
      IF(crys==2) WRITE(u0,'(A)')'fcc.dat' 
      IF(crys==3) WRITE(u0,'(A)')'fcct.dat'
      WRITE(u0,'(A)') '0  0  0         LISTING CONTROL PARAMETERS' 
      WRITE(u0,'(A)') '0 0 0.0 0.0     CRSS VALUES (END)'
      WRITE(u0,'(A)') '0               CRSS-SWITCH'
      WRITE(u0,'(A)') '0.01            STRAIN RATE EXPONENT M'
      WRITE(u0,'(A)') '1.000           REFERENCE SLIP RATE GAMMA_DOT_0'
      WRITE(u0,'(2A)') '0               TYPE OF INPUT-TEXTURE-Put',  
     &                 ' filename on next line:'
      WRITE(u0,'(A)') TRIM(inptx0f)
      WRITE(u0,'(A8,A)')outpref,'        CODE TO BE PUT IN OUTPUT-FILE'
      WRITE(u0,'(I4,12X,A)') nrun,'NUMBER OF RUNS TO BE PERFORMED'
      WRITE(u0,'(2A)') '0               IF =1 : Put name of file with', 
     &                 ' distortions on next line'
      WRITE(u0,'(2A)') '1               FILE CONTROL (IF =1 : Put', 
     &                 ' filename on next line)'
      WRITE(u0,'(A)') TRIM(outtx1f)
      WRITE(u0,'(A40)') outinfo
	WRITE(u0,'(A)')'1               INPUT TEXTURE FILE: CHOSEN RUN'
	WRITE(u0,'(A)')'0                                   CHOSEN STEP'
	WRITE(u0,'(I4,12X,A)') nsteps, 'NUMBER OF STEPS TO BE PERFORMED'
      IF (typrel==1) THEN
        WRITE(u0,'(A)')'1               NUMBER OF RELAXATIONS'
        WRITE(u0,'(A)')'0.0  0.0  1.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
      ELSE IF (typrel==2) THEN
        WRITE(u0,'(A)')'2               NUMBER OF RELAXATIONS'
        WRITE(u0,'(A)')'0.0  0.0  1.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0  1.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
      ELSE IF (typrel==3) THEN
        WRITE(u0,'(A)')'2               NUMBER OF RELAXATIONS'
        WRITE(u0,'(A)')'1.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0 -1.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
        WRITE(u0,'(A)')'0.0  0.5  0.0'
        WRITE(u0,'(A)')'0.5  0.0  0.0'
        WRITE(u0,'(A)')'0.0  0.0  0.0'
      ELSE  
        WRITE(u0,'(A)')'0               NUMBER OF RELAXATIONS'
      END IF
      WRITE(u0,'(F8.5,8X,2A)') deps, 'DELTA EPSILON(if zero:only', 
     &                  ' Taylor factor calculation)'
      WRITE(u0,'(3(F9.5,1X))') dfm(1,1), dfm(1,2), dfm(1,3)
      WRITE(u0,'(3(F9.5,1X))') dfm(2,1), dfm(2,2), dfm(2,3)
      WRITE(u0,'(3(F9.5,1X))') dfm(3,1), dfm(3,2), dfm(3,3)
      CLOSE(u0)
C
C     Writing .i01 file for ROTTEX.EXE. The symmetry is applied
C       based on imag and idn. No rotation is applied however. 
C
      OPEN (u1,FILE=inpi01f,ACTION='WRITE',STATUS='REPLACE')
        WRITE(u1,'(2A)') '00.0        00.0      00.00',
     &                   '               PHI1/PHI/PHI2'
        WRITE(u1,'(I5,I5,32X,A)') imag, idn,
     &                        'IMPOSED IMAG, IMPOSED IDN'
        WRITE(u1,'(2A,I5,I5)') TRIM(inpcoef),'->(IMAG,IDN)=', imag, idn
      CLOSE (u1)
C
C     Writing .i01 file for CALCODF.EXE
C
      OPEN (u2,FILE=inpi02f,ACTION='WRITE',STATUS='REPLACE')
        WRITE(u2,'(I5,20X,A)') 0,'IEVOD: if =0: Ordinary case'
        WRITE(u2,'(32X,A)') 'if =1: Only ODD part of the ODF'
        WRITE(u2,'(32X,A)') 'if =2: Only EVEN part of the ODF'
      CLOSE (u2)
C
C     Writing .i01 file for ODFTAY.EXE
C
      OPEN (u3,FILE=inpi03f,ACTION='WRITE',STATUS='REPLACE')
        WRITE(u3,'(A,11X,A)') ' 5000',
     &                        'Number of Selectors (Max. 5000)'
      CLOSE (u3)
C
C     Writing the batch file to execute ROTTEX, CALCODF, ODFTAY & TAYLORS.EXEs
C
      OPEN (u4,FILE=taybatf,ACTION='WRITE',STATUS='REPLACE')
	IF (opcode==1) THEN
        WRITE(u4,'(2A,1X,2A,1X,A,1X,A)')'ROTTEX.EXE ',TRIM(inpi01f),
     &      TRIM(inpl01f),' wagner.b04 ',TRIM(inputCf),TRIM(inpcoef)
        IF (lmax<=22) 
     &    WRITE(u4,'(4A,1X,A)') 'CALCODF.EXE ', TRIM(inpi02f),
     &      ' create6.b01 create6.b02 ',TRIM(inpcoef),TRIM(inpaodf)
        IF (lmax>22) 
     &    WRITE(u4,'(4A,1X,A)') 'CALCODF.EXE ', TRIM(inpi02f),
     &      ' create5.b01 create5.b02 ', TRIM(inpcoef),TRIM(inpaodf)
        WRITE(u4,'(A,5(A,1X))') 'ODFTAY.EXE ', TRIM(inpi03f),
     &      TRIM(inpl03f), TRIM(inpsmtf), TRIM(inptx0f),TRIM(inpaodf)
        WRITE(u4,'(A)') 'TAYLORS.EXE'
	ELSE IF (opcode==2) THEN
        WRITE(u4,'(2A,1X,2A,1X,A,1X,A)')'./ROTTEX.EXE ',TRIM(inpi01f),
     &      TRIM(inpl01f), ' wagner.b04 ',TRIM(inputCf), TRIM(inpcoef)
        IF (lmax<=22) 
     &    WRITE(u4,'(4A,1X,A)') './CALCODF.EXE ', TRIM(inpi02f),
     &      ' create6.b01 create6.b02 ', TRIM(inpcoef), TRIM(inpaodf)
        IF (lmax>22) 
     &    WRITE(u4,'(4A,1X,A)') './CALCODF.EXE ', TRIM(inpi02f),
     &      ' create5.b01 create5.b02 ', TRIM(inpcoef), TRIM(inpaodf)
        WRITE(u4,'(A,5(A,1X))') './ODFTAY.EXE ', TRIM(inpi03f),
     &      TRIM(inpl03f), TRIM(inpsmtf), TRIM(inptx0f),TRIM(inpaodf)
        WRITE(u4,'(A)') './TAYLORS.EXE'
      END IF
	CLOSE (u4)
C
      RETURN
      END SUBROUTINE PRETAYSIM
C
C********************** END OF SUBROUTINE PRETAYSIM ****************************
C
C********************* BEGIN OF SUBROUTINE POSTAYSIM ***************************
C
      SUBROUTINE POSTAYSIM (outpref,reabatf,outinfo,imag,idn,lmax,
     &                      nrun,nsteps,idm,phi0,opcode)
C
C     Objective: Postprocessing after TAYLORS simulation
C
C         - Extracts the deformed orientations from TAYLORS output
C         - Prepares the new C-coefficients
C
      CHARACTER outpref*8, reabatf*40, outinfo*40
	INTEGER imag, idn, idm, lmax, nrun, nsteps, opcode
      DOUBLE PRECISION phi0
C
      CHARACTER reactlf*40              ! Control file for readtxt.exe
	CHARACTER outtx1f*40              ! Discretised Output texture TX1 (bin)
	CHARACTER outsmtf*40              ! Discretised Output texture SMT (ascii)
	CHARACTER outcoef*40              ! Output C-coefficients filename (bin)
      CHARACTER outi01f*40, outl01f*40  ! .i01 & .l01 file for smtodf.exe
C  
      INTEGER u0, u1, u2                ! Channels for file I/O
	PARAMETER (u0=908, u1=909, u2=910) 
C
C     Initialisation of filenames
C
      reactlf = 'READTXT.CTL'
      outtx1f = TRIM(outpref) // '.tx1'
      outsmtf = TRIM(outpref) // '.smt'
	outcoef = TRIM(outpref) // '.C'
	outi01f = TRIM(outpref) // '.i01'
	outl01f = TRIM(outpref) // '.l01'
C
      OPEN(u0,FILE=reactlf,STATUS='REPLACE',ACTION='WRITE')
      WRITE(u0,'(A)')  '0               LIST CONTROL PARAMETER'
      WRITE(u0,'(2A)') '1               TYPE OF INPUT TEXTURE-FILE',
     &                 '(Filename on next line)'
      WRITE(u0,'(A)') TRIM(outtx1f)
      WRITE(u0,'(I4,12X,A)') nrun, 'NUMBER OF RUNS TO BE PERFORMED'
	WRITE(u0,'(2A)') '1               Output texture file? ',
     &                 'If=1, filename on next line: ' 
      WRITE(u0,'(A)') TRIM(outsmtf)
      WRITE(u0,'(2A)') '0               M-factor file parameter.',
     &                 'If>1,filename on next line: '                
      WRITE(u0,'(A)')  '1               CHOSEN RUN'
      WRITE(u0,'(I4,12X,A)') nsteps, 'CHOSEN STEP'
      CLOSE (u0)
C  
      OPEN(u1,FILE=outi01f,STATUS='REPLACE',ACTION='WRITE')
      WRITE(u1,'(I5,I5,I5,I5,A)') imag, idn, idm, 0, 
     &         '  IMAG, IDN, IDM, IPR(0: no listing 1: listing)'
      WRITE(u1,'(A,I5,F5.2)') 'LMAX ', lmax, PHI0
	WRITE(u1,'(2A,I5,F5.2)') TRIM(outpref),'(LMAX,PHI0)=',lmax,PHI0 
      CLOSE (u1)
C
      OPEN(u2,FILE=reabatf,STATUS='REPLACE',ACTION='WRITE')
	IF (opcode==1) THEN
       WRITE(u2,'(A)') 'READTXT.EXE'
	 IF (lmax<=22)
     & WRITE(u2,'(6(A,1X))')'SMTODF.EXE',TRIM(outi01f),TRIM(outl01f), 
     &      TRIM(outsmtf), 'create6.b01 create6.b02', TRIM(outcoef) 
	 IF (lmax>22)
     & WRITE(u2,'(6(A,1X))')'SMTODF.EXE',TRIM(outi01f),TRIM(outl01f), 
     &      TRIM(outsmtf), 'create5.b01 create5.b02', TRIM(outcoef) 
      ELSE IF (opcode==2) THEN
       WRITE(u2,'(A)') './READTXT.EXE'
	 IF (lmax<=22)
     & WRITE(u2,'(6(A,1X))')'./SMTODF.EXE',TRIM(outi01f),TRIM(outl01f), 
     &      TRIM(outsmtf), 'create6.b01 create6.b02', TRIM(outcoef) 
	 IF (lmax>22)
     & WRITE(u2,'(6(A,1X))')'./SMTODF.EXE',TRIM(outi01f),TRIM(outl01f), 
     &      TRIM(outsmtf), 'create5.b01 create5.b02', TRIM(outcoef) 
      END IF	 
	CLOSE (u2)
C
      RETURN
      END SUBROUTINE POSTAYSIM
C
C********************** END OF SUBROUTINE POSTAYSIM ****************************
C
C********************** BEGIN OF SUBROUTINE RDLINES ****************************
C
      SUBROUTINE RDLINES(funit,nlines)
C	
      INTEGER funit, i, nlines
      CHARACTER subline*120
C
	DO i = 1, nlines
	  READ(funit,'(A)') subline
	END DO
C     
      RETURN  
      END 
C
C*********************** END OF SUBROUTINE RDLINES *****************************