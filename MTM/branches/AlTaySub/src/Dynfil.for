      module DYNFIL
      implicit none

      double precision,dimension(3,3),parameter :: unitMatrix = reshape(
     &[ 1.D0, 0.D0, 0.D0,
     &  0.D0, 1.D0, 0.D0,
     &  0.D0, 0.D0, 1.D0], [ 3, 3 ])
      
      TYPE :: grain
            double precision :: tFI1 = 0.D0,tPHI = 0.D0, tFI2 = 0.D0
            double precision :: tGEW = 1.D0 ,tGAM = 0.D0
            double precision, dimension(3) :: tAXES = 1.D0, tEULR = 0.D0
            double precision, dimension(3,3) :: tT = 0.D0
            double precision, dimension(3,3) :: tF = unitMatrix
            double precision, dimension(3,3) :: tCIJ = unitMatrix
            double precision, dimension(3,3) :: tTAX = unitMatrix
            double precision, dimension(3,3) :: tZERO = 0.D0,tRHO = 0.D0
      END TYPE grain
      
      type :: matFrame
            double precision,dimension(3,3) :: FALG = unitMatrix
            double precision,dimension(3,3) :: CIJ0 = unitMatrix
            double precision,dimension(3,3) :: TAX0 = unitMatrix
            double precision,dimension(3) :: GAXES = 1.D0,GEULR = 0.D0
      end type

C     following array is actually allocated in the subroutine DYNFIL1:
      type(grain),dimension(:),allocatable,save :: DFIL

      type(matFrame),save     :: mf

      character(len=40),save  :: filetitle = ''
      
      integer,save            :: NRSTEP = 0
      
      
      contains
      
      !> Allocate the memory block for the state variables
      subroutine DYNFIL0(npoint,MPOINT,istat)
      implicit none
      integer,intent(in)      :: npoint
      integer,intent(in)      :: MPOINT
      integer,intent(out)     :: istat
      !
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /TEXTUR/ DUM1(29),NO,DUM2(9),
     1IDUM(2),DUM3(2),NLIST
      integer :: LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      integer :: NO,IDUM,NLIST
      double precision :: DUM1,DUM2,DUM3

      ! Make sure the old content is deallocated
      istat = 1
      if (allocated(DFIL)) deallocate(DFIL)
      if ((npoint > MPOINT).or.(npoint <= 0)) then
            ! Error handling
            if(NLIST.eq.1) write(IMP,100)
            return
      endif
      ! Allocate the storage
      allocate(DFIL(npoint),stat=istat)
      if (istat /= 0) then
            ! Error handling
            if(NLIST.eq.1) write(IMP,101)
      endif
      !
 100  format(' DYNFIL0 - requested number of grains exceeds limits.')
 101  format(' DYNFIL0 - allocation of memory failed.')
      end subroutine
      
      !> Initialize the memory block for the state variables 
      !> (texture, grain axes etc.)
      subroutine DYNFIL1(nunit,npoint,MPOINT,istat)
      implicit double precision (a-h,o-z)
      integer,intent(in)      :: nunit
      integer,intent(in)      :: npoint
      integer,intent(in)      :: MPOINT
      integer,intent(out)     :: istat
      !
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
	COMMON /TEXTUR/ DUM1(29),NO,DUM2(9),
     1IDUM(2),DUM3(2),NLIST
      DIMENSION AXES(3),EULR(3),CIJ(3,3),TAX(3,3),F(3,3),T(3,3),
     1 ZERO(3,3)
      integer :: LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      integer :: NO,IDUM,NLIST
      integer :: i
      !      
      if (.not. allocated(DFIL)) call DYNFIL0(npoint,MPOINT,istat)
      !!! FIXME !!!
      rewind nunit
      read (nunit) nrstep,mf%FALG,mf%GAXES,mf%GEULR,mf%CIJ0,mf%TAX0
      do i=1,npoint
         read(NUNIT) FI1,PHI,FI2,T,GEW,GAM,F,AXES,EULR,CIJ,TAX,ZERO
         DFIL(i)%tFI1=FI1
         DFIL(i)%tPHI=PHI
         DFIL(i)%tFI2=FI2
         DFIL(i)%tGEW=GEW
         DFIL(i)%tGAM=GAM
         DFIL(i)%tAXES=AXES
         DFIL(i)%tEULR=EULR
         DFIL(i)%tT=T
         DFIL(i)%tF=F
         DFIL(i)%tCIJ=CIJ
         DFIL(i)%tTAX=TAX
         DFIL(i)%tZERO=ZERO
         DFIL(i)%tRHO=ZERO
      end do
      rewind nunit
      !!! FIXME !!!
      istat = 0
      !
      end subroutine DYNFIL1


      !> To read the first record of the storage
      subroutine DYNFIL2(n,F,AXES,EULR,CIJ,TAX)
      integer,intent(out)     :: n
      double precision,intent(out) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),
     & F(3,3)
      !
            n=nrstep
            F=mf%FALG
            AXES=mf%GAXES
            EULR=mf%GEULR
            CIJ=mf%CIJ0
            TAX=mf%TAX0
      !
      end subroutine DYNFIL2

      !> To write the first record of the storage
      subroutine DYNFIL3(n,F,AXES,EULR,CIJ,TAX)
      integer,intent(in)     :: n
      double precision,intent(in) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),
     & F(3,3)
      !
            nrstep=n
            mf%FALG=F
            mf%GAXES=AXES
            mf%GEULR=EULR
            mf%CIJ0=CIJ
            mf%TAX0=TAX
      !
      end subroutine DYNFIL3

      !> To read a record of the storage
      subroutine DYNFIL4(i,FI1,PHI,FI2,T,
     1                  GEW,GAM,F,AXES,EULR,CIJ,TAX,ZERO)
      implicit none
      integer,intent(in) :: i
      double precision,intent(out) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(out) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),
     1 F(3,3),T(3,3),ZERO(3,3)
      !
            FI1=DFIL(i)%tFI1
            PHI=DFIL(i)%tPHI
            FI2=DFIL(i)%tFI2
            GEW=DFIL(i)%tGEW
            GAM=DFIL(i)%tGAM
            AXES=DFIL(i)%tAXES
            EULR=DFIL(i)%tEULR
            T=DFIL(i)%tT
            F=DFIL(i)%tF
            CIJ=DFIL(i)%tCIJ
            TAX=DFIL(i)%tTAX
            ZERO=DFIL(i)%tZERO
      !
      end subroutine DYNFIL4


      !> To write a record of the storage
      subroutine DYNFIL5(i,FI1,PHI,FI2,T,
     1                  GEW,GAM,F,AXES,EULR,CIJ,TAX,ZERO)
      implicit none
      integer,intent(in) :: i
      double precision,intent(in) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(in) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),
     1 F(3,3),T(3,3),ZERO(3,3)
      !
            DFIL(i)%tFI1=FI1
            DFIL(i)%tPHI=PHI
            DFIL(i)%tFI2=FI2
            DFIL(i)%tGEW=GEW
            DFIL(i)%tGAM=GAM
            DFIL(i)%tAXES=AXES
            DFIL(i)%tEULR=EULR
            DFIL(i)%tT=T
            DFIL(i)%tF=F
            DFIL(i)%tCIJ=CIJ
            DFIL(i)%tTAX=TAX
            DFIL(i)%tZERO=ZERO
      !
      end subroutine DYNFIL5

      !> For IDIR=0:
      !> To read RHOS from the temporary file in memory
      !> For IDIR=1:
      !> To write RHOS in the temporary file in memory
      subroutine DYNFIL7(i,IDIR,RHOS)
      implicit none
      integer,intent(in) :: i, IDIR
      double precision :: RHOS(3,3)
      !
            if (IDIR.eq.0) then
                  RHOS=DFIL(i)%tRHO
            else
                  DFIL(i)%tRHO=RHOS
            endif
      !
      end subroutine DYNFIL7
      
      end module DYNFIL


      MODULE MICROSTR
      implicit double precision (a-h,o-z)
C     following array is actually allocated in the subroutine GRFIL:
      double precision, dimension(:,:,:),allocatable,save :: TmatGr
      integer,save :: NGrElm
      character*40, save :: TitMic
      logical vers
      data vers /.true./
      end module MICROSTR

      

      


      SUBROUTINE GRFIL  
C     Reading of "microstructure" (Euler angles defining 
C       grain boundary segments)
C     Allocation of "temporary file" to memory
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 28/11/2011
C   The output to .LST in this subroutine doesn't depend on the value of NLIST, since NLIST doesn't have value yet!
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
#ifdef ALTAY_SUBROUTINE
      use altayConfig, only: acnf
#endif
      USE MICROSTR
      use miscutils
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      DIMENSION TA(3,3),A(3,3),A1(3,3)
c <jg>
      integer,parameter :: pathlength=512
      character(len=pathlength) :: fnam1
c </jg>
      SAVE
      data convf/0.5729577951308232D+02/
      DATA A  / 8 * 0.0D0 , 1.0D0  /
      FPI=1.0D0/convf
#ifdef ALTAY_SUBROUTINE
      fnam1 = acnf%micros_fname  
#else
      read (KLEC,88) fnam1
  88  format (a)
c <jg>
      call stripComment(fnam1)
c </jg>
      write (*,103) trim(fnam1)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 28/10/2011
C    Since this output is not controled by NLIST, I supressed it.
C      write (IMP,103) fnam1
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
 103  format (' GRFIL - Input Texture File:',a)
#endif
  99  FORMAT (I5)
C     UNIT NDAT1= INITIAL MICROSTRUCTURE
      open (unit=NDAT1,file=fnam1,status='old')
C
      read (NDAT1,94) NGrElm,TitMic
  94  format(I5,5x,A)
#ifndef NO_STDOUT
      write (*,93) NGrElm,TitMic
#endif
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 28/10/2011
C    Since this output is not controled by NLIST, I supressed it.
C      write (IMP,93) NGrElm,TitMic
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
  93  format (' Number of orientations in MICROSTRUCTURE file:',I5,/,
     1' Titel on  file: ',A)
      ALLOCATE(TmatGr(3,3,NGrElm),STAT=jok)
      if (jok.ne.0) then
            stop
      endif
 101  format (' GRFIL ',
     1 'Allocation of RAM-memory was succesful')
 102  format (' GRFIL - ',
     1 'Allocation of memory failed')
      do 11 IGrElm=1,NGrElm
      READ (NDAT1,96) PHI2,PHI,PHI1
  96  FORMAT (3F10.0)                                      
      PHI=PHI*FPI
      PHI2=PHI2*FPI
      C=COS(PHI)
      S=SIN(PHI)
      C2=COS(PHI2)
      S2=SIN(PHI2)
      TA(1,1)=C2                                                        
      TA(2,1)=-S2                                                      
      TA(3,1)=0.D0                                                        
      TA(1,2)=S2*C                                                      
      TA(2,2)=C2*C                                                      
      TA(3,2)=-S                                                        
      TA(1,3)=S2*S                                                     
      TA(2,3)=C2*S                                                      
      TA(3,3)=C                                                         
      FI1=PHI1*FPI
      C1=COS(FI1)
      S1=SIN(FI1)
      A(1,1)=C1                                                         
      A(2,1)=-S1                                                        
      A(1,2)=S1                                                         
      A(2,2)=C1 
      !!! FIXME: instead of the code above, just:      
      ! ! Note: FI1 is not modified/updated!
      ! call  grainOrient(PHI1,PHI,PHI2,TA,A)
      !
      !!! FIXME
      CALL MATPROD(A1,TA,A,3,3,3)
      do i=1,3
         do j=1,3
            TmatGr(i,j,IGrElm)=A1(j,i)
         enddo
      enddo                                                       
  11  CONTINUE                                                          
      CLOSE (unit=NDAT1)
      RETURN
      END SUBROUTINE GRFIL


      ! Converts the Euler angles from degrees to radians. 
      ! Returns relevant rotation matrices TA and A.
      subroutine grainOrient(PHI1,PHI,PHI2,TA,A)
      implicit none
      double precision,intent(inout)   :: PHI1,PHI,PHI2
      double precision,dimension(3,3),intent(out)     :: TA, A
      !
      double precision, parameter :: convf = 0.5729577951308232D+02
      double precision, parameter :: FPI = 1.0D0/convf
      double precision :: C,C1,C2,S,S1,S2
      !
      PHI=PHI*FPI
      PHI2=PHI2*FPI
      C=COS(PHI)
      S=SIN(PHI)
      C2=COS(PHI2)
      S2=SIN(PHI2)
      TA(1,1)=C2                                                        
      TA(2,1)=-S2                                                      
      TA(3,1)=0.D0                                                        
      TA(1,2)=S2*C                                                      
      TA(2,2)=C2*C                                                      
      TA(3,2)=-S                                                        
      TA(1,3)=S2*S                                                     
      TA(2,3)=C2*S                                                      
      TA(3,3)=C                                                         
      !
      A = 0.D0
      PHI1=PHI1*FPI
      C1=COS(PHI1)
      S1=SIN(PHI1)
      A(1,1)=C1                                                         
      A(2,1)=-S1                                                        
      A(1,2)=S1                                                         
      A(2,2)=C1 
      A(3,3) = 1.D0
      !     
      end subroutine




      SUBROUTINE LEESOR(NUNIT,MPOINT)
#ifdef ALTAY_SUBROUTINE
      use altayConfig, only: acnf
#endif
      use dynfil
      use miscutils
      implicit double precision (a-h,o-z)
      COMMON /ES/ LEC,KLEC,IDISK1,IMP,IMP1,IMP2,NDAT1
      COMMON /TEXTUR/ DUM1(29),NO,DG(3,3),
     1ITW,IPR,GMMA,GEWF,NLIST
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW
      DIMENSION T(3,3),TA(3,3),A(3,3),F(3,3),FALG(3,3),ZERO(3,3),
     1 GAXES(3),GEULR(3),CIJ(3,3),TAXES(3,3)
      character*12 dom
!      character*12 fnam1,dom !
c <jg>
      integer,parameter :: pathlength = 512
      character(len=pathlength) :: fnam1
      integer :: info
c </jg>      
      character*40 Titel
C      dimension TG(3,3)
      SAVE
      data convf/0.5729577951308232D+02/
      DATA A  / 8 * 0.0D0 , 1.0D0  /,ZERO/9*0.0D0/
      data nbyp/0/
      data FALG/1.0D0,0.0D0,0.0D0,
     1          0.0D0,1.0D0,0.0D0,
     2          0.0D0,0.0D0,1.0D0/
C      data TG  /1.0D0,0.0D0,0.0D0,
C     1          0.0D0,1.0D0,0.0D0,
C     2          0.0D0,0.0D0,1.0D0/
      data GAXES/1.0D0,1.0D0,1.0D0/,GEULR/0.0D0,0.0D0,0.0D0/
      FPI=1.0D0/convf
C     UNIT NUNIT = Temporary file
      open(unit=NUNIT,status='SCRATCH',form='unformatted')
#ifdef ALTAY_SUBROUTINE
      NDAT = acnf%texture%input_type
      fnam1 = trim(acnf%texture%input_fname)
      NSTP = acnf%texture%block_id
#else      
      read (KLEC,99) NDAT
      read (KLEC,88) fnam1
c <jg>
      call stripComment(fnam1)
c </jg>
  88  format (a)
      write (*,103) trim(fnam1)
	if(NLIST.eq.1) then
      write (IMP,103) trim(fnam1)
	end if
 103  format (' LEESOR - Input Texture File:',a)
      read (KLEC,99) NSTP
  99  FORMAT (I5)
      if(NLIST.eq.1) then
      WRITE (IMP,100) NDAT,NSTP
	end if
      WRITE (*,100) NDAT,NSTP
 100  FORMAT (' LEESOR - READS A TEXTURE FILE Type (NDAT) is:'
     1 ,I5,' CHOSEN BLOCK:',I5)
#endif
C     UNIT NDAT1= INPUT TEXTURE
      if (nbyp.eq.0) open (unit=NDAT1,file=fnam1,status='old')
      nbyp=1
      if (ndat.gt.1) goto 33
C
C     "Manual-made" type of input texture (.SMT-file)
C
      read (NDAT1,94) NREC,TITEL
  94  format(I5,5x,A)
#ifndef NO_STDOUT      
      write (*,93) NREC,TITEL
#endif
	if(NLIST.eq.1) then
      write (IMP,93) NREC,TITEL
	end if
  93  format (' Number of orientations in SMT-type input file:',I5,/,
     1' Titel on input file: ',A)

      call TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
      call Transf(GAXES,CIJ,TAXES)
C      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TG
      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TAXES
      goto 14
C
C     Input texture made during a previous simulation (.CUR-file)
C
  33  read (NDAT1,92) TITEL
  92  format (A)
      if(NLIST.eq.1) then
      write (IMP,102) TITEL
	end if
#ifndef NO_STDOUT            
      write (*,102) TITEL
#endif
  102 format(' Title on CUR-type-input file:',A)
  14  LPOINT=MPOINT+1
      J=4
      NPOINT=1
      NSTAP=1
      STAP=0.0D0
      TOTGEW=0.D0
      IF (NDAT.EQ.1) GOTO 6
      IF (NSTP.EQ.0) GOTO 19
      DO 7 I=1,NSTP
      read (NDAT1,91) DOM
      read (NDAT1,91) DOM
  91  format (A)
      read (NDAT1,90) NREC
  90  format (11x,I5)
      read (NDAT1,91) DOM
      DO 8 J=1,NREC
      READ (NDAT1,98) dom
  98  format (A)
   8  CONTINUE
   7  CONTINUE                                                          
  19  read (NDAT1,91) DOM
      read (NDAT1,91) DOM
      read (NDAT1,89) NS,NREC,FALG,GAXES,GEULR
  89  format (I6,5x,I5,44x,5(2x,3f10.0))
#ifndef NO_STDOUT
      write (*,104) NS,NREC
#endif
	if(NLIST.eq.1) then
      write (IMP,104) NS,NREC
	end if
 104  format (' Input block nr.',i5,3x,'  Number of crystallites',i5)
      read (NDAT1,91) DOM
      do 23 K=1,3
      GEULR(K)=GEULR(K)*FPI
  23  continue
      call TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
      call Transf(GAXES,CIJ,TAXES)
C      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TG
      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TAXES
   6  I=0
      do 11 j=1,NREC
      if (NDAT.eq.1) goto 20
      READ (NDAT1,97) I,GEW,PHI1,PHI,PHI2,GAMMA,F,GAXES,GEULR
  97  FORMAT (I6,F10.0,2x,3f10.0,2x,F10.0,5(2x,3f10.0))
      do 24 K=1,3
      GEULR(K)=GEULR(K)*FPI
  24  continue
      call TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
      call Transf(GAXES,CIJ,TAXES)
      goto 21
  20  READ (NDAT1,96) PHI2,PHI,PHI1,STAP,NSTAP,GEW,GAMMA
  96  FORMAT (4F10.0,I5,5X,2F10.0)                                      
C      WRITE (*,150) PHI2,PHI,PHI1,STAP,NSTAP,GEW,GAMMA
 150  FORMAT (/,1H ,'PHI2=',F7.2,'   PHI=',F7.2,'   FIRST PHI1=',
     1F7.2,'   STEP PHI1=',F7.2,'   NUMBER STEPS=',I4,'   WEIGHT='      
     2,F10.4,'   GAMMA=',F10.4)                                         
      do 40 k=1,3
      do 41 l=1,3
      F(k,l)=0.0
  41  continue
      F(k,k)=1.0
  40  continue
  21  PHI=PHI*FPI
      PHI2=PHI2*FPI
      C=COS(PHI)
      S=SIN(PHI)
      C2=COS(PHI2)
      S2=SIN(PHI2)
      TA(1,1)=C2                                                        
      TA(2,1)=-S2                                                      
      TA(3,1)=0.D0                                                        
      TA(1,2)=S2*C                                                      
      TA(2,2)=C2*C                                                      
      TA(3,2)=-S                                                        
      TA(1,3)=S2*S                                                     
      TA(2,3)=C2*S                                                      
      TA(3,3)=C                                                         
      GAM=GAMMA                                                         
      DO 34 K=1,NSTAP
      FI1=PHI1+(K-1)*STAP
C     WRITE (IMP,151) NPOINT,FI1                                        
 151  FORMAT (1H ,I10,F12.2)
      FI1=FI1*FPI
      C1=COS(FI1)
      S1=SIN(FI1)
      A(1,1)=C1                                                         
      A(2,1)=-S1                                                        
      A(1,2)=S1                                                         
      A(2,2)=C1                                                         
      CALL MATPROD(T,TA,A,3,3,3)
  29  call TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
      call Transf(GAXES,CIJ,TAXES)
C      WRITE (NUNIT) FI1,PHI,PHI2,T,GEW,GAM,F,GAXES,GEULR,CIJ,TG,ZERO
      WRITE (NUNIT) FI1,PHI,PHI2,T,GEW,GAM,F,GAXES,GEULR,CIJ,TAXES,ZERO
      TOTGEW=TOTGEW+GEW
      NPOINT=NPOINT+1                                                   
  28  CONTINUE
  34  CONTINUE
  11  CONTINUE                                                          
      NPOINT=NPOINT-1 
	if(NLIST.eq.1) then                                                  
      WRITE (IMP,107) NPOINT,TOTGEW
	end if
 107  FORMAT (' NUMBER OF ORIENTATIONS=',I6,'   SUM OF ALL WEIGHT ',
     1 'FACTORS=',F15.7,/)
      J=1
      call DYNFIL1(nunit,npoint,MPOINT,info)
      close(nunit)
      RETURN
      END SUBROUTINE LEESOR



