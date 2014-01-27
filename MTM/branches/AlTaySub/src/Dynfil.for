      module DYNFIL
      use miscutils, only: unitMatrix
      implicit none

      
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

      !> State variable: array of grains/orientations.
      !>
      !> The array is actually allocated in the subroutine DYNFIL0.
      type(grain),dimension(:),allocatable,save :: DFIL

      !> State variable: material (frame) global geometry
      type(matFrame),save     :: mf

      !> State variable: title of the input texture file 
      character(len=40),save  :: filetitle = ''
      
      !> State variable: step number.
      integer,save            :: NRSTEP = 0
      
      
      contains
      
      !> Allocate the memory block for the state variables.
      subroutine DYNFIL0(npoint,mpoint,keepstate,istat)
      use IOConfig
      implicit none
      !> Number of points (elements) to be allocated
      integer,intent(in)      :: npoint
      !> Maximal number of points that are allowed
      integer,intent(in)      :: mpoint
      !> Flag: if .true., the contents of the DFIL will be preserved
      !> on reallocation.
      logical,intent(in)      :: keepstate
      !> Exit code: 0 on success
      integer,intent(out)     :: istat
      !
      type(grain),dimension(:),allocatable :: tmp
      integer :: ntransf
      !
      istat = 1
      if ((npoint > mpoint).or.(npoint <= 0)) then
            ! Error handling
            if(NLIST.eq.1) write(IMP,100)
            return
      endif
      ! 
      if (.not. allocated(DFIL)) then
            allocate(DFIL(npoint),stat=istat)
      else
            ! DFIL is previously allocated
            if (size(DFIL) == npoint) then
                  ! Nothing to do.
                  istat = 0
                  return
            endif
            if (keepstate) then
                  ! Transfer npoints 
                  allocate(tmp(npoint),stat=istat)
                  if (istat == 0) then
                        ntransf = min(npoint,size(DFIL))
                        tmp(1:ntransf) = DFIL(1:ntransf)
                        call move_alloc(tmp,DFIL)
                  endif
            else
                  deallocate(DFIL)
                  allocate(DFIL(npoint),stat=istat)
            endif
      endif
      ! Error handling
      if (istat /= 0) then
            if(NLIST.eq.1) write(IMP,101)
      endif
      !
 100  format(' DYNFIL0 - requested number of grains exceeds limits.')
 101  format(' DYNFIL0 - allocation of memory failed.')
      end subroutine
      
      !> Finalizes the module. The subroutine puts the module variables 
      !> into initial state and deallocates the storage.
      subroutine DYNFIL_finalize(info)
      implicit none
      integer,intent(out)     :: info
      !
            info = 0
            mf = matFrame()
            filetitle = ''
            NRSTEP = 0
            if (allocated(DFIL)) deallocate(DFIL,stat=info)
      !
      end subroutine
      
      
      !> Initialize the memory block for the state variables 
      !> (texture, grain axes etc.)
      subroutine DYNFIL1(nunit,npoint,MPOINT,istat)
      use IOConfig
      implicit double precision (a-h,o-z)
      integer,intent(in)      :: nunit
      integer,intent(in)      :: npoint
      integer,intent(in)      :: MPOINT
      integer,intent(out)     :: istat
      !
      DIMENSION AXES(3),EULR(3),CIJ(3,3),TAX(3,3),F(3,3),T(3,3),
     1 ZERO(3,3)
      integer :: i
      !      
      if (.not. allocated(DFIL)) then 
            call DYNFIL0(npoint,MPOINT,.false.,istat)
            if (istat /= 0) return
      endif
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
      istat = 0
      !
      end subroutine DYNFIL1


      !> Extract the global material data
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

      !> Write the global material data
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

      !> Get the record data for i-th grain
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


      !> Put the record data for i-th grain
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

      !> Set the computed fields in grain structure.
      !>
      !> The following fields are modified:
      !>  - tT is calculated from Euler angles as defined by the tfi1,
      !>    tPHI and tfi2 fields
      !>  - tAXES,tEULR,tF,tCIJ,tTAX - inherit corresponding properties 
      !>    from mf
      !>  - tZERO and tRHO - are zeroed.
      subroutine initFields(mf,gr)
      implicit none
      type(matFrame),intent(in)     :: mf
      type(grain),intent(inout)     :: gr
      !
            call EulRad_2_Tmatrix(gr%tT,gr%tfi1,gr%tPHI,gr%tfi2)
            !
            ! Backward compatibility with type(gr):
            ! initialize the remaining components with mf data...
            gr%tAXES = mf%GAXES 
            gr%tEULR = mf%GEULR
            gr%tF   = mf%FALG
            gr%tCIJ = mf%CIJ0
            gr%tTAX = mf%TAX0
            ! ... and zero all the rest.
            gr%tZERO = 0.D0
            gr%tRHO  = 0.D0
      !
      end subroutine
      
      
      end module DYNFIL

      SUBROUTINE LEESOR(NUNIT,MPOINT)
#ifdef ALTAY_SUBROUTINE
      use altayConfig, only: acnf
#endif
      use dynfil
      use miscutils
      use IOConfig
      implicit double precision (a-h,o-z)
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW
      DIMENSION T(3,3),TA(3,3),A(3,3),F(3,3),FALG(3,3),ZERO(3,3),
     1 GAXES(3),GEULR(3),CIJ(3,3),TAXES(3,3)
      character*12 dom
!      character*12 fnam1,dom !
c <jg>
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

      call EulRad_2_TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
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
      call EulRad_2_TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
      call Transf(GAXES,CIJ,TAXES)
C      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TG
      write (nunit) nrstep,FALG,GAXES,GEULR,CIJ,TAXES
   6  I=0
      do 11 j=1,NREC
      if (NDAT.eq.1) goto 20
      READ (NDAT1,97) I,GEW,PHI1,PHI,PHI2,GAMMA
  97  FORMAT (I6,F10.0,2x,3f10.0,2x,F10.0)
      F = FALG
      do 24 K=1,3
      GEULR(K)=GEULR(K)*FPI
  24  continue
      call EulRad_2_TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
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
  29  call EulRad_2_TMATRIX(TAXES,GEULR(1),GEULR(2),GEULR(3))
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

      subroutine xleesor()
      use DYNFIL
      implicit double precision (a-h,o-z)
      COMMON /SYMP/ INV,ISP,LOM,KSYM,KTYP,NPOINT,TEN(3,3),TOTGEW
      ! Fetch the number of grains
      NPOINT = size(DFIL)
      end subroutine
      

