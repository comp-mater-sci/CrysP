      module altayDynfil
      use altayMiscutils, only: unitMatrix
      use criMathUtils
      implicit none

      !> Texture-related state variables for single grain 
      TYPE :: grain
            double precision :: tFI1 = 0.D0,tPHI = 0.D0, tFI2 = 0.D0 !< Euler angles
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
      subroutine DYNFIL0(npoint,keepstate,istat)
      use altayIOConfig
      !> Number of points (elements) to be allocated
      integer,intent(in)      :: npoint
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
      if (npoint <= 0) then
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
 100  format('DYNFIL0: error: requested number of grains is zero.')
 101  format('DYNFIL0: error: allocation of memory failed.')
      end subroutine
      
      !> Finalizes the module. The subroutine puts the module variables 
      !> into initial state and deallocates the storage.
      subroutine DYNFIL_finalize(info)
      integer,intent(out)     :: info
      !
            info = 0
            mf = matFrame()
            filetitle = ''
            NRSTEP = 0
            if (allocated(DFIL)) deallocate(DFIL,stat=info)
      !
      end subroutine
      

      !> Extract the global material data
      subroutine DYNFIL2(n,F,AXES,EULR,CIJ,TAX)
      integer,intent(out)     :: n
      double precision,intent(out) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3), &
       F(3,3)
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
      double precision,intent(in) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),  &
       F(3,3)
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
      subroutine DYNFIL4(i,FI1,PHI,FI2,T,                                &
                        GEW,GAM,F,AXES,EULR,CIJ,TAX,ZERO)
      integer,intent(in) :: i
      double precision,intent(out) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(out) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3), &
       F(3,3),T(3,3),ZERO(3,3)
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
      subroutine DYNFIL5(i,FI1,PHI,FI2,T,                                &
                        GEW,GAM,F,AXES,EULR,CIJ,TAX,ZERO)
      integer,intent(in) :: i
      double precision,intent(in) :: FI1,PHI,FI2,GEW,GAM
      double precision,intent(in) :: AXES(3),EULR(3),CIJ(3,3),TAX(3,3),  &
       F(3,3),T(3,3),ZERO(3,3)
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
      type(matFrame),intent(in)     :: mf
      type(grain),intent(inout)     :: gr
      !
            gr%tT = rotmat(gr%tfi1,gr%tPHI,gr%tfi2)
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
      
      
      end module
      

