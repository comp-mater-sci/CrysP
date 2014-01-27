!> $Id$
      
!> Microstructure representation in AlTay
module microstr
implicit none

      !> Transformation matrix associated to the grain boundary reference frame 
      !> in the initial state.
      !> Shape is: [3,3,ngr], where ngr is the number of grains.
      double precision, dimension(:,:,:),allocatable,save :: TmatGr
      integer,save :: NGrElm = 0
      character*40, save :: TitMic = ''

      
contains
      
      ! Reading of "microstructure" (Euler angles defining 
      ! grain boundary segments) in SMT-format, allocation 
      ! and assignment of the module variables.
      subroutine GRFIL(fnam,F_mic,ierr)
      use miscutils
      use IOConfig
      implicit none
      !
      integer,intent(out)         :: ierr
      character(len=*),intent(in) :: fnam
      !F_mic is a deformation gradient that conceptually
      ! 'deforms' a spherical grain into an ellipsoidal shape
      double precision, dimension(3,3), intent(in) :: F_mic
      !
      integer          :: IGrElm
      double precision :: PHI2,PHI,PHI1
      double precision, dimension(3,3) :: T 
      ! 
      ierr = -1
      !
      ! output to NLIST 
      if(NLIST.eq.1) write (IMP,103) fnam
      103  format (' GRFIL - Input Texture File:' ,a)
      !
      open (unit=NDAT2,file=fnam,status='old',iostat=ierr) 
      if (ierr /= 0) return
      !
      read (NDAT2,94) NGrElm,TitMic
      94  format(I5,5x,A)
#ifndef NO_STDOUT
      write (*,93) NGrElm,TitMic
#endif
      ! output to NLIST 
      if(NLIST.eq.1) write (IMP,93) NGrElm,TitMic
      93  format (' Number of orientations in MICROSTRUCTURE file:' ,I5,/,' Titel on  file: ',A)
    
      !
      allocate(TmatGr(3,3,NGrElm),STAT=ierr)
      if (ierr.ne.0) then
          if(NLIST.eq.1) write(IMP,102)
          return
      end if 
      102  format (' GRFIL - Allocation of memory failed')
      !
      do IGrElm=1,NGrElm
            read (NDAT2,96) PHI2,PHI,PHI1
            !Calc. the transformation matrix T
            call EulDeg_2_Tmatrix(T,PHI1,PHI,PHI2)
            !TmatGr(1:3,i,IGrElm) for i=1,2 holds two non-parallel vectors 
            !  within the initial GB (grain boundary) plane.      
            !TmatGr(1:3,i,IGrElm) for i=3 holds a vector out of the initial
            !  GB plane (not necessarily perpendicular to the GB plane).
            TmatGr(:,:,IGrElm)=matmul(F_mic,transpose(T))
      enddo
      96 format(3F10.0)
      !
      close(unit=NDAT2)
      ierr = 0
      return
      end subroutine GRFIL


      !> Finalizes the module. The subroutine puts the module variables 
      !> into initial state and deallocates the storage.
      subroutine MICROSTR_finalize(info)
      implicit none
      integer,intent(out)     :: info
      !
            NGrElm = 0
            TitMic = ''
            if (allocated(TmatGr)) deallocate(TmatGr,stat=info)
      !
      end subroutine
      
end module microstr