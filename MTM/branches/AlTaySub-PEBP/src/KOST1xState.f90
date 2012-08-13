module KOST1xState
use KOST1x
implicit none

      type(StatVar),allocatable,dimension(:),private,save    :: KS_state
      
contains

      !> Allocate memory to the KS_state array.
      !>
      !> The function simply makes allocation. It relies on a default initializer 
      !> of StatVar type.
      integer function KS_initState(norient) result(info)
      implicit none
      integer,intent(in)      :: norient !< Number of orientations in the material
      !
      integer :: i
      !
            info = -1
            if (norient > 0) allocate(KS_state(norient),stat=info)
            if (info /= 0) return
            ! All elements of the KS_state array must have the same initial state.
            call GetInitStatVar(KS_state(1),info)
            if (info /= 0) return
            do i = 2, norient
                  KS_state(i) = KS_state(1)
            enddo
      !
      end function

    
      !> Update the state variables of the PEBP model for i-th grain.
      subroutine KS_updateState(i,sliprate,deltaT,info)
      implicit none
      integer,intent(in)                              :: i        !< Grain identifier
      double precision,intent(in), dimension(24)      :: sliprate !< slip rates on 2*12 slip systems
      double precision,intent(in)                     :: deltaT   !< Time increment
      integer,intent(out)                             :: info
      !
      type(StatVar) :: SV_tmp
      !
            info = -1
            if (size(KS_state) < i) return
            !
            call MakeInc(KS_state(i),sliprate,deltaT,SV_tmp,info)
            if (info /= 0) return
            ! Update the state of the i-th grain
            KS_state(i) = SV_tmp
      !
      end subroutine

      
      subroutine KS_getCRSS(i,Mcrss,info)
      implicit none
      integer,intent(in)                              :: i        !< Grain identifier
      integer,intent(out)                             :: info
      
      double precision,dimension(:,:),intent(out)     :: Mcrss    !< 
      !
            info = -1
            if (size(KS_state) < i) return
            if ( any(shape(Mcrss) /= shape(KS_state(i)%CRSS)) ) return
            ! Extract the CRSSes
            Mcrss = KS_state(i)%CRSS
            info = 0
      !      
      end subroutine
      
      !>
      integer function KS_writeState(iounit) result(info)
      implicit none
      integer,intent(in)                              :: iounit   !< I/O unit number
      !
      integer :: i, n
      !
            info = -1
            n = size(KS_state)
            do i = 1, n
                  ! TODO: here we place a call to function KOST1x::writeState that 
                  ! sends one line output to iounit.
                  if (WriteSVfile(iounit,KS_state(i))) exit
            enddo
            if (i > n) info = 0 
      !
      end function

      !>
      integer function KS_readState(iounit) result(info)
      implicit none
      integer,intent(in)                              :: iounit   !< I/O unit number
      !
      integer :: i, n
      !
            info = -1
            n = size(KS_state)
            do i = 1, n
                  ! TODO: here we place a call to function KOST1x::writeState that 
                  ! sends one line output to iounit.
                  if (readSVfile(iounit,KS_state(i))) exit
            enddo
            if (i > n) info = 0 
      !
      end function

      
end module