module KOST1xState
use KOST1x
implicit none

      type(StatVar),allocatable,dimension(:),save    :: KS_state
      
public KS_writeState, KS_updateState
      
contains

      !> Allocate memory to the KS_state array.
      !>
      !> The function simply makes allocation. It relies on a default initializer 
      !> of StatVar type.
      integer function KS_initState(norient) result(info)
      implicit none
      integer,intent(in)      :: norient !< Number of orientations in the material
      !
            info = -1
            if (norient > 0) allocate(KS_state(norient),stat=info)
      !
      end function

    
      !> Update the state variables of the PEBP model for i-th grain.
      subroutine KS_updateState(i,sliprate,deltaT,Mcrss,info)
      implicit none
      integer,intent(in)                              :: i        !< Grain identifier
      double precision,intent(in), dimension(24)      :: sliprate !< slip rates on 2*12 slip systems
      double precision,intent(in)                     :: deltaT   !< Time increment
      double precision,dimension(:,:),intent(out)     :: Mcrss    !< 
      integer,intent(out)                             :: info
      !
      TYPE(StatVar) :: SV_tmp
      !
            info = -1
            if ((size(KS_state) < i) .or. any(shape(Mcrss) /= shape(SV_tmp%CRSS)) ) return
            !
            call MakeInc(KS_state(i),sliprate,deltaT,SV_tmp,info)
            if (info /= 0) return
            ! Update the state of the i-th grain
            KS_state(i) = SV_tmp
            ! Extract the CRSSes
            Mcrss = KS_state(i)%CRSS
      !
      end subroutine

      
      !>
      integer function KS_writeState(iounit)
      implicit none
      integer,intent(in)                              :: iounit   !< I/O unit number
      !
      integer :: i, n
      !
            n = size(KS_state)
            do i = 1, n
                  ! TODO: here we place a call to function KOST1x::writeState that 
                  ! sends one line output to iounit.
            enddo
            KS_writeState = -1
      !
      end function
      
end module