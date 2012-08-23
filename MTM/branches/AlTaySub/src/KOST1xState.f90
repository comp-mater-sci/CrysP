!
! $Id$
!
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
            info = KS_ErrBadDims
            if (size(KS_state) < i) return
            if ( any(shape(Mcrss) /= shape(KS_state(i)%CRSS)) ) return
            ! Extract the CRSSes
            Mcrss = KS_state(i)%CRSS
            info = KS_OK
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
            write(iounit,fmt=100) n
            write(iounit,fmt=110)
            do i = 1, n
                  write(iounit,fmt=200) i      
                  if (WriteSVfile(iounit,KS_state(i))) exit
            enddo
            write(iounit,fmt=111)
            if (i > n) info = KS_OK 
            
100         format(I5,1X,' # of points in KOST11 block')
110         format('-->')
111         format('<--')
200         format(I5)
      !
      end function

      !>
      integer function KS_readState(iounit) result(info)
      implicit none
      integer,intent(in)                              :: iounit   !< I/O unit number
      !
      integer :: i, n, nf, tmp, ioerr
      character(len=5) :: tmp_str      
      !
            info = KS_ErrIO
            n = size(KS_state)
            nf = 0
            read(iounit,fmt=100,iostat=ioerr) nf
            if ((nf /= n) .or. (ioerr /= 0)) return
            read(iounit,fmt=110) tmp_str
            do i = 1, n
                  read(iounit,fmt=200,iostat=ioerr) tmp
                  if ( (ioerr /= 0) .or. (readSVfile(iounit,KS_state(i)))) exit
            enddo
            read(iounit,fmt=111,iostat=ioerr) tmp_str
            if ((i > n) .and. (ioerr == 0)) info = KS_OK 
100         format(I5)
110         format(A)
111         format(A)
200         format(I5)      !
      end function

       
end module