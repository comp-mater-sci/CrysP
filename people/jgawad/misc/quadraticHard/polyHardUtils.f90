module polyHardUtils
use fngPath
use dmcUtils  ! for stripComment
implicit none

      type :: PolyHardConfig
            character(len=max_pathlen)    :: input_fname = ''
            character(len=max_pathlen)    :: output_fname = ''
            integer                       :: polynomial_order = 0
      end type
      
contains
      
      subroutine readPolyHardConfig(inpunit,cnf,info)
      integer,intent(in)                  :: inpunit
      type(PolyHardConfig),intent(out)    :: cnf
      integer,intent(out)                 :: info
      !
            read(inpunit,'(A)',iostat=info) cnf%input_fname     ! defdata.dat
            call stripComment(cnf%input_fname)
            read(inpunit,'(A)',iostat=info) cnf%output_fname    ! .hard
            call stripComment(cnf%output_fname)
            read(inpunit,*,iostat=info) cnf%polynomial_order
      !      
      end subroutine
      
      subroutine makeDatapoints(npoints,eps_0,eps_1,deps,vEps,vSigma,info)
      implicit none
      integer,intent(in)                              :: npoints
      double precision,intent(in)                     :: eps_0
      double precision,intent(in)                     :: eps_1
      double precision,intent(out)                    :: deps
      double precision,dimension(:),allocatable,intent(out) :: vEps, vSigma
      integer,intent(out)                             :: info
      !
      integer :: i
      !
            info = -1
            if (npoints <= 0) return
            allocate(vEps(npoints),vSigma(npoints))
            !
            deps = (eps_1 - eps_0) / dble(npoints-1)
            ! Interpolation points must be distinguishable
            if (deps < epsilon(0.D0)) return
            do i=1,npoints
                  vEps(i) = eps_0 + dble(i-1)*deps
            enddo
            vSigma = 0.D0
            info = 0
      !
      end subroutine
      
end module