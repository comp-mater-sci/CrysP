! $Id$
program rotdef
use fngMathUtils
implicit none
integer :: iunit = 100, ioerr, i, iarg, nargs
integer :: id_incr, id_ref
double precision,dimension(3,3) :: D,R  
type(EulerAngles) :: rot
double precision,dimension(3) :: arr
double precision :: tmp
!
character(len=512) :: fname, strtmp
!
      nargs = command_argument_count()
      if (nargs < 1) call exit(1)
      
      call get_command_argument(1,fname) 
      open(iunit,file=fname, status='old', iostat=ioerr)
      if (ioerr /= 0) call exit(2)
      read(iunit,*) id_incr, id_ref
      do i = 1,3
            read(iunit,*,iostat=ioerr) D(i,:)
      enddo      
      
      if (nargs > 1) then
            arr = 0.D0
            iarg = 2
            do i = 1, nargs-1
                  call get_command_argument(iarg,strtmp)
                  iarg = iarg + 1
                  read(strtmp,fmt=*,iostat=ioerr) tmp
                  if (ioerr /= 0) call exit(2)
                  arr(i) = tmp 
            enddo
            rot = Arr2EulerAngles(deg2rad(arr))
            ! rot = EulerAnglesDeg2Rad(rot)
      endif
      R = rotmat(rot)

      D = rotateSRTensorTo(D,R)

      write(*,fmt='(I0,1X,I0)') id_incr, id_ref
      write(*,fmt=500) ( D(i,:), i=1,3)
      500 format(3(ES13.5,1X))
!
end program
