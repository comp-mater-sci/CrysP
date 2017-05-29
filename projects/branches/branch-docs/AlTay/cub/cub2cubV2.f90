!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2009-10-06
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file cub2cubV2.f90 
!>    
!

      program cub2cubV2
      use cubAccess
      implicit none
      integer,parameter             :: ncubunit=110,ncurunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamcub, fnamcub2
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 2 ) then
            write(*,*) 'arguments: cubfile cubfileV2'
            call exit(10)
      endif
      ! Form title
      do i=3,argc
            call GET_COMMAND_ARGUMENT(i,buf,status=iuerr)
            if (i == 3 ) then
                  title = trim(buf)
            else
                  title = trim(title) // ' ' // trim(buf)
            endif
      end do
      write(*,*) 'title: ', title

      call GET_COMMAND_ARGUMENT(1,fnamcub,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamcub2,status=iuerr)
      
      write(*,*) trim(fnamcub), ' => ',trim(fnamcub2)
      ! Open CUB file 
      open (unit=ncubunit,file=TRIM(fnamcub),                            &
                     status='old',form='UNFORMATTED')
      
      call readCub(ncubunit,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading CUB file.'
            call exit(11)
      endif
      close(ncubunit)
      ! Mangle title
      micros%TITLE = trim(title)
      ! Open CUBv2 file 
      open (unit=ncubunit,file=TRIM(fnamcub2),                           &
                     status='unknown',form='UNFORMATTED')
      call writeCubV2(ncubunit,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error writing CUBv2 file ',trim(fnamcub2)
            call exit(11)
      endif
 
      close(ncubunit)

      end program 


