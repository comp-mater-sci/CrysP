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
!>    \file cub2cur.f90 
!>    
!

      program cub2cur
      use cubAccess
      implicit none
      integer,parameter             :: ncubunit=110,ncurunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamcub, fnamcur
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 2 ) then
            write(*,*) 'arguments: cubfile curfile [title]'
            call exit(10)
      endif
      ! Form title
      title=''
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
      call GET_COMMAND_ARGUMENT(2,fnamcur,status=iuerr)
      
      write(*,*) trim(fnamcub), ' => ',trim(fnamcur)
      ! Open CUB file 
      open (unit=ncubunit,file=TRIM(fnamcub),                            &
                     status='old',form='UNFORMATTED')
      
      call readCub(ncubunit,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading CUB file.'
            call exit(11)
      endif
      close(ncubunit)
      ! Open CUR file
      open (unit=ncurunit,file=TRIM(fnamcur),                            &
                     status='unknown',form='FORMATTED')
      ! Mangle title
      if (len_trim(title) > 0 ) micros%TITLE = trim(title)
      
      call writeCur(ncurunit,micros,iuerr)     
      if (iuerr /= 0) then
            write(*,*) 'Error writing CUR file.'
            call exit(11)
      endif
      close(ncurunit)

      end program 


