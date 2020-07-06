!
! $Id: euler2cur.f90 822 2011-11-04 09:07:00Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2009-10-06
!>    $Revision: 822 $
!>    $Date: 2011-11-04 10:07:00 +0100 (Fri, 04 Nov 2011) $
!>
!>    History of modifications: (see svn log)
!
!
!>    \file euler2cur.f90 
!>    
!

      program euler2cur
	  use eulerAccess
      implicit none
      integer,parameter             :: neulerunit=110,ncurunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnameuler, fnamcur
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 2 ) then
            write(*,*) 'arguments: eulerfile curfile [title]'
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

      call GET_COMMAND_ARGUMENT(1,fnameuler,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamcur,status=iuerr)
      
      write(*,*) trim(fnameuler), ' => ',trim(fnamcur)
      
      call readEuler(fnameuler,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading euler file.'
            call exit(11)
      endif

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


