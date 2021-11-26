!
! $Id: smt2cur.f90 822 2011-11-04 09:07:00Z jgawad $
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
!>    \file smt2cur.f90 
!>    
!

      program smt2cur
	  use smtAccess
	  use cubAccess
      implicit none
      integer,parameter             :: nsmtunit=110,ncurunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamsmt, fnamcur
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
	  character(LEN=10)             :: st_style
	  integer                       :: style
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 3 ) then
            write(*,*) 'arguments: smtfile curfile style [title]'
			write(*,*) 'style can be 1 (bare), 2 (with weights), or 3 (full format)'
            call exit(10)
      endif
      ! Form title
      title=''
      do i=4,argc
            call GET_COMMAND_ARGUMENT(i,buf,status=iuerr)
            if (i == 4 ) then
                  title = trim(buf)
            else
                  title = trim(title) // ' ' // trim(buf)
            endif
      end do
      write(*,*) 'title: ', title

      call GET_COMMAND_ARGUMENT(1,fnamsmt,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamcur,status=iuerr)
	  call GET_COMMAND_ARGUMENT(3,st_style,status=iuerr)
      
      write(*,*) trim(fnamsmt), ' => ',trim(fnamcur)
	  
	  ! Open SMT file 
      open (unit=nsmtunit,file=TRIM(fnamsmt),status='old',form='FORMATTED')
      
	  read(st_style,*) style
      call readsmt(nsmtunit,style,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading smt file.'
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


