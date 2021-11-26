!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2011-11-02
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file ori2smt.f90 
!>    
!
program ori2smt
      use smtAccess
      use oriAccess
      implicit none
      integer,parameter             :: noriunit=110,nsmtunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamcub, fnamsmt
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,header,buf
      character(len=10)             :: stylename
      integer                       :: nstyle
      integer,parameter             :: titlearg = 4
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 3 ) then
            write(*,*) 'arguments: orifile smtfile style [title]'
            write(*,*)  'available styles: bare, plain, full'
            call exit(10)
      endif
      ! Form title
      title = ''
      do i=titlearg,argc
            call GET_COMMAND_ARGUMENT(i,buf,status=iuerr)
            if (i == titlearg ) then
                  title = trim(buf)
            else
                  title = trim(title) // ' ' // trim(buf)
            endif
      end do

      call GET_COMMAND_ARGUMENT(1,fnamcub,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamsmt,status=iuerr)
      call GET_COMMAND_ARGUMENT(3,stylename,status=iuerr)

      ! Interpret style name 
      nstyle = 0 
      if (trim(adjustl(stylename)) == 'bare') nstyle=1
      if (trim(adjustl(stylename)) == 'plain') nstyle=2
      if (trim(adjustl(stylename)) == 'full') nstyle=3
      if (nstyle == 0 ) then
            write(*,*) 'Bad style selector'
            call exit(10)
      endif
      ! Open files
      write(*,*) trim(fnamcub), ' => ',trim(fnamsmt)
      ! Open ORI file 
      open (unit=noriunit,file=TRIM(fnamcub),status='old',form='FORMATTED')
      call readOri(noriunit,micros,header,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading ORI file.'
            call exit(11)
      endif
      close(noriunit)
      ! Open SMT file
      open (unit=nsmtunit,file=TRIM(fnamsmt),status='unknown',form='FORMATTED')
      ! Mangle title
      if (len_trim(title) >= 1) then
            micros%TITLE = trim(title)
            write(*,*) 'title: ', title
      else
            micros%TITLE = trim(header)
            write(*,*) 'title: ', header
      endif

      call writeSMT(nsmtunit,micros,nstyle,iuerr)     
      if (iuerr /= 0) then
            write(*,*) 'Error writing SMT file.'
            call exit(11)
      endif
      close(nsmtunit)

end program 

 
