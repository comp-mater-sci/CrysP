!
! $Id: smt2cub.f90 822 2011-11-04 09:07:00Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2010-02-16
!>    $Revision: 822 $
!>    $Date: 2011-11-04 10:07:00 +0100 (Fri, 04 Nov 2011) $
!>
!>    History of modifications: (see svn log)
!
!
!>    \file cub2smt.f90 
!>    
!


program smt2cub
      use cubAccess
      use smtAccess
      implicit none
      integer,parameter             :: nsmtunit=110,ncubunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamsmt, fnamcub
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
	  integer                       :: nstyle
      integer,parameter             :: titlearg = 3
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 2 ) then
            write(*,*) 'arguments: smtfile cubfile [title]'
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

      call GET_COMMAND_ARGUMENT(1,fnamsmt,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamcub,status=iuerr)

      ! Open files
      write(*,*) trim(fnamsmt), ' => ',trim(fnamcub)
      ! Open CUB file 
      open (unit=nsmtunit,file=TRIM(fnamsmt),status='old',form='FORMATTED')
     
      nstyle = 1
      call readSMT(nsmtunit,nstyle,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading SMT file.'
            call exit(11)
      endif
      close(nsmtunit)
      ! Open CUB file
      open (unit=ncubunit,file=TRIM(fnamcub),status='unknown',form='UNFORMATTED')
      ! Mangle title
      if (len_trim(title) >= 1) then
             micros%TITLE = trim(title)
            write(*,*) 'title: ', title
      endif

      call writeCub(ncubunit,micros,.true.,.false.,iuerr)     
      if (iuerr /= 0) then
            write(*,*) 'Error writing CUB file.'
            call exit(11)
      endif
      close(ncubunit)

end program 

      
