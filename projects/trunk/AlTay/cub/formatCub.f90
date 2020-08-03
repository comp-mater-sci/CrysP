!
! $Id: cub2cur.f90 822 2011-11-04 09:07:00Z jgawad $
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
!>    \file cub2cur.f90 
!>    
!

      program formatCub
      use cubAccess
      implicit none
      integer,parameter             :: nucubunit=110,nfcubunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamucub, fnamfcub
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 2 ) then
            write(*,*) 'arguments: UnformattedCub FormattedCub'
            call exit(10)
      endif
      ! Form title
      title=''
      write(*,*) 'title: ', title

      call GET_COMMAND_ARGUMENT(1,fnamucub,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamfcub,status=iuerr)
      
      write(*,*) trim(fnamucub), ' => ',trim(fnamfcub)
      ! Open unformatted CUB file 
      open (unit=nucubunit,file=TRIM(fnamucub),                            &
                     status='old',form='UNFORMATTED')
      
      call readCub(nucubunit,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading CUB file.'
            call exit(11)
      endif
      close(nucubunit)
      ! Open formatted CUB file
      open (unit=nfcubunit,file=TRIM(fnamfcub),                            &
                     status='unknown',form='FORMATTED')
      ! Mangle title
      if (len_trim(title) > 0 ) micros%TITLE = trim(title)
      
      call writeFormattedCub(nfcubunit,micros,iuerr)     
      if (iuerr /= 0) then
            write(*,*) 'Error writing CUB file.'
            call exit(11)
      endif
      close(nfcubunit)

      end program 


