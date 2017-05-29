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
!>    \file oriAccess.f90 
!>    
!
module oriAccess

contains

      subroutine readOri(nunit,micros,header,iuerr)
      use cubAccess
      implicit none       
            integer,intent(in)                        :: nunit
            type(microsDesc),intent(inout)            :: micros
            character(len=ctitlelen),intent(out)      :: header
            integer,intent(out)                       :: iuerr
      !
            integer :: norient,ioerr = 0, i
            character(len=ctitlelen)      :: buf
            
            iuerr = 1
            header = ''
            ioerr = 0
            norient = 0
            ! read two first lines
            read(nunit,'(A)',iostat=ioerr) header
            if (ioerr /= 0) return
            read(nunit,'(A)',iostat=ioerr) buf 
            ! Merge it
            header = trim(adjustl(header)) // ' ' // trim(buf)
            ! 3rd line
            read(nunit,fmt=*,iostat=ioerr) norient  ! Only the first number is important
            if ((ioerr /= 0) .or. (norient <= 0)) return
            call allocateMicros(micros,norient,ioerr)
            if (ioerr /= 0) return
            do i=1,norient
                  associate (grain => micros%grains(i))
                        read(nunit, fmt=*,iostat=ioerr) grain%PHI1, grain%PHI, grain%PHI2, grain%GEW
                  end associate
                  if (ioerr /= 0) exit
            enddo      
            if ((ioerr == 0) .and. (i >= norient)) iuerr = 0

      end subroutine

end module


