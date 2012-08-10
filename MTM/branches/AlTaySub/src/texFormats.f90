!
! $Id$
!

module TexFormats
use dynfil
use curAccess
use cubAccess
implicit none

      !>@{ \name Named constants for supported texture file formats (aka FormatID)
      integer,parameter :: TF_SMT  = 1
      integer,parameter :: TF_CUR  = 2
      integer,parameter :: TF_CUB  = 3
      integer,parameter :: TF_HDF5 = 5

      !>@}
      
      integer,parameter :: TF_MaxPoints = 30000
      
contains
      
      subroutine loadTexture(texfmt,nunit,fname,iblock,info)
      implicit none
      integer,intent(in)            :: texfmt
      integer,intent(inout)         :: nunit
      character(len=*),intent(in)   :: fname
      integer,intent(in)            :: iblock
      integer,intent(out)           :: info
      !
            info = -1
            if (openTextureFile(nunit,fname,texfmt,'r') /= 0) return
            !
            select case(texfmt)
            case(2)     ! CUR
                  call CURreadTitle(nunit,filetitle,info)
                  call CURreadBlock(nunit,iblock,TF_MaxPoints,info)
            case(3)
                  call CURreadTitle(nunit,filetitle,info)
                  call CUBreadBlock(nunit,TF_MaxPoints,info)
            end select
      !
      end subroutine

      
      !> Open a file for texture output
      integer function openTextureFile(iounit,fname,texfmt,mode) result(info)
      implicit none
      integer,intent(in)            :: iounit   !< I/O unit
      character(len=*)              :: fname    !< File name to be open
      integer,intent(in)            :: texfmt   !< Format ID
      character(len=1),intent(in)   :: mode     !< Mode: r / w 
      !
      character(len=10) :: stat
      !
            info = -1
            select case(mode)
            case('r')   
                  stat = 'old'
            case('w')   
                  stat = 'replace'
            case default 
                  return
            end select
            !
            select case(texfmt)
            case(TF_SMT,TF_CUR)     ! formatted
                  open(unit=iounit,file=trim(fname),status=trim(stat),form='formatted',iostat=info)
                  if (info /= 0) return
            case(TF_CUB)
                  open(unit=iounit,file=trim(fname),status=trim(stat),form='unformatted',iostat=info)
            case default
                  info = -1
            end select
      !
      end function
      
      subroutine outputCurrentTexture(iounit,texfmt,info)
      implicit none
      integer,intent(in)            :: iounit   !< I/O unit
      integer,intent(in)            :: texfmt   !< Format ID
      integer,intent(out)           :: info
      !
            info = -1
            ! TODO: write code for the available formats
            select case(texfmt)

            case default
                  info = -1
            end select
      !
      end subroutine
 
      
      
end module