!
! $Id$
!

module TexFormats
use dynfil
use smtAccess
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
            case(TF_SMT)
                  call SMTreadHeader(nunit,TF_MaxPoints,filetitle,info)
                  if (info /= 0) return
                  call SMTreadBlock(nunit,TF_MaxPoints,info)
            !
            case(TF_CUR)
                  call CURreadTitle(nunit,filetitle,info)
                  if (info /= 0) return
                  call CURreadBlock(nunit,iblock,TF_MaxPoints,info)
            !
            case(TF_CUB)
                  call CURreadTitle(nunit,filetitle,info)
                  if (info /= 0) return
                  call CUBreadBlock(nunit,TF_MaxPoints,info)
            end select
      !
      end subroutine

      
      !> Open a file for texture output
      integer function openTextureFile(iounit,fname,texfmt,mode) result(info)
      implicit none
      integer,intent(in)            :: iounit   !< I/O unit
      character(len=*),intent(in)   :: fname    !< File name to be opened
      integer,intent(in)            :: texfmt   !< Format ID
      character(len=1),intent(in)   :: mode     !< Mode: ['r'|'w'] 
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
      
      subroutine outputCurrentTexture(iounit,texfmt,full,info)
      implicit none
      integer,intent(in)            :: iounit   !< I/O unit
      integer,intent(in)            :: texfmt   !< Format ID
      !< If true, both header and block are written, otherwise only the block output is written out.
      logical,intent(in)            :: full     
      integer,intent(out)           :: info
      !
            info = -1
            ! TODO: write code for the available formats
            select case(texfmt)
            case(TF_SMT)
                  if (full) call SMTwriteHeader(iounit,filetitle,info)
                  if (info == 0) call SMTwriteBlock(iounit,info)
            case(TF_CUR)
                  if (full) call CURwriteTitle(iounit,filetitle,info)
                  if (info == 0) call CURwriteBlock(iounit,info)
            case(TF_CUB)
                  call CUBwriteBlock(iounit,info)
            case default
                  info = -1
            end select
      !
      end subroutine
 
      
      
end module