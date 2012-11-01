!
! $Id$
!

!> Provides named constants for supported file formats of texture data.
module TexFormatConstants
implicit none

      !>@{ \name Named constants for supported texture file formats (aka FormatID)
      integer,parameter :: TF_SMT  = 1
      integer,parameter :: TF_CUR  = 2
      integer,parameter :: TF_CUB  = 3
      integer,parameter :: TF_HDF5 = 5

      !>@}

end module
