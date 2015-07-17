!
! $Id$
!

!> Dispatcher subroutines for IO operation on texture data files.
module altayTexFormats
use criErrcodes
use altayTexFormatConstants
use altaySmtAccess
use altayCurAccess
use altayCubAccess

implicit none
      
      
    contains
    
    !> Creates an instance of relevant texture access method
    !> \todo consider generalization of the instance OR rename the function.
    function textureAccessFactory(id, context, readonly, info) result(instance)
    implicit none
    class(TextureRawFileAccess),pointer     :: instance
    integer,intent(in)                      :: id
    class(RawFileContext),intent(inout)     :: context
    logical,intent(in)                      :: readonly
    integer,intent(out)                     :: info
    !
        nullify(instance)
        select case(id)
        case(TF_SMT)
            allocate(SMTFileAccess :: instance)
        case(TF_CUR)
            allocate(CURFileAccess :: instance)
        case(TF_CUB)
            allocate(CUBFileAccess :: instance)
!        case(TF_HDF5)
            !> \todo Add the actual allocation of appropriate instance
!            continue
            ! allocate(HDF5FileAccess :: instance)
        end select
        call instance%initialize(context, readonly, info)
        if (info /= criSuccess) then
            deallocate(instance)
            nullify(instance)
        endif
    !
    end function
    
end module
