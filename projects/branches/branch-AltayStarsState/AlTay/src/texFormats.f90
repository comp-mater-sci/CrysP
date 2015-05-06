!
! $Id$
!

!> Dispatcher subroutines for IO operation on texture data files.
module altayTexFormats
use criErrcodes
use altayTexFormatConstants
use altayStateTypes
use altayTexAccess
use altaySmtAccess
use altayCurAccess
use altayCubAccess

implicit none
      
      
    contains

    !> Read texture data from file.
    !\todo documentation
    subroutine loadTexture(texfmt,fname,iblock,meso,texture,info,iounit)
    implicit none
    integer,intent(in)              :: texfmt !< Texture file format identifier
    character(len=*),intent(in)     :: fname  !< Path to the texture file
    !> If the texture file format permits multiple blocks of texture data, 
    !> `iblock` provides block number inside the texture file. 
    !> iblock must be >= 0. The first block is at iblock = 0.
    integer,intent(in)              :: iblock
    type(MesostructureState),target,intent(out) :: meso
    type(TextureData),target,intent(out)   :: texture
    integer,intent(out)             :: info
    integer,intent(in),optional     :: iounit ! If provided, fname will not be opened, but the IO unit will instead be used
    !
    integer :: nunit
    type(TextureAssembly) :: assembly
    !
        info = criError
        if (present(iounit)) then
            nunit = iounit
        else
            call openTextureFile(fname,texfmt, 'r', nunit, info)
            if (info /= criSuccess) return
        endif
        !
        assembly = TextureAssembly(texture, meso)
        !
        info =criErr_BadArgs
        select case(texfmt)
        case(TF_SMT)
            call SMTread(assembly, nunit, info)
        case(TF_CUR)
            call CURread(assembly, nunit, iblock, info)
        case(TF_CUB)
            call CUBread(assembly, nunit, info)
        end select
    !
    end subroutine

      
    !> Open a file for texture output
    subroutine openTextureFile(fname,texfmt,mode,iounit,info)
    implicit none
    character(len=*),intent(in)   :: fname    !< File name to be opened
    integer,intent(in)            :: texfmt   !< Format ID
    character(len=1),intent(in)   :: mode     !< Mode: ['r'|'w']
    integer,intent(out)           :: iounit   !< I/O unit
    integer,intent(out)           :: info     !< exit code
    !
    character(len=10) :: stat
    !
        info = criErr_BadArgs
        select case(mode)
        case('r')   
                stat = 'old'
        case('w')   
                stat = 'replace'
        case default 
                return
        end select
        !
        info = criErr_IOOpen
        select case(texfmt)
        case(TF_SMT,TF_CUR)     ! formatted
            open(newunit=iounit,file=trim(fname),status=trim(stat),form='formatted',iostat=info)
            if (info == 0) info = criSuccess
        case(TF_CUB)
            open(newunit=iounit,file=trim(fname),status=trim(stat),form='unformatted',iostat=info)
            if (info == 0) info = criSuccess
        case default
            info = criErr_BadArgs
        end select
    !
    end subroutine
      
    subroutine outputCurrentTexture(iounit,texfmt,meso,texture,full,info)
    implicit none
    integer,intent(in)            :: iounit   !< I/O unit
    integer,intent(in)            :: texfmt   !< Format ID
    
    type(MesostructureState),target,intent(in) :: meso
    type(TextureData),target,intent(in)  :: texture
    !> Flag: If true, both header and block are written, otherwise only 
    !> the block output is written out.
    logical,intent(in)            :: full
    integer,intent(out)           :: info
    !
    type(TextureAssembly) :: assembly
    !
        info = -1
        assembly = TextureAssembly(texture, meso)
        
        !> \todo write code for the available formats
        select case(texfmt)
        case(TF_SMT)
            call SMTwrite(assembly,iounit,info)
        case(TF_CUR)
            call CURwrite(assembly,iounit,full,info)
        case(TF_CUB)
              call CUBwrite(assembly,iounit,info)
        case default
                info = criErr_BadArgs
        end select
    !
    end subroutine
      
      
end module
