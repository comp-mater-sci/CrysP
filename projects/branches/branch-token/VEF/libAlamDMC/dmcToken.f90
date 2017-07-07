!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2017-07-05
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> Token-based material signatures
module dmcToken
use,intrinsic :: iso_c_binding, only: C_NULL_CHAR
use criPath, only: max_pathlen
use criErrcodes
use fslis
implicit none

private

public :: Token

    !> Component that wraps essential token-related operation in alamDMC
    type :: Token
        
    private
        character(len=max_pathlen)  :: tokenfile_path = C_NULL_CHAR
    contains
        !> Set path to the token file.
        procedure,pass(this),public :: setTokenPath

        !> Verify if the token is valid and authentic
        procedure,pass(this),public :: verifyToken

        !> Verify file signature with the token.
        procedure,pass(this),public :: verifySignature
        
        !> Sign output file with the token
        procedure,pass(this),public :: signDatafile
        
    end type

contains


    integer function setTokenPath(this, token_path) result(info)
    implicit none
    class(Token),intent(inout)  :: this
    character(len=*),intent(in) :: token_path
    !
        if (len_trim(token_path) > 1 .and. len_trim(token_path) < max_pathlen) then
            ! Set token path to be C-API conformant
            this%tokenfile_path = trim(token_path)//C_NULL_CHAR
            info = criSuccess
        else
            info = criErr_BadArgs
        endif
    !
    end function
    
    
    integer function verifyToken(this) result(info)
    implicit none
    class(Token),intent(in)      :: this
    !
        CHOOSE(info, isTokenValid(this%tokenfile_path), criSuccess, criError)
    !
    end function
    
    
    integer function verifySignature(this, file_path) result(info)
    implicit none
    class(Token),intent(in)         :: this
    character(len=*),intent(in)     :: file_path
    !
        ! Set file path to be C-API conformant
        CHOOSE(info, isSignatureValid(trim(file_path)//C_NULL_CHAR, this%tokenfile_path, C_NULL_CHAR), criSuccess, criError)
    !
    end function
    
    
    integer function signDatafile(this, file_path) result(info)
    implicit none
    class(Token),intent(inout)      :: this
    character(len=*),intent(in)     :: file_path
    !
        ! Set file path to be C-API conformant
        CHOOSE(info, signFile(trim(file_path)//C_NULL_CHAR, this%tokenfile_path, C_NULL_CHAR) == ok, criSuccess, criError)
    !
    end function
end module
