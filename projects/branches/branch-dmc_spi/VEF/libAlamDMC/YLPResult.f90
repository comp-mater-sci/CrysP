! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2017-04-07, as a result of refactoring 'stressDrivenModule.f90'
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!>
module dmcYLPResult
use criErrcodes
use alamYLPConstants, only: alamEval_vSD_dim
implicit none

    !> Datatype to store results of iterative search
    type :: YLPResult
        double precision,dimension(alamEval_vSD_dim) :: vA = 0.D0 
        double precision,dimension(alamEval_vSD_dim) :: vS = 0.D0 
        double precision,dimension(alamEval_vSD_dim) :: vSonA = 0.D0 
        double precision,dimension(alamEval_vSD_dim) :: vSonAn = 0.D0 
        double precision :: R = 0.D0
        double precision :: dotWonA = 0.D0
        double precision :: scal_s = 0.D0
    end type

contains


    !> Print detailed info about YLP solution based on the content of YLPResult 
    !> object.
    integer function printYLPResult(iounit, ylp_result) result(info)
    implicit none
    integer,intent(in)              :: iounit
    type(YLPResult),intent(in)      :: ylp_result
    !
        write(iounit,fmt=100)
        write(iounit,fmt=200) 'Requested stress:', ylp_result%vS
        write(iounit,fmt=200) 'Identified normalized stress:', ylp_result%vSonAn
        write(iounit,fmt=201) 'Norm of stress residual:', ylp_result%R
        write(iounit,fmt=100)
        write(iounit,fmt=200) 'Stress on vA:', ylp_result%vSonA
        write(iounit,fmt=201) 'Norm of stress on vA:', norm2(ylp_result%vSonA)
        write(iounit,fmt=100)
        !
        info = criSuccess
        !
        100 format()
        200 format(A,T40,5(E12.5,1X))
        201 format(A,T40,E12.5)
    !
    end function

end module
