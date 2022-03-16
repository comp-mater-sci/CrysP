! $Id: YLPResult.f90 3483 2021-03-02 14:42:38Z Matthias.Bonisch $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2017-04-07, as a result of refactoring 'stressDrivenModule.f90'
!>    $Revision: 3483 $
!>    $Date: 2021-03-02 15:42:38 +0100 (Tue, 02 Mar 2021) $
!>
!>    History of modifications: (see svn log)

!> Datatypes that simplify work with results of multilevelYLP and procedures
!> that operate on these datatypes.
module dmcYLPResult
use criErrcodes
use criMathUtils
use alamYLPConstants, only: alamEval_vSD_dim
implicit none

    !> Datatype to store results of iterative search
    type :: YLPResult
        double precision,dimension(alamEval_vSD_dim) :: vA = 0.D0       !< Strain rate mode on yield locus
        double precision,dimension(alamEval_vSD_dim) :: vS = 0.D0       !< Imposed stress
        double precision,dimension(alamEval_vSD_dim) :: vSonA = 0.D0    !< Stress corresponding to A
        double precision,dimension(alamEval_vSD_dim) :: vSonAn = 0.D0   !< Stress mode corresponding to A
        double precision :: R = 0.D0                  !< Norm of stress residual
        double precision :: dotWonA = 0.D0            !< Work rate corresponding to vA and vSonA
        double precision :: scal_s = 0.D0             !< norm2(vSonA) / vS_length
        double precision :: vS_length = 0.D0          !< norm2(vS)
    end type


    !> Constructors of YLPResult type
    interface YLPResult
        module procedure YLPResult_init
    end interface

    double precision, parameter,private :: default_residual_tolerance_factor = 5.D0
    
    !> Default angular tolerance (given in degrees)
    double precision, parameter,private :: default_angular_tolerance = 0.25D0
    
    !> Datatype for commonly used tolerances that the YLPResult should meet to be
    !> an acceptable solution. The defaults are 
    type :: YLPResultTolerance
        !> Tolerance in terms of residual norm
        double precision    :: residual_tolerance_factor = default_residual_tolerance_factor
        !> Tolerance in terms of angle between requested stess and identified stress (in degrees)
        double precision    :: angular_tolerance = default_angular_tolerance
    end type
    
contains


    !> Create YLPResult from arbitrary vS and performs normalization.
    !>
    !> \post A correctly initialized result has non-zero vS_length field.
    pure function YLPResult_init(vS) result(res)
    implicit none
    type(YLPResult) :: res
    double precision,dimension(alamEval_vSD_dim),intent(in) :: vS
    !
        res%vS_length = norm2(vS)
        if (res%vS_length > 0.D0) res%vS = vS / res%vS_length
    !
    end function


    !> Derive dependant fields from properly initialized and evaluated YLPResult;
    !>
    !> This requires fields: vS, vA and vS_length.
    !> \return criErr_BadArgs if input ylp_result contains wrong data.
    integer function deriveYLPResult(ylp_result) result(info)
    implicit none
    type(YLPResult),intent(inout)   :: ylp_result
    !
    double precision :: SonA_norm
    !
        info = criErr_BadArgs
        SonA_norm = norm2(ylp_result%vSonA)
        if ((ylp_result%vS_length < epsilon(0.D0)) .or. (SonA_norm < epsilon(0.D0))) return
        !
        ylp_result%dotWonA = dot_product(ylp_result%vA, ylp_result%vSonA)
        ylp_result%scal_s = SonA_norm / ylp_result%vS_length
        ! Calculate normalized stess
        ylp_result%vSonAn = ylp_result%vSonA / SonA_norm
        info = criSuccess
    !
    end function


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


    !> Check if ylp_result is within all tolerances.
    !> \returns .true. if ylp_result passes all tolerances (ie. is acceptable),
    !> .false. otherwise.
    !>
    !> The procedure checks the norm of residual error and angle between the solution
    !> stress and requested stress. These two quantities are very much correlated,
    !> except for unconverged solution where they are not. For this reason it 
    !> appears better to check both.
    pure logical function checkYLPResult(ylp_result, tolerance, target_residual) result(val)
    implicit none
    type(YLPResult),intent(in)          :: ylp_result
    type(YLPResultTolerance),intent(in) :: tolerance
    double precision,intent(in)         :: target_residual
    !
        val = (ylp_result%R < tolerance%residual_tolerance_factor * target_residual) &
              .and. &
              (vec_angle(ylp_result%vS, ylp_result%vSonA) < deg2rad(tolerance%angular_tolerance))
    !
    end function
    
end module
