!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of the initial release: 2016-05-31 (based on dmcUDSA.f90)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Data types for stress evolution outputs
module dmcEvolutionOutputRecord
use criMathUtils, only: SRTensor
use dmcIncrementationControl, only: IncrementationControlVariables
implicit none

    !> Data outputed per increment of stress driven state evolution
    type :: IncrementOutputRecord
         
        double precision :: vm_strain = 0.D0
        double precision :: vm_strain_total = 0.D0
        
        double precision :: norm_P_abs = 0.D0
        
        double precision :: dotWonA = 0.D0
        double precision :: taylor_factor = 0.D0
        double precision :: scal_s = 0.D0
        double precision :: norm_SonA = 0.D0
        double precision :: R = 0.D0

        type(SRTensor) :: A
        type(SRTensor) :: SonA

        type(SRTensor)  :: P_inc_evol
        type(SRTensor)  :: S_evol
            
        type(IncrementationControlVariables) :: icv
        
    end type
    
end module
