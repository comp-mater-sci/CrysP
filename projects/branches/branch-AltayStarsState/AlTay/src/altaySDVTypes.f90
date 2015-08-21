!
! $Id$
!

#include "criMacros.fpp"

!> altaySDVTypes defines datatypes for State-Derived Variables
module altaySDVTypes
use altayCRSSTypes
use altayDeformationMechanismConstants, only: DM_dev_dims
use altayMaterialTypes
use altayStateTypes
implicit none
    
    !> \todo Find a more appropriate place for this pretty much generic type
    type :: MapData
        integer,dimension(:),allocatable :: map
    end type

    
    
    !> \fixme get rid of this constant or at least rename it
    integer, parameter, public ::          Pancak2_max_activesystems = 8

    
    type :: LinearProgrammingSDV
          
        !> local stress expressed in the sample reference frame
        double precision, dimension(3,3)                       :: stress_sam = 0.0d0
          
        !> symmetric part of (non-normalized) relaxation tensor, in sample reference system
        double precision, dimension(3,3)                       :: relaxationrate_sam = 0.0d0
          
        !> anti-symmetric part of (non-normalized) relaxation tensor, in sample reference system
        double precision, dimension(3,3)                       :: relaxationspin_sam = 0.0d0
          
        !> number of potentially active deformation systems
        integer                                                :: nactiv = 0
          
        !> Indices of the potentially active slip systems
        integer,dimension(Pancak2_max_activesystems)           :: indact = 0
          
        !> Slip rates, normalized by von Mises equivalent strain rate, of the potentially active slip systems
        double precision, dimension(Pancak2_max_activesystems) :: sliplp = 0.0d0
          
        !> CRSSs of the potentially active slip systems
        double precision, dimension(Pancak2_max_activesystems) :: taurlp = 0.0d0
          
        !> Local strain rate (in vector format)
        double precision, dimension(DM_dev_dims)               :: localstrainrate = 0.0d0
          
    end type
    
    
    type :: DeformationRateSDV
        !> Shear rates over all deformation systems (slip and twinning systems)
        !> for given grain 
        type(ShearRateData) :: shearrate
        !> Total (sum of absolute values of) shear rate over all deformation 
        !> systems (slip and twinning systems)
        double precision    :: totalshearrate = 0.0D0
        !> Taylor factor for given grain
        double precision    :: taylorfactor = 0.0D0
        !> Work rate for given grain
        double precision    :: workrate = 0.0D0
        !> von Mises equivalent stress for given grain
        double precision    :: vMeqstress = 0.0D0
    end type
    
    
    type :: GrainSDV
        
        !> CRSS of all deformation systems
        type(CRSSData)          :: crss

        type(LinearProgrammingSDV)   :: solution
        
        type(DeformationRateSDV)   :: sliprat_solution
        
    end type
    
    
    
    type :: HomogenizedSDV
        !> Macroscopic (homogenized) stress
        double precision,dimension(3,3)     :: stress_tensor = 0.D0
        !> Macroscopic (homogenized) Taylor factor
        double precision                    :: taylor_factor = 0.D0
        !> Strain Rate Heterogeneity in polycrystal. Non-zero only for models that consider clusters of grains:
        !> \f$ \kappa = (||d-D||) / ||D|| \f$
        double precision                    :: strain_rate_heterogeneity = 0.D0
        !> Macroscopic stress, defined as the work conjugate to D_vM: 
        !> \f$ \sigma_{eq} = (\mathbf{S} \cdot \mathbf{D}) / D_{vM} \f$
        double precision                    :: equivalent_stress = 0.D0
        !> Macroscopic (homogenized) effective von Mises stress
        double precision                    :: effective_stress = 0.D0
        !> Macroscopic (homogenized) plastic slip
        double precision                    :: homogenised_slip = 0.D0
        !> Macroscopic (homogenized) plastic slip - total over the calls
        double precision                    :: homogenised_slip_tot = 0.D0
        !> Macroscopic (imposed) effective von Mises strain - total over the steps
        double precision                    :: effective_macro_strain = 0.D0
        !> Macroscopic (imposed) effective von Mises strain - total over the calls
        double precision                    :: effective_macro_strain_tot = 0.D0
    end type
    
     
    type :: altaySDV
        
        type(GrainSDV),dimension(:), allocatable :: grain_sdv
       
        type(HomogenizedSDV),dimension(:),allocatable :: homogenized_phases
        
        type(HomogenizedSDV) :: homogenized_material
        
        type(MapData),dimension(:),allocatable :: phase_reverse_maps
        
    end type
    
    interface initialize
        module procedure altaySDV_initialize
    end interface
    
contains
    
    subroutine altaySDV_initialize(this, state, info)
    implicit none
    type(altaySDV),intent(out)              :: this
    type(altayStateVariables),intent(in)    :: state 
    integer,intent(out)                     :: info
    !
    integer :: n_grains, n_phases, phase_id
    !
        info = criErr_BadArgs
        ALLOCATED_SIZE(n_phases, state%material%phases)
        ALLOCATED_SIZE(n_grains, state%grainstates%grains)
        !
        if ((n_phases < 1) .or. (n_grains < 1)) return
        allocate(this%grain_sdv(n_grains))
        allocate(this%homogenized_phases(n_phases))
        allocate(this%phase_reverse_maps(n_phases))
        !
        do phase_id = 1, n_phases
            call GrainStateCollection_reverseMapping(state%grainstates, phase_id, &
                                                     this%phase_reverse_maps(phase_id)%map, info)
            if (info /= criSuccess) exit
        enddo
    !
    end subroutine
    
end module
