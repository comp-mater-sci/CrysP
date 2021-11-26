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


    !> SDVs that result from linear programming
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
          
        !> CRSSs (not RSS??) of the potentially active slip systems
        double precision, dimension(Pancak2_max_activesystems) :: taurlp = 0.0d0
          
        !> Local strain rate (in vector format)
        double precision, dimension(DM_dev_dims)               :: localstrainrate = 0.0d0
          
    end type
    
    
    !> SDV that describe per-grain deformation rate, including slip rates on slip
    !> systems and shear on twinning systems.
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
    
    
    !> SDV per-grain
    type :: GrainSDV
        
        !> CRSS of all deformation systems
        type(CRSSData)          :: crss

        type(LinearProgrammingSDV)   :: solution
        
        type(DeformationRateSDV)   :: sliprat_solution
        
    end type
    
    
    type :: GrainClusterSolution
        
        type(GrainSDV),dimension(:),allocatable :: components
        
    end type


    !> Homogenized quantities per volume of material.
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


    !> State Dependent Variables (SDV)
    type :: altaySDV
        
        !> SDV per grain. Shape: [1:n_grains]
        type(GrainSDV),dimension(:), allocatable :: grain_sdv

        !> Homogenized SDV per phase. Shape: [1:n_phases]
        type(HomogenizedSDV),dimension(:),allocatable :: homogenized_phases
        
        !> Homogenized SDV in material.
        type(HomogenizedSDV) :: homogenized_material
        
        !> Maps (phase -> grains) per phase. Shape: [1:n_phases]
        type(MapData),dimension(:),allocatable :: phase_reverse_maps
        
    end type


    interface initialize
        module procedure altaySDV_initialize
    end interface
    
contains


    !> Initialize components of altaySDV object.
    !>
    !> The initialization allocates allocatable components. It also calculates
    !> the reverse maps (phase -> grains).
    subroutine altaySDV_initialize(this, state, info)
    implicit none
    type(altaySDV),intent(out)              :: this  !< Object to initialize
    !> State of the material (it must be already initialized)
    type(altayStateVariables),intent(in)    :: state
    integer,intent(out)                     :: info !< Exit code
    !
    integer :: n_grains, n_phases, i, ierr
    !
        info = criErr_BadArgs
        if (.not. associated(state%material)) return
        ALLOCATED_SIZE(n_phases, state%material%phases)
        ALLOCATED_SIZE(n_grains, state%grainstates%grains)
        !
        if ((n_phases < 1) .or. (n_grains < 1)) return
        !
        RETURN_ON_WITH(allocate(this%grain_sdv(n_grains), stat=ierr), ierr /= 0, info=criErr_MemAlloc)
        RETURN_ON_WITH(allocate(this%homogenized_phases(n_phases), stat=ierr), ierr /= 0, info=criErr_MemAlloc)
        RETURN_ON_WITH(allocate(this%phase_reverse_maps(n_phases), stat=ierr), ierr /= 0, info=criErr_MemAlloc)
        !
        ! Initialize per-phase sdv
        do i = 1, n_phases
            call GrainStateCollection_reverseMapping(state%grainstates, i, &
                                                     this%phase_reverse_maps(i)%map, info)
            if (info /= criSuccess) exit
        enddo
        !
        ! Initialize per-grain sdv
        !> \todo altaySliprate::SLIPRAT re-allocates the shearrate component
        !>       anyway. To be checked where the allocation should take place,
        !>       but this routine seems more appropriate (no re-allocation on every
        !>       entry to altaySliprate::SLIPRAT)
        do i = 1, n_grains
            call ShearRateData_init(this%grain_sdv(i)%sliprat_solution%shearrate, &
                                    state%grainstates%grains(i)%phase%deformationmechanism%n_systems,&
                                    info)
            if (info /= criSuccess) exit
        enddo
    !
    end subroutine
    
end module
