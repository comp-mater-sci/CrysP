!
! $Id$
!
#ifndef HDF5_DISABLE
!> State persistence based on HDF5 API.
!>
!> \todo Format version information must be stored. This will help in keeping
!>       backward compatibility in the future. Appropriate attribute should
!>       be set per collection/group.
!>
#include "criMacros.fpp"
module altayHDF5PersistenceScheme
use criErrcodes
use criAlgorithm
use FH5
use altayStateTypes
use altayStatePersistence
use altayHDF5Context
use altayHDF5Access
implicit none

    character(len=6), parameter :: altayHDF5PersistenceScheme_location_name = 'AlTay'
    
    type,extends(StatePersistenceScheme) :: HDF5PersistenceScheme


        logical                         :: is_incremental = .false.
        
        !> Name of the group that stores the collection.
        !>
        !> If is_incremental, the name will be recycled as a prefix to form 
        !> the actual group name.
        character(len=:),allocatable    :: group_name
        
        integer                         :: counter = 0

        type(FH5File)                   :: file
        type(FH5Group)                  :: container
        type(FH5Group)                  :: collection
       
    contains
    
        procedure :: initialize => HDF5PersistenceScheme_initialize
        procedure :: saveState => HDF5PersistenceScheme_saveState
        procedure :: loadState => HDF5PersistenceScheme_loadState

        procedure,private :: saveMesostructure=> HDF5PersistenceScheme_saveMesostructure
        procedure,private :: saveGrainstate => HDF5PersistenceScheme_saveGrainstate
        procedure,private :: savePhaseData => HDF5PersistenceScheme_savePhaseData
        
        procedure,private :: loadMesostructure=> HDF5PersistenceScheme_loadMesostructure
        procedure,private :: loadGrainstate => HDF5PersistenceScheme_loadGrainstate
        
        final :: HDF5PersistenceScheme_finalize
        
    end type
    
    
contains

    
    !> Open HDF5 file and location inside it.
    subroutine HDF5PersistenceScheme_initialize(this, extpath, groupname, access_mode, is_incremental, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    character(len=*),intent(in)                 :: extpath
    character(len=*),intent(in)                 :: groupname
    integer,intent(in)                          :: access_mode
    logical,intent(in)                          :: is_incremental
    integer,intent(out)                         :: info
    !
    character(len=len(extpath)) :: file_path, location_path
    character(len=:),allocatable :: container_path
    character(len=2) :: mode_selector
    !
        info = criErr_BadArgs
        if (.not. any(access_mode == StatePersistence_AccessModes)) return
        this%access_mode = access_mode
        this%group_name = groupname
        this%is_incremental = is_incremental
        !
        call splitHDF5extpath(extpath, file_path, location_path, info)
        if (info /= criSuccess) return
        ! Open the HDF5 file
        select case(access_mode)
        case(StatePersistence_Read)
            mode_selector = 'r'
        case(StatePersistence_Write)
            mode_selector = 'w'
        case(StatePersistence_Append)
            mode_selector = 'rw'
        end select
        !
        info = this%file%open(path=file_path, mode=mode_selector)
        if (info /= criSuccess) return
        ! Make sure the container is in place, create it.
        container_path = trim(location_path)//FH5_path_sep//altayHDF5PersistenceScheme_location_name
        info = this%container%create(this%file, container_path)
        !> \todo: put attributes on the location: version, meta-data, ...
    !
    end subroutine
    
    
    !> Release resources and finalize the instance.
    subroutine HDF5PersistenceScheme_finalize(this)
    implicit none
    type(HDF5PersistenceScheme),intent(inout) :: this
    !
    integer :: info
    !
        info = this%collection%close()
        info = this%container%close()
        info = this%file%close()
    !
    end subroutine
    
    
    
    subroutine HDF5PersistenceScheme_saveState(this, state, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)   :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(out)                         :: info
    !
    integer,parameter :: max_intwidth = 32
    character(len=len_trim(this%group_name)+max_intwidth) :: collection_name
    integer :: phase_id
    !
        if (this%is_incremental) then
            collection_name = trim(this%group_name)//trim(tostring(this%counter,max_intwidth))
        else
            collection_name = trim(this%group_name)
        endif
        this%counter = this%counter + 1
        !
        ! Create group for the collection
        info = this%collection%create(this%container, collection_name)
        if (info /= criSuccess) return
        !
        ! Take all components of the state and save them into the collection.
        !
        
        !
        ! Part 1: Global data
        !
        ! Mesostructure:
        info = this%saveMesostructure(state%mesostructure)
        if (info /= criSuccess) return
        !
        !> Collection of the state of grains
        info = this%saveGrainstate(state%grainstates)
        if (info /= criSuccess) return

        !
        ! Part 2: Phase data
        do phase_id = lbound(state%material%phases,dim=1), ubound(state%material%phases,dim=1)
            info = this%savePhaseData(state, phase_id)
            if (info /= criSuccess) return
        enddo
        !
        ! Part 3: Inter-phase data

#ifdef PEBP_ENABLED
        ! State variables of the DSH hardening law.
        ! state%dsh_state
        continue
#endif
        ! State variables of the Kocks-Mecking hardening law.
        
        ! Other components
        !> \todo
        !!if (allocated(state%km_state)) then
        !!    continue
        !!endif
        info = this%collection%close()

    !
    end subroutine

    
    subroutine HDF5PersistenceScheme_loadState(this, state, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(inout)    :: state
    integer,intent(out)                         :: info
    !
    type(HDF5Context) :: context
    type(HDF5Access) :: texaccess
    !
#ifdef FIXME_ENABLE
        !
        ! Create group for the collection
        info = this%collection%open(this%container, this%group_name)
        if (info /= criSuccess) return
        !
        ! Take all components of the state from the collection

        ! Load the ODF data
        context%group_id = this%collection%object_id
        call texaccess%initialize(context, .true., info)
        if (info /= criSuccess) return
        !
        !state%texture
        info = texaccess%read(0, odf=state%texture)
        if (info /= criSuccess) return
        !
        ! Mesostructure:
        info = this%loadMesostructure(state%mesostructure)
        if (info /= criSuccess) return
        !
        ! Collection of the state of grains
        ! Initialize first:
        call state%grainstates%initialize(state%texture, info)
        if (info /= criSuccess) return
        ! Load the grains
        info = this%loadGrainstate(state%grainstates)
        if (info /= criSuccess) return
        
#ifdef PEBP_ENABLED
        ! State variables of the DSH hardening law.
        ! state%dsh_state
        continue
#endif
        ! State variables of the Kocks-Mecking hardening law.
        
        ! Other components

        info = this%collection%close()
#endif
        info = criSuccess
    !
    end subroutine
    
    integer function HDF5PersistenceScheme_savePhaseData(this, state, phase_id) result(info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(altayStateVariables),intent(in)        :: state
    integer,intent(in)                          :: phase_id
    !
    type(FH5GroupScoped)   :: group
    type(FH5Dataset)       :: map_dataset
    type(HDF5Context) :: context
    type(HDF5Access) :: texaccess
    type(DiscreteODF) :: odf
    integer,dimension(:),allocatable :: map
    character(len=:),allocatable :: group_name
        !
        ! Create group for the phase
        associate(phase_name => state%material%phases(phase_id)%name)
            CHOOSE(group_name, len_trim(phase_name) > 0, phase_name, 'phase' // tostring(phase_id,3))
            info = group%create(this%collection, group_name)
        end associate
        context%group_id = group%object_id
        ! Store the ODF data
        !
        ! Get ODF of the phase
        call GrainStateCollection_DiscreteODF(state%grainstates, odf, phase_id, info) 
        if (info /= criSuccess) return
        call texaccess%initialize(context, .false., info)
        if (info /= criSuccess) return
        !state%texture
        info = texaccess%write(0, odf)
        if (info /= criSuccess) return
        !
        ! Mapping from phase to state%grainstates
        call GrainStateCollection_reverseMapping(state%grainstates,phase_id, map, info)
#ifdef FULL_MAP
        !> \todo Consider more compact representation: if map includes 
        !>       consequtive numbers, it is enough to store min & max
        !>       and put appropriate attribute on the set.
        info = FH5Dataset_init(map_dataset, group, &
                                shape=shape(map), &
                                compression=FH5_compression_zip)
        if (info /= criSuccess) return
        info = map_dataset%write(, map)
        if (info /= criSuccess) return
        info = map_dataset%close()
#else
       info = map_dataset%make(group, 'state_map', map, &
                               compression=FH5_compression_zip)
#endif
    !
    end function
    
    integer function HDF5PersistenceScheme_saveMesostructure(this, state) result(info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(MesostructureState),intent(in)        :: state
    type(FH5Dataset) :: dataset
    type(FH5GroupScoped)   :: group
    !
        info = group%create(this%collection, 'MesostructureState')
        if (info /= criSuccess) return
        info = FH5Dataset_init(dataset, group, &
                               shape=shape(state%deformationgradient), &
                               compression=FH5_compression_zip)
        if (info /= criSuccess) return
        info = dataset%write('deformationgradient', state%deformationgradient)
        if (info /= criSuccess) return
        info = dataset%close()
    !
    end function
    
    
    integer function HDF5PersistenceScheme_saveGrainstate(this, state) result(info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(GrainStateCollection),intent(in)                 :: state
    !
    type(FH5Dataset) :: dataset
    type(FH5GroupScoped)   :: group
    double precision,dimension(:,:),allocatable :: tmp_arr
    integer,parameter :: nvariables = 1
    integer :: npoints
    !
        info = group%create(this%collection, 'GrainStateCollection')
        if (info /= criSuccess) return
        ! Make temporary array
        npoints = size(state%grainstate)
        allocate(tmp_arr(npoints,nvariables))
        tmp_arr(:,1) = state%grainstate(:)%accumulatedshear
        !
        info = FH5Dataset_init(dataset, group, &
                               shape=shape(tmp_arr), &
                               compression=FH5_compression_zip)
        if (info /= criSuccess) return
        info = dataset%write('grainstate', tmp_arr)
        if (info /= criSuccess) return
        info = dataset%close()
    !
    end function
    
    
    integer function HDF5PersistenceScheme_loadMesostructure(this, state) result(info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)      :: this
    type(MesostructureState),intent(inout)          :: state
    !
    type(FH5Dataset) :: dataset
    type(FH5GroupScoped)   :: group
    double precision,dimension(:,:),allocatable :: tmp_arr
    !
        info = group%open(this%collection, 'MesostructureState')
        if (info /= criSuccess) return
        info = FH5Dataset_init(dataset, group, 'deformationgradient')
        if (info /= criSuccess) return
        info = dataset%read(tmp_arr)
        if ((info == criSuccess) .and. &
            all(shape(state%deformationgradient) == shape(tmp_arr))) then
            state%deformationgradient = tmp_arr
        endif
        info = dataset%close()
    !
    end function
    
    integer function HDF5PersistenceScheme_loadGrainstate(this, state) result(info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    type(GrainStateCollection),intent(inout)    :: state
    !
    type(FH5Dataset) :: dataset
    type(FH5GroupScoped)   :: group
    double precision,dimension(:,:),allocatable :: tmp_arr
    integer,parameter :: nvariables = 1
    integer :: npoints
    !
        info = criErr_BadArgs
        if (.not. allocated(state%grainstate)) return
        npoints = size(state%grainstate)
        !
        info = group%open(this%collection, 'GrainStateCollection')
        if (info /= criSuccess) return
        !
        info = FH5Dataset_init(dataset, group, 'grainstate')
        if (info == criSuccess) then
            if (dataset%read(tmp_arr) == criSuccess) then
                info = criErr_BadDims
                if (npoints /= size(tmp_arr, dim=1)) return
                !
                state%grainstate(:)%accumulatedshear = tmp_arr(:,1)
                info = criSuccess
            endif
        endif
        info = dataset%close()
    !
    end function
    

    
end module

#endif
