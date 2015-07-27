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
use altayState
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
        
        procedure,private :: loadMesostructure=> HDF5PersistenceScheme_loadMesostructure
        procedure,private :: loadGrainstate => HDF5PersistenceScheme_loadGrainstate
        
        final :: HDF5PersistenceScheme_finalize
        
    end type
    
    
contains

    
    !> Open HDF5 file and location inside it.
    subroutine HDF5PersistenceScheme_initialize(this, extpath, groupname, is_readonly, is_incremental, info)
    implicit none
    class(HDF5PersistenceScheme),intent(inout)  :: this
    character(len=*),intent(in)                 :: extpath
    character(len=*),intent(in)                 :: groupname
    logical,intent(in)                          :: is_readonly
    logical,intent(in)                          :: is_incremental
    integer,intent(out)                         :: info
    !
    character(len=len(extpath)) :: file_path, location_path
    character(len=:),allocatable :: container_path
    character(len=2) :: mode_selector
    !
        this%group_name = groupname
        this%is_incremental = is_incremental
        !
        call splitHDF5extpath(extpath, file_path, location_path, info)
        if (info /= 0) return
        ! Open the HDF5 file
        CHOOSE(mode_selector, is_readonly, 'r', 'w')
        info = this%file%open(path=file_path, mode=mode_selector)
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
    type(HDF5Context) :: context
    type(HDF5Access) :: texaccess
    integer,parameter :: max_intwidth = 32
    character(len=len_trim(this%group_name)+max_intwidth) :: collection_name
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
        ! Take all components of the state and save them into the collection

        ! Store the ODF data
        !> Orientations of discrete ODF.
        context%group_id = this%collection%object_id
        call texaccess%initialize(context, .false., info)
        if (info /= criSuccess) return
        !state%texture
        info = texaccess%write(0, odf=state%texture)
        if (info /= criSuccess) return
        !
        ! Mesostructure:
        info = this%saveMesostructure(state%mesostructure)
        if (info /= criSuccess) return
        !
        !> Collection of the state of grains
        info = this%saveGrainstate(state%grainstates)
        if (info /= criSuccess) return
        
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
    type(altayStateVariables),intent(inout)     :: state
    integer,intent(out)                         :: info
    !
    type(HDF5Context) :: context
    type(HDF5Access) :: texaccess
    !
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
        info = criSuccess
    !
    end subroutine
    
    
    
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
