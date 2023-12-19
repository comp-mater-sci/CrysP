#include "criMacros.fpp"
!> Calculations of Equi-Work Contours
module dmcEWC
use criMathUtils
use criRange
use criNumerics
use criLinearMap
use criConfigReader
use commonConfig
use dmcStressDrivenEvolutionModule
use dmcIncrementationControl
use dmcEvolutionOutputrecord
use dmcResultFileOutput
use commonUtils
use utils

implicit none

    public:: EWCModule
    private

    integer, parameter, private:: n_base_vectors = 2


    type, extends(StressDrivenEvolutionModule):: EWCModule

        character(len = max_pathlen)              :: output_fname = ''

        logical                                 :: use_reference_stress_mode = .false.

        real(DP), dimension(6)   :: reference_stress_mode = 0.D0
        real(DP), dimension(6, n_base_vectors)   :: base_vectors = real(reshape( &
                                                [1, 0, 0, 0, 0, 0, & ! First base vector
                                                 0, 1, 0, 0, 0, 0], & ! second base vector
                                                [6, n_base_vectors]), DP)

        !> Range of angles that provide stress ratios
        class(range_type), pointer               :: ptr_theta_range => null()

        class(range_type), pointer               :: ptr_contourlevel_range => null()

        logical                                :: report_state = .false.

        !> Number of data points per contour level along regular evolution lines
        !>
        !> \todo Consider changing it into double: much less limiting control over the number
        !> of data points
        integer                                 :: n_intervals = 0
    contains

        !>@{ \name Interface methods of AbstractModule

        procedure, pass(this)    :: readConfig => EWCModule_readConfig

        procedure, pass(this)    :: run => EWCModule_run

        !>@}

        procedure, pass(this)    :: fileOutput => EWCModule_fileOutput

        procedure, pass(this)    :: fileOutputMeta => EWCModule_fileOutputMeta

    end type


contains

    integer function EWCModule_readConfig(this, cnfunit) result(info)
    implicit none
    class(EWCModule), intent(inout)            :: this
    integer, intent(in)                        :: cnfunit
    !
    integer:: i, id
    logical:: use_default_settings
    !
    ! Keywords for mode selection
    integer, parameter:: nmodes = 2, mode_reference_id = 1, mode_direct_id = 2
    type(MapItem), dimension(nmodes), parameter:: mode_keywords = [ MapItem('reference',mode_reference_id), &
                                                                   MapItem('direct',mode_direct_id) ]
    !
        info = this%StressDrivenEvolutionModule%ReadConfig(cnfunit)
        if (info /= VEF_OK) return
        info = VEF_ERROR
        ! Read parameters specific for the EWCModule
        !
        ! Check how the work levels are provided
        if (readKeyword(cnfunit, mode_keywords, id)) then
            select case(id)
            case(mode_reference_id)
                ! Evolution along the reference stress mode
                if (.not. readValue(cnfunit, this%reference_stress_mode)) return
                if (norm2(this%reference_stress_mode) < epsilon(0.D0)) then
                    write(display_unit, fmt = 900) 'Norm of the reference mode must not be zero'
                    return
                endif
                this%use_reference_stress_mode = .true.
                call IncrementationControlSettings_read(this%control, cnfunit, info, &
                                                        allowed=[scalingStrainTensor, &
                                                                 scalingStrainTensorIncrement, &
                                                                 scalingPlasticWork])
                if (info /= VEF_OK) return
            case(mode_direct_id)
                this%use_reference_stress_mode = .false.
            end select
        else
            write(display_unit, fmt = 900) 'Unknown keyword for work level selection'
            info = VEF_ERROR
            return
        endif
        !
        ! Contour lines
        this%ptr_theta_range => rangeFromConfig(cnfunit, info)
        if (info /= VEF_OK .or. .not. associated(this%ptr_theta_range)) return
        this%ptr_contourlevel_range => rangeFromConfig(cnfunit, info)
        if (info /= VEF_OK .or. .not. associated(this%ptr_contourlevel_range)) return
        if (.not. readValue(cnfunit, this%n_intervals)) return
        ! Advanced settings
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
            ! read(cnfunit, fmt=*,iostat = ioerr) this%reference_frame
            do i = 1, size(this%base_vectors, dim = 2)
                if (.not. readValue(cnfunit, this%base_vectors(:,i))) return
                if (norm2(this%base_vectors(:,i)) < epsilon(0.D0)) then
                    write(display_unit, fmt = 900) 'Norm of each base vectors must not be zero'
                    return
                endif
                this%base_vectors(:,i) = this%base_vectors(:,i) / norm2(this%base_vectors(:,i))
            enddo
        endif

        ! Override the requests for outputs:
        this%altay%output_config%nfile = 0   ! texture
        this%output%outputRequest = .false.       ! idem.

        info = VEF_OK
        !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end function



    subroutine EWCModule_run(this, info)
        class(EWCModule), intent(inout)            :: this
        integer, intent(out)                       :: info
        !
        integer:: i, j, npoints
        real(DP):: theta, &
                    sigma(3, 3)

        type(IncrementOutputRecord), dimension(:), allocatable:: ref_output, output
        ! Shape or `results` is: [0:n_countours, 1:n_theta]. Zeroth column
        ! shall include the theta angles
        real(DP), dimension(:,:), allocatable, target:: results
        type(IncrementationControlSettings):: evolution_control
        real(DP), dimension(6):: sigma_vector
        real(DP), dimension(:), allocatable:: vEquivalentStrainLevels, &
                                                     vPlasticWorkLevels, &
                                                     vPlasticWork_ref, &
                                                     vEquivalentStrain_ref, &
                                                     vPlasticWork, &
                                                     vScalS
        real(DP), dimension(:), pointer:: vTheta

        type(BarycentricInterpolator):: bi
        integer, parameter:: interpolation_order = 2
        integer:: n_theta, n_contours
        logical:: tmp_flag

        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%StressDrivenEvolutionModule%run(info))
        !
        ! Prepare the input data: array of increments, and
        ! array of results.
        n_theta = this%ptr_theta_range%size()
        n_contours = this%ptr_contourlevel_range%size()
        !
        allocate(results(0:n_contours, n_theta))
        vTheta => results(0, :)
        !
        allocate(vPlasticWorkLevels(n_contours))
        !
        if (this%use_reference_stress_mode) then
            allocate(vEquivalentStrainLevels(n_contours))
            do i = 1, n_contours
                tmp_flag = this%ptr_contourlevel_range%next(vEquivalentStrainLevels(i))
            enddo
            !
            ! Evaluate the reference mode
            sigma = Vec6ToMat33(this%reference_stress_mode)
            info = this%calculateStressPath(sigma, this%control, ref_output)
            if (info /= VEF_OK) return
            !
            ! Calculate work levels that correspond to the requested levels of
            ! equivalent plastic strain.
            vEquivalentStrain_ref = ref_output%vm_strain_total
            vPlasticWork_ref = ref_output%icv%plastic_work_total
            call BarycentricInterpolator_init(bi, interpolation_order, vEquivalentStrain_ref, vPlasticWork_ref, info)
            if (info /= VEF_OK) then
                info = VEF_ERROR
                return
            endif
            do i = 1, n_contours
                vPlasticWorkLevels(i) = interpolate(bi, vEquivalentStrainLevels(i))
            enddo
        else
            ! Direct selection of the work levels
            do i = 1, n_contours
                tmp_flag = this%ptr_contourlevel_range%next(vPlasticWorkLevels(i))
            enddo
        endif
        !
        ! prepare controls for evolution lines
        evolution_control%scaling_type = scalingPlasticWork
        evolution_control%step_size = maxval(vPlasticWorkLevels)
        evolution_control%increment_size = evolution_control%step_size/dble(this%n_intervals*n_contours)
        !
        ! Transform: main loop over the theta angles. Calculate the evolution
        !            of the state. Reset the state at the end of each iteration.
        !            Note: the iterations of the main loop are conceptually independent
        !            of each other. Current implementation of the back-end CP model
        !            prevents exploiting that.
        npoints = this%ptr_theta_range%size()
        i =  0
        do while (this%ptr_theta_range%next(theta))
            i = i+1
            !
            vTheta(i) = theta
            theta = deg2rad(theta)
            !
            ! Calculate S by combining the base vectors
            sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta)
            sigma = Vec6ToMat33(sigma_vector)
            !
            ! Re-initialize AlTay
            info = this%reinitializeLibAltay()
            if (info /= VEF_OK) exit
            !
            if (this%calculateStressPath(sigma, evolution_control, output) /= VEF_OK) then
                ! For a certain reason we cannot calculate this path.
                results(:,i) = 0.D0
                cycle
            endif
            !
            vPlasticWork = output%icv%plastic_work_total
            vScalS = output%scal_s
            call BarycentricInterpolator_init(bi, interpolation_order, vPlasticWork, vScalS, info)
            if (info == VEF_OK) then
                do j = 1, size(vPlasticWorkLevels)
                    results(j, i) = interpolate(bi, vPlasticWorkLevels(j))
                enddo
            else
                ! something is wrong with the input data (size of arrays, content?)
                ! Let's ignore this line.
                results(:,i) = 0.D0
                cycle
            endif

        enddo
        if (info /= VEF_OK) return
        !
        if (this%use_reference_stress_mode) then
            info = this%fileOutput(vEquivalentStrainLevels, results)
            info = this%fileOutputMeta('contours',vEquivalentStrainLevels, vPlasticWorkLevels)
            info = this%fileOutputMeta('reference',vEquivalentStrain_ref, vPlasticWork_ref)
        else
            info = this%fileOutput(vPlasticWorkLevels, results, use_work_levels=.true.)
        endif
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    !
    end subroutine


    !> Post-process the result and generate the output.
    integer function EWCModule_fileOutput(this, vLevels, results, use_work_levels) result(info)
    implicit none
    class(EWCModule), intent(inout)              :: this
    real(DP), dimension(:), intent(in)    :: vLevels
    real(DP), dimension(0:,:), intent(in)  :: results
    logical, optional, intent(in)                 :: use_work_levels
    !
    integer:: iounit, ierr, i, n_contours, n_columns
    integer, parameter:: output_column_width = 25
    character(len = output_column_width), dimension(:), allocatable:: header_columns
    character(len = output_column_width):: tmp_str, label_str
    !
        info = VEF_ERROR
        n_contours = ubound(results, dim = 1)
        if ((size(vLevels) /= n_contours)) return
        !
        info = VEF_ERROR
        open(newunit = iounit, file = trim(this%output%outputPrefix)//'.ewc', status='replace', iostat = ierr)
        if (ierr /= 0) return
        !
        n_columns = 1+n_contours  ! Theta followed by n_contours columns
        !
        ! Make the headers
        CHOOSE(label_str, optionalDefault(use_work_levels, .false.), 'S|W=', 'S|eps_vM=')
        allocate(header_columns(n_columns))
        header_columns(1) = 'theta'
        do i = 1, n_columns-1
            ! beware: G10 is OK as long as 10 < output_column_width
            tmp_str = trim(label_str) // trim(adjustl(tostring(vLevels(i))))
            header_columns(i+1) = tmp_str
        enddo
        !
        info = writeResultFile(iounit, results, header_columns, [output_column_width])

        close(iounit)
    !
    end function


    !> Create meta-data output file.
    integer function EWCModule_fileOutputMeta(this, prefix, vEquivalentStrainLevels, vPlasticWorkLevels) result(info)
    implicit none
    class(EWCModule), intent(inout)              :: this
    character(len=*), intent(in)                 :: prefix
    real(DP), dimension(:), intent(in)    :: vEquivalentStrainLevels, vPlasticWorkLevels
    !
    integer:: iounit, ierr, i
    !
        info = VEF_ERROR
        if (size(vEquivalentStrainLevels) /= size(vPlasticWorkLevels)) return
        info = VEF_ERROR
        open(newunit = iounit, file = trim(this%output%outputPrefix)//'_'//trim(prefix)//'.ewcm', &
             status='replace', iostat = ierr)
        if (ierr /= 0) return
        info = VEF_ERROR
        ! Make format strings for the header and the data
        write(iounit, '(A15, 1X, A15)') 'eps_vM', 'W(eps_vM)'
        do i = 1, size(vPlasticWorkLevels)
            write(iounit, fmt='(E15.7, 1X, E15.7)',iostat = ierr) vEquivalentStrainLevels(i), vPlasticWorkLevels(i)
            if (ierr /= 0) exit
        enddo
        if (ierr == 0) info = VEF_OK
        close(iounit)
    !
    end function

end module
