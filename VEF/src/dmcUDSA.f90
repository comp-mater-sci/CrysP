#include "criMacros.fpp"

!> dmcUDSA (Uniaxially-Dominated Stress Analysis)  allows one to track anisotropic properties
!> along deformation due to the uniaxial tension or compression stress.
module dmcUDSA
use criMathUtils
use criRange
use criNamedRange
use criConfigReader
use dmcStressDrivenEvolutionModule
use dmcIncrementationControl
use commonUtils
use qrsTypes
use commonConfig
use definitions

implicit none

    public :: UDSAModule
    private

    integer,parameter,private :: tension_state = 0, compression_state = 1
    type(MapItem),dimension(2),parameter :: stress_states = [MapItem('compression', compression_state),&
                                                            MapItem('tension', tension_state)]

    integer,parameter,private :: sample_orientation_inplane_id = 1, &
                                sample_orientation_ND_id = 2, &
                                sample_orientation_arbitrary_id = 3

    integer,parameter,private :: n_orientation_types = 3
    type(MapItem),dimension(n_orientation_types),parameter,private :: sample_orientation_types = [ &
            MapItem('inplane', sample_orientation_inplane_id), &
            MapItem('ND', sample_orientation_ND_id), &
            MapItem('arbitrary', sample_orientation_arbitrary_id) ]



    type,extends(StressDrivenEvolutionModule) :: UDSAModule

        integer           :: orientation_type_id = sample_orientation_inplane_id

        class(range_type),pointer   :: ptr_orientation_range => null()

        type(EulerAngles) :: sample_orientation

        integer           :: stress_state_id = tension_state

        !> Stress ratio
        double precision  :: rho = 0.D0

    contains

        !>@{ \name Interface methods of AbstractModule

        procedure,pass(this)    :: readConfig => UDSAModule_readConfig

        procedure,pass(this)    :: run => UDSAModule_run

        !>@}

        procedure,private,pass(this)    :: createOutputFile => UDSAModule_createOutputFile

        procedure,private,pass(this)    :: outputFile => UDSAModule_outputFile

        procedure,pass(this)    :: outputPrefix => UDSAModule_outputPrefix
    end type


    type :: UDSAOutputRecord
        integer             :: increment
        double precision    :: vm_strain = 0.D0
        double precision    :: norm_P_abs = 0.D0
        double precision    :: TNorm = 0.D0  ! Tensile strain
        double precision    :: TSigma = 0.D0 ! Tensile total stress
        double precision    :: TSNorm = 0.D0 ! Tensile deviatoric stress
        double precision    :: plastic_work_total = 0.D0
        double precision    :: dotWonA = 0.D0
        double precision    :: taylor_factor = 0.D0
        type(qrsData)       :: instantaneous_qrsvalue
        type(qrsData)       :: cummulative_qrsvalue
        double precision    :: residual = 0.D0
    end type


contains

    integer function UDSAModule_readConfig(this,cnfunit) result(info)
    implicit none
    integer,intent(in)                        :: cnfunit
    class(UDSAModule),intent(inout)            :: this
    !
    logical :: use_default_settings
    double precision,dimension(3) :: arr_euler
    !
        info = this%StressDrivenEvolutionModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        info = VEF_ERROR
        ! Read parameters specific for the UDSAModule program
        if (.not. readKeyword(cnfunit, sample_orientation_types, this%orientation_type_id)) return
        ! Read the sub-options
        select case(this%orientation_type_id)
        case(sample_orientation_inplane_id)
            this%ptr_orientation_range => rangeFromConfig(cnfunit, info)
            if (info /= VEF_OK .or. .not. associated(this%ptr_orientation_range)) return
        !
        case(sample_orientation_ND_id)
            ! No sub-options
            this%ptr_orientation_range => rangeFactory_extended('zero') ! One-element range
        !
        case(sample_orientation_arbitrary_id)
            this%ptr_orientation_range => rangeFactory_extended('zero') ! One-element range
            ! Read Euler angles
            if (.not. readValue(cnfunit, arr_euler)) return
            this%sample_orientation = Arr2EulerAngles(arr_euler)
        end select
        ! Read incrementation settings
        call IncrementationControlSettings_read(this%control, cnfunit, info)
        if (info /= VEF_OK) return
        !
        if (.not. readKeyword(cnfunit, stress_states, this%stress_state_id)) return
        use_default_settings = .true.
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
            if (.not. readValue(cnfunit, this%rho)) return
        endif
        info = VEF_OK
    !
    end function


    subroutine UDSAModule_run(this,info)
    implicit none
    class(UDSAModule),intent(inout)            :: this
    integer,intent(out)                        :: info
    
    ! Note about naming convention for variables:
    !    - All variables for vectors and tensors suffixed with _t are expressed
    !      in the "tensile sample coordinate system".
    !    - All other variables are implicitly expressed in the "material coordinate system"
    real(DP), dimension(3,3) :: Mrot = 0.0_DP, &
                                sigma, &
                                sigma_t, &
                                s_t, &
                                d_t, &
                                p_t, &
                                p_t_end
    type(EvolutionOutput) :: output
    type(EulerAngles) :: sample_orientation
    real(DP)    :: angle, stress_direction
    integer :: test_run, n_test_runs, increment, ofunit
    type(UDSAOutputRecord)  :: outrec
    !
    ! Super-class first
    RETURN_IF(info /= VEF_OK, call this%StressDrivenEvolutionModule%run(info))
    !
    ! Check the preconditions
    !
    info = VEF_ERROR
    if (.not. associated(this%ptr_orientation_range)) return

    !
    ! Prepare the input data
    ! Take uniaxial/{slightly biaxial} tensile stress, to be rotated to the given sample
    ! orientation.
    !
    !> Uniaxial stress state. Negative value denotes compressive state;
    !> non-negative values are used for tensile state.
    sigma_t = 0.D0
    stress_direction = merge(-1.D0,1.D0,(this%stress_state_id == compression_state))
    sigma_t(1,1) = stress_direction * sqrt(3.D0/2.D0)/sqrt(this%rho**2-this%rho+1)
    sigma_t(2,2) = this%rho*sigma_t(1,1)
    !
    ! Loop over test set
    !
    test_run = 0

    n_test_runs = this%ptr_orientation_range%size()

    test_run_loop: do while (this%ptr_orientation_range%next(angle))
        test_run = test_run + 1
        !
        ! Come back to the initial material state if needed
        ! Re-initialize altay
        if (n_test_runs > 1) then
            ! Re-initialize AlTay
            info = this%reinitializeLibAltay(this%outputPrefix(angle))
            if (info /= VEF_OK) exit
        endif
        !
        ! Set sample orientation and make rotation matrix
        !
        select case(this%orientation_type_id)
        case(sample_orientation_inplane_id)
            sample_orientation = EulerAngles(0.D0, 0.D0, angle)
        case(sample_orientation_ND_id)
            sample_orientation = EulerAngles(90.D0, 90.D0, 90.D0)
        case(sample_orientation_arbitrary_id)
            sample_orientation = this%sample_orientation
        end select
        sample_orientation = deg2rad(sample_orientation)
        !
        ! Rotate stress from "tensile" to material coordinate system
        ! Calculate rotation matrix
        Mrot = rotmat(sample_orientation)
        sigma = rotateSRTensorTo(sigma_t, Mrot)
        !
        ! Open and initialize result files
        !
        if (n_test_runs > 1) then
            info = this%createOutputFile(ofunit, angle)
        else
            info = this%createOutputFile(ofunit)
        endif
        if (info /= VEF_OK) then
            write(display_unit, fmt=900) 'Cannot create result file for the current virtual test'
            exit
        endif

        info = this%calculateStressPath(sigma, this%control, output, Mrot)
        if (info /= VEF_OK) then
            write(display_unit,fmt=960)
            exit
        endif
        !
        ! Process the output evolution path and produce result file
        !
        do increment = 1, size(output%values)
            ! Total plastic strain at the _begining_ of the inrement.

            associate(v => output%values(increment))

                ! Rotate back to the "tensile test" coordinate system
                D_t = rotateSRTensorFrom(v%A, Mrot)
                S_t = rotateSRTensorFrom(v%SonA, Mrot)

                ! Total deviatoric strain (Note: the total, not per-step)
                P_t = vec5D2tens(v%icv%vP_total) ! at the beginning of the increment
                P_t_end = P_t + v%P_inc_evol ! at the end of the increment
                P_t = rotateSRTensorFrom(P_t, Mrot)
                P_t_end = rotateSRTensorFrom(P_t_end, Mrot)
                !
                ! Calculate output variables
                !
                ! Calculate q and r in tensile reference frame
                !
                ! Write out the result
                outrec = UDSAOutputRecord(increment = increment, &
                                          vm_strain = v%vm_strain, &
                                          norm_P_abs = v%norm_P_abs, &
                                          TNorm = abs(P_t(1,1)), & ! Tensile strain
                                          TSigma = S_t(1,1) - S_t(3,3), & ! Tensile total stress
                                          TSNorm = abs(S_t(1,1)), & ! Tensile deviatoric stress
                                          plastic_work_total = v%icv%plastic_work_total, &
                                          dotWonA = v%dotWonA, &
                                          taylor_factor = v%taylor_factor, &
                                          instantaneous_qrsvalue = calculateQRS(D_t,v%scal_s), &
                                          cummulative_qrsvalue = calculateQRS(P_t_end, v%norm_SonA), &
                                          residual =  v%R)
                !
                info = this%outputFile(iounit=ofunit, data_record=outrec)
            !
            end associate
        enddo
        !
        close(ofunit)
        if (info /= VEF_OK) then
            write(display_unit, fmt=900) 'Unable to store results for the current virtual tests'
        endif
    !
    end do test_run_loop
    !
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS

    end subroutine


    integer function UDSAModule_createOutputFile(this, iounit, tag_number) result(info)
    implicit none
    class(UDSAModule),intent(in)              :: this
    integer,intent(out)                       :: iounit
    double precision,intent(in),optional      :: tag_number
    !
    integer :: ierr
    character(len=max_pathlen) :: datafile_path
    !
        iounit = 0
        info = VEF_ERROR
        datafile_path = this%outputPrefix(tag_number)
        open(newunit=iounit,file=trim(datafile_path)//'.uds', status='replace', iostat=ierr)
        if (ierr /= 0) return
        info = this%outputFile(iounit, header=.true.)
        !
    end function


    integer function UDSAModule_outputFile(this, iounit, data_record, header) result(info)
    implicit none
    class(UDSAModule),intent(in)                :: this
    integer,intent(in)                          :: iounit
    type(UDSAOutputRecord),intent(in),optional  :: data_record
    logical,intent(in),optional                 :: header
    !
    integer :: i, ierr
    !
    integer,parameter :: ncolumn_labels = 16, column_width = 15, short_column_width = 9
    character(len=column_width),dimension(ncolumn_labels) :: file_column_labels = [character(len=column_width) :: &
        'increment','eps_vM','Pnorm','eps_xx', 'sigma_xx', 'S_xx','W','dotW',&
        'M-factor', &
        'q-value','r-value','s-value', &    ! qrsdata
        'q-valueA', 'r-valueA','S',& ! qrsdata
        'residual']
    !
        info = VEF_ERROR
        if (optionalDefault(header,.false.)) then
            write(iounit,701,iostat=ierr) toString(1), &
                                          (toString(i), i = 2, ncolumn_labels)
            if (ierr /= 0) return
            write(iounit,700,iostat=ierr) file_column_labels(1)(1:short_column_width), &
                                          (file_column_labels(i), i=2,ncolumn_labels)
            if (ierr /= 0) return
            info = VEF_OK
        endif
        !
        if (present(data_record)) then
            write(iounit,710,iostat=ierr) data_record
            if (ierr == 0) info = VEF_OK
        endif
        !
        ! Formats for the output file
        700 format(1X, 1(A9,1X),15(A18,  1X))
        701 format('#',1(A9,1X),15(A18,  1X))
        710 format(1X, 1(I9,1X),15(ES18.9E3,1X))
    !
    end function


    function UDSAModule_outputPrefix(this, tag_number) result(path)
    implicit none
    character(len=max_pathlen)                :: path
    class(UDSAModule),intent(in)              :: this
    double precision,intent(in),optional      :: tag_number
    !
    character(len=max_pathlen) :: datafile_tag
    integer :: i
    !
        if (present(tag_number)) then
            ! Make a decoration string based on angle.
            ! Substitute '.' with '_'
            write(datafile_tag, '(F10.3)') tag_number
            datafile_tag = '_' // trim(adjustl(datafile_tag))
            do i=1,len(datafile_tag)
                if (datafile_tag(i:i) == '.') datafile_tag(i:i) = '_'
            end do
        else
            datafile_tag = ''
        endif
        path = trim(this%output%outputPrefix)//datafile_tag
    !
    end function

end module
