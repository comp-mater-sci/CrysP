#include "criMacros.fpp"

!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
use criMathUtils
use criRange
use criUncomment, only: readValue
use dmcYLPResult
use dmcStressDrivenModule
use commonConfig
use commonUtils
use dmcResultFileOutput
use qrsTypes
implicit none

    public :: QRSModule
    private


    type,extends(StressDrivenModule) :: QRSModule
        class(range_type),pointer                 :: ptr_range

        double precision                          :: rho = 0.D0

        logical                                   :: calculate_Mfactor = .false.

        logical                                   :: use_stability_improvements = .false.

        logical                                   :: fold_symmetry = .false.

    contains

        !>@{ \name Interface methods of AbstractModule

        procedure,pass(this)    :: readConfig => QRSModule_readConfig

        procedure,pass(this)    :: printConfig => QRSModule_printConfig

        procedure,pass(this)    :: run => QRSModule_run

        !>@}

        procedure,pass(this)    :: fileOutput => QRSModule_fileOutput

    end type


    !> Container for output datapoints of QRS module
    type :: QRSOutputData
        double precision,dimension(:),allocatable   :: residuals,mfactors,phis,sigmas_x
        type(qrsData),dimension(:),allocatable      :: qrsvalues
    end type


    !> Constructors of QRSOutputData objects
    interface QRSOutputData
        module procedure QRSOutputData_init_size
    end interface

contains


    integer function QRSModule_readConfig(this,cnfunit) result(info)
    implicit none
    class(QRSModule),intent(inout)              :: this
    integer,intent(in)                        :: cnfunit
    !
    logical :: use_default_settings
    !
        use_default_settings = .false.
        info = this%StressDrivenModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        info = VEF_ERROR
        ! Read parameters specific for the QRSModule module
        this%ptr_range => rangeFromConfig(cnfunit,info)
        if ( (info /= VEF_OK) .or. (.not. associated(this%ptr_range)) ) return
        !
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
                info = VEF_ERROR
                if (.not. readValue(cnfunit, this%rho)) return
                if (.not. readValue(cnfunit, this%calculate_MFactor)) return
                if (.not. readValue(cnfunit, this%fold_symmetry)) return
                if (.not. readValue(cnfunit, this%use_stability_improvements)) return
        endif
        !
        ! Override the requests for outputs:
        this%altay%output_config%nfile = 0   ! texture
        this%altay%output_config%npebp = 0   ! KOST1x state
        this%output%outputRequest = .false.       ! idem.
        !
        info = VEF_OK
    !
    end function


    integer function QRSModule_printConfig(this,outunit) result (info)
    implicit none
    class(QRSModule),intent(in)         :: this
    integer,intent(in)                  :: outunit
    !
    integer :: ioerr
    !
        info = VEF_OK
    !
    end function



    subroutine QRSModule_run(this,info)
    implicit none
    class(QRSModule),intent(inout)      :: this
    integer,intent(out)                 :: info

    ! Convention: strain rate and stress tensors in
    ! - "Tensile sample coordinate system" have suffix _t
    ! - "Material coordinate system" have no suffix.
    !
    type(SRTensor)                            :: D_t, S_t, sigma, sigma_t, SonA, D, Dresume_t, &
                                                 SmIdent    !< obtained stress mode
    double precision,dimension(3,3)           :: Mrot = 0.0
    type(YLPResult)                           :: ylp_result
    !
    double precision                          :: fi1,phi,fi2, residual_resume
    integer     :: i,j, k, npoints, npoints_ok, ofunit
    logical     :: useVMGuess, acceptable_point
    !
    type(QRSOutputData) :: results
    !
    integer,parameter :: column_width = 15
    ! For display output:
    integer,parameter :: ncolumn_labels_display = 6, column_width_display = 14
    character(len=column_width-1),dimension(ncolumn_labels_display) :: display_column_labels = &
        [ character(len=column_width_display) ::  &
        'angle','rho','q-value','r-value','sigma_xx','residual' ]
    !
        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%StressDrivenModule%run(info))
        !
        info = VEF_ERROR
        !
        npoints = this%ptr_range%size()
        !
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.xqrs', ofunit))
        !
        ! Apply correction to the configuration of the search procedure:
        ! there will be no need to use the full model in the last call unless
        ! the average Taylor factor is requested.
        this%ylp%evaluate_full_model  = this%calculate_MFactor
        !
        results = QRSOutputData(npoints)
        !
        fi1 = 0.D0
        phi = 0.D0
        !
        ! Set sigma_t in such way that deviatoric part is of unit length
        sigma_t%t = 0.D0
        sigma_t%t(1,1) = root32/sqrt(this%rho**2-this%rho+1.D0)
        sigma_t%t(2,2) = this%rho*sigma_t%t(1,1)
        !
        i = 1
        do while (this%ptr_range%next(fi2))
            !
            ! use von Mises guess as a default
            useVMGuess = .true.
            !
            fi2 = deg2rad(fi2)
            ! Calculate rotation matrix
            Mrot = rotmat(fi1,phi,fi2)

            ! Rotate from "tensile" to material coordinate system
            sigma = rotateSRTensorTo(sigma_t, Mrot)
            !
            if ((this%use_stability_improvements) .AND. (i > 1)) then
                ! Reuse previously stored result in new coordinate system
                ! if it represents a converged solution.
                if (residual_resume <= this%ylp%obj_func_eps) then
                    ! Rotate Dresume_t to new coordinate system
                    D = rotateSRTensorTo(Dresume_t, Mrot)
                    ! Disable Von Mises guess
                    useVMGuess = .false.
                endif
            endif
            !
            info = this%findSolution(sigma, D, ylp_result, useVMGuess, is_acceptable=acceptable_point)
            if ((info /= VEF_OK) .and. .not. acceptable_point) then
                write(display_unit,fmt=860) 'Cannot find solution, datapoint dropped'
                cycle
            endif

            if (info == VEF_ERROR) exit
            !
            SonA%t = vec5D2tens(ylp_result%vSonA)
            SmIdent%t = vec5D2tens(ylp_result%vSonAn) ! stress mode for found strain mode

            ! Rotate back to the "tensile test" coordinate system
            D_t = rotateSRTensorFrom(D, Mrot)
            S_t = rotateSRTensorFrom(SonA, Mrot)
            !
            !(***) Prepare next iteration if re-using is requested.
            if (this%use_stability_improvements) then
                Dresume_t = D_t
                residual_resume = ylp_result%R
            endif
            !
            ! Calculate output variables
            !
            associate(r => results)
                !
                r%phis(i) = rad2deg(fi2)
                r%qrsvalues(i) = calculateQRS(D_t%t,ylp_result%scal_s)
                r%sigmas_x(i) = S_t%t(1,1) - S_t%t(3,3)
                r%residuals(i) = ylp_result%R
                ! Optional: Taylor factor can be retrieved
                if (this%calculate_MFactor) then
                    call getTaylorFactor(1, r%mfactors(i), info)
                    if (info /= 0) then
                        write(display_unit,980)
                        exit
                    endif
                endif
            end associate
            !
            i = i + 1
            !
            info = VEF_OK
        enddo
        !
        if (info == VEF_ERROR) return

        npoints_ok = i-1
        if (npoints /= npoints_ok) then
            write(display_unit,fmt=850) 'There were unconverged solutions, so some of datapoints are dropped'
            ! FIXME: temporary solution: folding cannot be done if there are missing points.
            if (this%fold_symmetry) then
                write(display_unit,fmt=850) 'Folding is turned off.'
                this%fold_symmetry = .false.
            endif
        endif
        !
        info = this%fileOutput(ofunit, results, header=.true., restrict=npoints_ok)
        close(ofunit)
        !
        !
        3400 format('| sigma',T40,'| SmIdent',T80,'|Dmcoord')
        3401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
        ! Formats for the display output
        2600 format(1X, 6(A14,    1X))
        2601 format('|',6(14('-'),'|'))
        2610 format(1X, F14.2, 1X, 3(F14.6,1X),2(E14.6,1X)) ! 6 fields in total
        1600 format(/,'Sample ', I0, ' out of ',I0, ', sample orientation: ',F0.2)
        !
#define MSG_GROUP_RULERS
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end subroutine


    !> Write out results to the output file
    integer function QRSModule_fileOutput(this, iounit, data_record, header, restrict) result(info)
    implicit none
    class(QRSModule),intent(in)                 :: this
    integer,intent(in)                          :: iounit !< Output IO unit
    type(QRSOutputData),intent(in),optional     :: data_record !< Data to be written out
    logical,intent(in),optional                 :: header !< Header to be written out
    integer,intent(in),optional                 :: restrict
    !
    integer :: i, npoints, left, right, stride, ierr
    !
    integer,parameter :: ncolumn_labels = 8, column_width = 18
    character(len=column_width),dimension(ncolumn_labels) :: column_names = &
        [ character(len=column_width) ::  &
        'angle','rho','q-value','r-value','s-value','sigma_xx','M-factor','residual' ]
    !
        info = VEF_ERROR
        if (optionalDefault(header,.false.)) then
            info = writeStandardHeader(iounit, column_names, [column_width])
            if (info /= VEF_OK) return
        endif
        !
        if (present(data_record)) then
            info = VEF_ERROR
            ! FIXME: flawed assumption, other arrays may have different size
            ALLOCATED_SIZE(npoints, data_record%phis)
            if (present(restrict)) then
                RETURN_IF_WITH(npoints < restrict, info=VEF_ERROR)
                npoints = restrict
            endif
            ! Write output file
            if (this%fold_symmetry) then
                ! Average over symmetric positions
                left = 1
                right = npoints
                do
                    if (left > right) exit
                    stride = right - left
                    if (stride == 0) stride = 1
                    write(iounit,fmt=710,iostat=ierr) data_record%phis(left), &
                                                      this%rho,                 &
                                                      avgQRS(data_record%qrsvalues(left:right:stride)), &
                                                      average(data_record%sigmas_x(left:right:stride)), &
                                                      average(data_record%mfactors(left:right:stride)), &
                                                      average(data_record%residuals(left:right:stride))
                    if (ierr /= 0) return
                    left = left + 1
                    right = right -1
                enddo
            else
                ! Output complete set of points
                do i=1,npoints
                    write(iounit,fmt=710,iostat=ierr) data_record%phis(i), &
                                                      this%rho, &
                                                      data_record%qrsvalues(i), &
                                                      data_record%sigmas_x(i), &
                                                      data_record%mfactors(i), &
                                                      data_record%residuals(i)
                    if (ierr /= 0) return
                enddo
            endif
        endif
        !
        info = VEF_OK
        !
        ! Formats for the output file
        710 format(1X, 8(ES18.9E3,1X))
    end function


    !> Initialize QRSOutputData to store npoints datapoints
    pure function QRSOutputData_init_size(npoints) result(res)
    type(QRSOutputData)     :: res
    integer,intent(in)      :: npoints
    !
        ! Make space for the results
        allocate(res%qrsvalues(npoints))
        ! Other entities are of the same type, but they can not be treated in a single
        ! statement if SOURCE is provided...
        allocate(res%residuals(npoints), source=0.D0)
        allocate(res%sigmas_x(npoints), source=0.D0)
        allocate(res%mfactors(npoints), source=0.D0)
        allocate(res%phis(npoints), source=0.D0)
    !
    end function

end module
