!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
    use conversions
    use dmcStressDrivenModule
    use file_io
    use commonUtils
    use dmcResultFileOutput
    use logging

    implicit none

    private
    public:: QRSModule

    character(*), parameter:: MOD_NAME = 'dmcQRS'

    type qrsData
          real(DP) :: qvalue = 0.D0
          real(DP) :: rvalue = 0.D0
          real(DP) :: svalue = 0.D0
    end type

    type, extends(StressDrivenModule):: QRSModule
        real(DP):: angular_resolution
    contains
        procedure, pass(this)    :: readConfig => QRSModule_readConfig
        procedure, pass(this)    :: run => QRSModule_run
        procedure, pass(this)    :: fileOutput => QRSModule_fileOutput
    end type


    !> Container for output datapoints of QRS module
    type:: QRSOutputData
        real(DP), dimension(:), allocatable   :: residuals, phis
        type(qrsData), dimension(:), allocatable      :: qrsvalues
    end type

    !> Constructors of QRSOutputData objects
    interface QRSOutputData
        module procedure QRSOutputData_init_size
    end interface

contains


    type(qrsData) pure function calculateQRS(Dt,s) result(qrsvalue)
        real(DP),dimension(3,3),intent(in)      :: Dt
        real(DP),intent(in)                     :: s

         if ( abs(Dt(3,3)) >= epsilon(0.D0) ) then
             qrsvalue%rvalue = Dt(2,2) / Dt(3,3)
             qrsvalue%qvalue = qrsvalue%rvalue / (1.D0 + qrsvalue%rvalue)
             qrsvalue%svalue = s
         else
             qrsvalue = qrsData(0.D0, 0.D0, 0.D0)
         endif
    end function


    integer function QRSModule_readConfig(this, cnfunit) result(info)
        class(QRSModule), intent(inout)              :: this
        integer, intent(in)                        :: cnfunit

        character(*), parameter:: PROC_NAME = 'QRSModule_readconfig'

        logical:: use_default_settings

        use_default_settings = .false.
        info = this%StressDrivenModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        info = VEF_ERROR

        ! Read parameters specific for the QRS module
        if (.not. readValue(cnfunit, this%angular_resolution)) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read angular resolution')

        this%angular_resolution = deg_to_rad(this%angular_resolution)

        ! Override the requests for outputs:
        this%altay%nfile = 0   ! texture
        this%output%outputRequest = .false.       ! idem.

        info = VEF_OK
    end function


    subroutine QRSModule_run(this, info)
        class(QRSModule), intent(inout)      :: this
        integer, intent(out)                 :: info

        ! Convention: strain rate and stress tensors in
        ! - "Tensile sample coordinate system" have suffix _t
        ! - "Material coordinate system" have no suffix.
        real(DP), dimension(3, 3)         ::  Mrot, &
                                            D_t, &
                                            S_t, &
                                            sigma, &
                                            sigma_t, &
                                            sona, &
                                            dresume_t, &
                                            smident
        real(DP)                        :: fi2, residual_resume
        integer     :: i, ofunit, npoints
        logical     :: useVMGuess
        !
        type(QRSOutputData):: results

        real(DP):: target_stress_mode(5), &
                   strain_mode(5), &
                   stress(5), &
                   residual(5)

        info = this%openOutputFile('.xqrs', ofunit)
        if (info /= VEF_OK) return

        !
        npoints = ceiling(2._DP*PI / this%angular_resolution - TOLERANCE)
        results = QRSOutputData(npoints)
        !
        fi2 = 0._DP
        !
        sigma_t = 0._DP
        sigma_t(1, 1) = 1._DP

        do i=1,npoints
            !
            ! use von Mises guess as a default
            useVMGuess = .true.

            ! Calculate rotation matrix
            ! - due to passive rotation convention
            Mrot = euler_to_tensor([0._DP,0._DP, -fi2])

            ! Rotate from "tensile" to material coordinate system
            sigma = rotate_to(sigma_t, Mrot)

            target_stress_mode = tensor_to_deviatoric(sigma)
            target_stress_mode = target_stress_mode / norm2(target_stress_mode)
            call this%findsolution(target_stress_mode, strain_mode, stress, residual)

            print *, target_stress_mode, strain_mode

            SonA = deviatoric_to_tensor(stress)
            SmIdent = deviatoric_to_tensor(stress / norm2(stress))  ! stress mode for found strain mode

            ! Rotate back to the "tensile test" coordinate system
            D_t = rotate_from(deviatoric_to_tensor(strain_mode), Mrot)
            S_t = rotate_from(SonA, Mrot)

            ! Calculate output variables
            associate(r => results)
                r%phis(i) = rad_to_deg(fi2)
                r%qrsvalues(i) = calculateQRS(D_t, norm2(stress))
                r%residuals(i) = norm2(deviatoric_to_unscaled_voigt(residual))
            end associate

            fi2 = fi2 + this%angular_resolution
        enddo

        info = this%fileOutput(ofunit, results, header=.true.)
        close(ofunit)
    end subroutine


    !> Write out results to the output file
    integer function QRSModule_fileOutput(this, iounit, data_record, header) result(info)
    implicit none
    class(QRSModule), intent(in)                 :: this
    integer, intent(in)                          :: iounit !< Output IO unit
    type(QRSOutputData), intent(in), optional     :: data_record !< Data to be written out
    logical, intent(in), optional                 :: header !< Header to be written out
    !
    integer:: i, npoints, left, right, stride, ierr
    !
    integer, parameter:: ncolumn_labels = 5, column_width = 18
    character(len = column_width), dimension(ncolumn_labels):: column_names = &
        [ character(len = column_width) ::  &
        'angle','q-value','r-value','s-value','residual' ]
    !
        info = VEF_ERROR
        if (optionalDefault(header, .false.)) then
            info = writeStandardHeader(iounit, column_names, [column_width])
            if (info /= VEF_OK) return
        endif
        !
        if (present(data_record)) then
            info = VEF_ERROR
            ! FIXME: flawed assumption, other arrays may have different size
            npoints = 0
            if (allocated(data_record%phis)) &
                npoints = size(data_record%phis)
            do i = 1, npoints
                write(iounit, fmt = 710, iostat = ierr) data_record%phis(i), &
                                                  data_record%qrsvalues(i), &
                                                  data_record%residuals(i)
                if (ierr /= 0) return
            enddo
        endif
        !
        info = VEF_OK
        !
        ! Formats for the output file
        710 format(1X, 8(ES18.9E3, 1X))
    end function


    !> Initialize QRSOutputData to store npoints datapoints
    pure function QRSOutputData_init_size(npoints) result(res)
    type(QRSOutputData)     :: res
    integer, intent(in)      :: npoints
    !
        ! Make space for the results
        allocate(res%qrsvalues(npoints))
        ! Other entities are of the same type, but they can not be treated in a single
        ! statement if SOURCE is provided...
        allocate(res%residuals(npoints), source = 0.D0)
        allocate(res%phis(npoints), source = 0.D0)
    !
    end function
end module
