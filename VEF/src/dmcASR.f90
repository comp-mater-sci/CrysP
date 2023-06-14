#include "criMacros.fpp"

!> DMC Arbitrary Stress Response
module dmcASR
use criMathUtils
use criUncomment, only: readValue
use dmcIncrementationControl
use dmcStressDrivenEvolutionModule
use commonConfig
use commonUtils
implicit none

    public :: ASRModule
    private

    type :: StressDrivenStep
        double precision,dimension(sr_symm_voigt_dim)   :: stress_mode = 0.D0
        type(IncrementationControlSettings)             :: incrementation_control
        logical                                         :: update_state = .false.
    end type


    type,extends(StressDrivenEvolutionModule) :: ASRModule
        type(EulerAngles)                         :: rotframe

        type(StressDrivenStep),dimension(:),allocatable :: steps

    contains

        !>@{ \name Interface methods of AbstractModule

        procedure,pass(this)    :: readConfig => ASRModule_readConfig

        procedure,pass(this)    :: printConfig => ASRModule_printConfig

        procedure,pass(this)    :: run => ASRModule_run

        !>@}

        procedure,pass(this)    :: outputFile => ASRModule_outputFile

    end type


    type :: ASROutput
        integer                 :: step = 0
        type(EvolutionOutput)   :: evolution_output
        double precision,dimension(rot_matrix_dim,rot_matrix_dim)   :: rotation_matrix = unit_sr_Matrix
    end type

contains

    integer function ASRModule_readConfig(this,cnfunit) result(info)
    implicit none
    class(ASRModule),intent(inout)            :: this
    integer,intent(in)                        :: cnfunit
    !
    double precision,dimension(3) :: tmp_euler
    integer :: i, n_steps
    !
        info = this%StressDrivenEvolutionModule%readConfig(cnfunit)
        if (info /= VEF_OK) return
        info = VEF_ERROR
        ! Read parameters specific for the ASRModule
        if (.not. readValue(cnfunit, tmp_euler)) return
        this%rotframe = Arr2EulerAngles(tmp_euler)
        if (.not. readValue(cnfunit, n_steps)) return
        if (n_steps <= 0) then
            write(display_unit, fmt=902) 'ASR module'
            return
        endif
        allocate(this%steps(n_steps))
        do i = 1, n_steps
            associate (step => this%steps(i))
                if (.not. readValue(cnfunit, step%stress_mode)) return
                if (.not. readValue(cnfunit, step%update_state)) return
                if (step%update_state) then
                    call IncrementationControlSettings_read(step%incrementation_control, cnfunit, info, &
                                                            allowed=[scalingStrainTensor, &
                                                                     scalingStrainTensorIncrement, &
                                                                     scalingPlasticWork])
                    if (info /= VEF_OK) return
                endif
                end associate
        enddo
        info = VEF_OK

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end function


    integer function ASRModule_printConfig(this,outunit) result (info)
    implicit none
    class(ASRModule),intent(in)         :: this
    integer,intent(in)                  :: outunit

        info = VEF_OK

    end function


    subroutine ASRModule_run(this,info)
    implicit none
    class(ASRModule),intent(inout)          :: this
    integer,intent(out)                     :: info
    !
    ! Quantities in the global (aka. material = texture) reference frame
    type(SRTensor)                  :: sigma, S,  Pressure  !< total stress, deviatoric stress, hydrostatic stress
    ! Quantities in rotated (aka. sample) reference frame
!    type(SRTensor)                  :: sigma_rot
    type(ASROutput)                 :: output
    type(IncrementationControl)     :: icv
    double precision,dimension(rot_matrix_dim,rot_matrix_dim)   :: Mrot
    !
    integer     :: j, istep, nsteps, ofunit
    !
        ! Super-class first
        RETURN_IF(info /= VEF_OK, call this%StressDrivenEvolutionModule%run(info))
        !
        ! Open and initialize result files
        !
        RETURN_IF(info /= VEF_OK, info = this%openOutputFile('.asr',ofunit))
        RETURN_IF(info /= VEF_OK, info = this%outputFile(ofunit, header=.true.))
        !
        nsteps = size(this%steps)
        !
        ! Calculate rotation matrix (active rotation from material (=texture) to sample frame)
        Mrot = rotmat(deg2rad(this%rotframe))
        !
        do  istep = 1, nsteps
                        !
            associate(step => this%steps(istep), control => this%steps(istep)%incrementation_control)
                !
                ! Acquire full stress tensor sigma
                sigma%t = Vec6ToMat33(step%stress_mode)
                Pressure%t = (trace(sigma) / 3.D0) * unit_sr_tensor%t
                S%t = sigma%t - Pressure%t
                !
                ! Rotate from the original reference frame to the sample reference frame
!                sigma_rot = rotateSRTensorTo(sigma, Mrot)
                !
                ! Print the input data:
                                !
                ! Follow the stress path
                !
                info = this%calculateStressPath(sigma, control, output%evolution_output, Mrot, &
                                                incrementation_control=icv)
                if (info /= VEF_OK) then
                    write(display_unit,fmt=960)
                    exit
                endif
                !
                ! Collect the outputs
                !
                output%step = istep
                output%rotation_matrix = Mrot
                !
                ! Post-process & report
                !
                info = this%outputFile(ofunit, output)
                if (info /= VEF_OK) then
                    write(display_unit,fmt=900) 'Cannot make output for the current step'
                    exit
                endif
            end associate
        enddo

        ! Formats
        3310 format('Input stress tensor, in the material reference frame:')
        3400 format(T15,A,T54,A,T85,A)
        ! 410 format('| SmScaled',T40,'| SmIdent',T80,'|SonA')
        3411 format(3(E10.3,1X),' | ',3(E10.3,1X),' | ',3(E10.3,1X))

        1600 format(/,'Step ',I0, ' out of ',I0)
#define MSG_GROUP_RULERS
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

    end subroutine


    !> Output the results
    !>
    !> The procedure writes either header, data or both.
    integer function ASRModule_outputFile(this, iounit, output, header) result(info)
    implicit none
    class(ASRModule),intent(in)         :: this
    integer,intent(in)                  :: iounit   !< I/O output unit
    type(ASROutput),intent(in),optional :: output   !< Data to be written out
    logical,intent(in),optional         :: header   !< Request for header to be written out
    !
    integer :: i, ierr, increment
    type(SRTensor)      :: SonA, A, P_step, P_step_rot, P_total_rot, P_total_end, P_total_end_rot
    double precision,dimension(sr_symm_voigt_dim) :: SonA_voigt, SonA_rot_voigt, &
                                                     A_voigt, A_rot_voigt, &
                                                     P_step_voigt, P_step_rot_voigt, &
                                                     P_total_end_voigt, P_total_end_rot_voigt
    integer,parameter :: ncolumn_labels = 2 + 10 + 4*2*6, column_width = 15, short_column_width = 9
    character(len=column_width),dimension(ncolumn_labels),parameter :: column_labels = &
            [ character(len=column_width) ::  &
                'step','increment', & ! 2 fields
                'eps_vM', 'eps_norm','Pnorm','eps_total_vM','W','dotW','M-factor','scal_s','S','residual', & ! 10 fields
                'S_11','S_22','S_33','S_12','S_23','S_13', & ! 6 fields  (I)
                'S_xx','S_yy','S_zz','S_xy','S_yz','S_xz', & ! 6 fields
                'A_11','A_22','A_33','A_12','A_23','A_13', & ! 6 fields  (II)
                'A_xx','A_yy','A_zz','A_xy','A_yz','A_xz', & ! 6 fields
                'eps_11','eps_22','eps_33','eps_12','eps_23','eps_13', & ! 6 fields  (III)
                'eps_xx','eps_yy','eps_zz','eps_xy','eps_yz','eps_xz', & ! 6 fields
                'eps_tot_11','eps_tot_22','eps_tot_33','eps_tot_12','eps_tot_23','eps_tot_13', & ! 6 fields (IV)
                'eps_tot_xx','eps_tot_yy','eps_tot_zz','eps_tot_xy','eps_tot_yz','eps_tot_xz'& ! 6 fields
            ]
        !
        info = VEF_OK
        !
        ! Write out header lines
        !
        if (optionalDefault(header, .false.)) then
            info = VEF_ERROR
            ! Column numbers
            write(iounit,701,iostat=ierr) (toString(i), i = 1,2), &
                                          (toString(i), i = 3, ncolumn_labels)
            if (ierr /= 0) return
            ! Column labels
            write(iounit,700,iostat=ierr) (column_labels(i)(1:short_column_width), i=1,2), &
                                          (column_labels(i), i=3,ncolumn_labels)
            if (ierr /= 0) return
            info = VEF_OK
        endif
        !
        ! Write out data output
        if (present(output)) then
            ierr = 0
            info = VEF_ERROR
            !
            do increment = 1, size(output%evolution_output%values)
                associate(v => output%evolution_output%values(increment), &
                          Mrot => output%rotation_matrix)
                    !
                    ! Step deviatoric strain
                    P_step_rot%t = vec5D2tens(v%icv%vP_step)
                    ! Total deviatoric strain
                    P_total_rot%t = vec5D2tens(v%icv%vP_total) ! at the beginning of the increment
                    P_total_end_rot%t = P_total_rot%t + v%P_inc_evol%t ! at the end of the increment
                    !
                    ! Rotate back to the original coordinate system
                    !
                    A = rotateSRTensorFrom(v%A, Mrot)
                    A_voigt = Mat33ToVec6(A%t)
                    !
                    SonA = rotateSRTensorFrom(v%SonA, Mrot)
                    SonA_voigt = Mat33ToVec6(SonA%t)
                    !
                    P_step = rotateSRTensorFrom(P_step_rot, Mrot)
                    P_step_voigt = Mat33ToVec6(P_step%t)
                    !
                    P_total_end = rotateSRTensorFrom(P_total_end_rot, Mrot)
                    P_total_end_voigt = Mat33ToVec6(P_total_end%t)
                    !
                    ! Convert to Voigt (to avoid temporaries in write)
                    A_rot_voigt = Mat33ToVec6(v%A%t)
                    SonA_rot_voigt = Mat33ToVec6(v%SonA%t)
                    P_step_rot_voigt =  Mat33ToVec6(P_step_rot%t)
                    P_total_end_rot_voigt = Mat33ToVec6(P_total_end%t)

                    write(iounit,fmt=710,iostat=ierr) &
                                output%step, v%icv%increment, & ! 2 fields
                                v%vm_strain, norm2(v%icv%vP_step), v%norm_P_abs, v%vm_strain_total, &
                                v%icv%plastic_work_total, v%dotWonA, &
                                v%taylor_factor, v%scal_s, v%norm_SonA, v%R, & ! 9 fields
                                SonA_voigt, SonA_rot_voigt, &
                                A_voigt, A_rot_voigt, &
                                P_step_voigt, P_step_rot_voigt, &
                                P_total_end_voigt, P_total_end_rot_voigt
                !
                end associate
            enddo
            if (ierr == 0) info = VEF_OK
        endif
        ! Formats for output file
        700 format(1X, 2(A9,1X),10(A18,  1X),4(5X,12(A18,1X)))
        701 format('#',2(A9,1X),10(A18,  1X),4(5X,12(A18,1X)))
        710 format(1X, 2(I9,1X),10(ES18.9E3,1X),4(5X,12(ES18.9E3,1X)))
    !
    end function

end module
