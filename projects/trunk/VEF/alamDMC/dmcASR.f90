!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-07-18 (under the name alamASR)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> DMC Arbitrary Stress Response
!>
module dmcASR
use criMathUtils
use criAlgorithm
use criLog
use criMathUtils
use criUncomment, only: readValue
use fngVec5D
use dmcUtils, only: display_unit
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
      
        procedure,pass(this)    :: readConfig => ASRModule_ReadConfig
            
        procedure,pass(this)    :: run => ASRModule_run
        
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
        if (info /= criSuccess) return
        info = criErr_IORead
        ! Read parameters specific for the ASRModule
        if (.not. readValue(cnfunit, tmp_euler)) return
        this%rotframe = Arr2EulerAngles(tmp_euler)
        if (.not. readValue(cnfunit, n_steps)) return
        if (n_steps <= 0) then
                write(display_unit, fmt=902) 'ASRModule'
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
                    if (info /= criSuccess) return
                endif
                end associate
        enddo
        info = criSuccess
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
    end function


    subroutine ASRModule_Run(this,info)
    implicit none
    class(ASRModule),intent(inout)          :: this
    integer,intent(out)                     :: info
    !
    ! Quantities in the global (aka. material) reference frame
    type(SRTensor)                  :: sigma, S,  Pressure
    ! Quantities in rotated (aka. sample) reference frame
    type(SRTensor)                  :: sigma_rot
    type(ASROutput)                 :: output
    type(IncrementationControl)     :: icv
    double precision,dimension(rot_matrix_dim,rot_matrix_dim)   :: Mrot
    !
    integer     :: j,istep, nsteps
    !
    integer :: ofunit, ierr
    !integer,dimension(2),parameter :: teeunits = [display_unit,histunit]
      
!    character(len=14),dimension(9) :: display_column_labels = [ character(len=14) ::  &
!        'step','incr','eps_vM','Pnorm','totalP_vM','W','scal_s','||S(A)||','residual' ]
        !
        info = criErr_BadArgs
        !
        ! Introduce youself ;-)
        write(display_unit,'(A)') 'ASR, $Rev$'
        !
        ! Open and initialize result files
        !
        open(newunit=ofunit, file=trim(this%output%outputPrefix)//'.asr',status='replace',iostat=ierr)
        if (ierr /= 0) then
            write(display_unit, fmt=952) trim(this%output%outputPrefix)//'.asr'
            info = criErr_IOWrite
            return
        endif
        info = this%outputFile(ofunit, header=.true.)
        if (info /= criSuccess) return
        !
        ! open(unit=histunit,file=trim(this%output%outputPrefix)//'.hsr',status='replace')
        !      
        nsteps = size(this%steps)
        !
        ! Calculate rotation matrix
        Mrot = rotmat(deg2rad(this%rotframe))
        !
        do  istep = 1, nsteps
            associate(step => this%steps(istep), control => this%steps(istep)%incrementation_control)
                !
                ! Acquire full stress tensor sigma
                sigma%t = Vec6ToMat33(step%stress_mode)
                Pressure%t = (trace(sigma) / 3.D0) * unit_sr_tensor%t
                S%t = sigma%t - Pressure%t
                !
                ! Rotate from the original reference frame to the sample reference frame
                sigma_rot = rotateSRTensorTo(sigma, Mrot)
                !
                ! Print the input data:
                if (doLogging(criLogInfo,this%output%verbosity)) then
                    write(display_unit,800)
                    write(display_unit,fmt=300) istep, nsteps
                    300 format(/, 'Step ', I0, ' out of ', I0, /)
                endif
                if (doLogging(criLogDebug,this%output%verbosity)) then
                    write(display_unit,fmt=310)
                    write(display_unit,400) 'sigma', 'S', 'sigma_h'
                    do j=1,3
                        write(display_unit,411) sigma%t(:,j), S%t(:,j), Pressure%t(:,j)
                    enddo
                    310 format('Input stress tensor, in the material reference frame:')
                endif
                !
                ! Follow the stress path
                !                
                info = this%calculateStressPath(sigma, control, output%evolution_output, Mrot, &
                                                incrementation_control=icv)
                if (info /= criSuccess) then
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
                if (info /= criSuccess) then
                    write(display_unit,fmt=900) 'Cannot make output for the current step'
                    exit
                endif
            end associate
        enddo
        
        ! Formats
        400 format(T15,A,T54,A,T85,A)
        ! 410 format('| SmScaled',T40,'| SmIdent',T80,'|SonA')
        411 format(3(E10.3,1X),' | ',3(E10.3,1X),' | ',3(E10.3,1X))

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
        info = criSuccess
        !
        ! Write out header lines
        !
        if (optionalDefault(header, .false.)) then
            info = criErr_IOWrite
            ! Column numbers
            write(iounit,701,iostat=ierr) (centered(i,short_column_width), i = 1,2), &
                                          (centered(i,column_width), i = 3, ncolumn_labels)
            if (ierr /= 0) return
            ! Column labels
            write(iounit,700,iostat=ierr) (column_labels(i)(1:short_column_width), i=1,2), &
                                          (centered(column_labels(i)), i=3,ncolumn_labels)
            if (ierr /= 0) return
            info = criSuccess
        endif
        !
        ! Write out data output
        if (present(output)) then
            ierr = 0
            info = criErr_IOWrite
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
            if (ierr == 0) info = criSuccess
        endif
        ! Formats for output file
        700 format(1X, 2(A9,1X),10(A18,  1X),4(5X,12(A18,1X)))
        701 format('#',2(A9,1X),10(A18,  1X),4(5X,12(A18,1X)))
        710 format(1X, 2(I9,1X),10(E18.9,1X),4(5X,12(E18.9,1X)))
    !
    end function


#ifdef ASR_STDOUT_FIXED
    !
    ! The code inside ASR_STDOUT_FIXED may be refactored in the near future, 
    ! so it is temporarily left here.
    !
        subroutine outputIdentResults(teeunits)
        implicit none
        integer,dimension(:),intent(in) :: teeunits
        !
        integer :: j, n
        type(SRTensor) :: S_tmp
                do j = 1,size(teeunits)
                    n = teeunits(j)
                    S_tmp%t = SmIdent*vS_norm
                    write(n,'(A,1X,I3,1X,A,1X,I3)') 'Step:', istep,'Increment:',increment 
                    write(n,'(A)') 'In the rotated reference frame:' 
                    call printIdentResultsT(n,Sm,S_tmp%t,SonA,D,info)
                    !
                    S_tmp%t = StIdent*vS_norm
                    write(n,'(A)') 'In the original reference frame:' 
                    call printIdentResultsT(n,Stdev,S_tmp%t,StonA,Dt,info)
                enddo
        end subroutine
            
        subroutine outputImposedStrain(teeunits)
        implicit none
        integer,dimension(:),intent(in) :: teeunits
        !
        integer :: j, n
                do j = 1,size(teeunits)
                    n = teeunits(j)
                    write(n,'(A)') 'Strain to be imposed for texture evolution De = '
                    write(n,500) De
                    write(n,*)
                enddo
        500 format(3(3(E12.5,1X),/))

        end subroutine
            
        subroutine outputStrainProgress(teeunits)
        implicit none
        integer,dimension(:),intent(in) :: teeunits
        double precision,dimension(3,3)           :: tmpP
        integer :: j, n
                do j = 1,size(teeunits)
                    n = teeunits(j)
                    !
                    write(n,'(A)') 'Increment strain:'
                    write(n,'(A,1X,F12.6)') '||P|| =', normP
                    write(n,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
                    tmpP = vec5D2tens(vP)
                    write(n,'(A)') 'P='
                    write(n,500) tmpP
                    write(n,'(A,1X,F12.6)') 'Winc =', plastic_work_inc
                    write(n,'(A)') 'Total strain:'
                    write(n,'(A,1X,F12.6)') '||Ptot|| =', totalPnorm
                    tmpP = vec5D2tens(vTotalP)
                    write(n,'(A)') 'Ptot='
                    write(n,500) tmpP
                    write(n,'(A,1X,F12.6)') 'Wtot =', plastic_work_total

                enddo
        500 format(3(3(E12.5,1X),/))

                  
        end subroutine
            
            
        subroutine outputSeparator(teeunits)
        implicit none
        integer,dimension(:),intent(in) :: teeunits
        integer :: j, n
                do j = 1,size(teeunits)
                    n = teeunits(j)
                    write(n,801)
                enddo
                801 format(112('-'))
        end subroutine
    end subroutine
    
#endif ! ASR_STDOUT_FIXED
    
end module
