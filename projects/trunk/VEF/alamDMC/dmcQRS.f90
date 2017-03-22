!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2010-11-03 (under the name alamQ)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criMacros.fpp"

!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
use qrsTypes
use dmcUtils
use dmcStressDrivenModule
use commonConfig
use commonUtils
use dmcResultFileOutput
use criMathUtils
use criRange
use criLog
use criAlgorithm
use fngVec5D
implicit none

    type,extends(StressDrivenModule) :: QRSModule
        class(range_type),pointer                 :: ptr_range

        double precision                          :: rho = 0.D0

        logical                                   :: calculate_Mfactor = .false.

        logical                                   :: use_stability_improvements = .false.

        logical                                   :: fold_symmetry = .false.
            
    contains

        procedure,pass(this)    :: readConfig => QRSModule_ReadConfig

        procedure,pass(this)    :: printConfig => QRSModule_printConfig

        procedure,pass(this)    :: run => QRSModule_run

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
        if (info /= criSuccess) return
        info = criErr_IORead
        ! Read parameters specific for the QRSModule module
        this%ptr_range => rangeFromConfig(cnfunit,info)
        if ( (info /= criSuccess) .or. (.not. associated(this%ptr_range)) ) return
        !
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
                info = criErr_IORead
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
        info = criSuccess
    !
    end function


    integer function QRSModule_printConfig(this,outunit) result (info)
    implicit none
    class(QRSModule),intent(in)         :: this
    integer,intent(in)                  :: outunit
    !
    integer :: ioerr
    !
        info = this%StressDrivenModule%printConfig(outunit)
        if (info /= criSuccess) return
        !
        ! Print banner
        write(outunit,'(A)') 'QRS: $Rev$'
        if (doLogging(criLogInfo,this%output%verbosity)) then
            ! Print-out summary of the configuration 
            !write(display_unit,fmt=fmtMsg2Other//'2(F8.3,1X))',iostat=ioerr) 'Angular range:', this%fi2min, this%fi2max
            write(outunit,fmt=fmtMsg2Int,iostat=ioerr)   'Number of points:', this%ptr_range%size() 
            write(outunit,fmt=fmtMsg2Float,iostat=ioerr) 'Stress ratio', this%rho 
            !
            write(outunit,fmt='(A, 1X)',advance='NO') 'Info:'
            if (this%use_stability_improvements) then
                write(outunit,'(A)') 'Strain rate from the previous solution will be re-used.'
            else
                write(outunit,'(A)') 'von Mises guess will be used.'
            endif
        endif
        info = criSuccess
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
    type(SRTensor)                            :: D_t, S_t, sigma, sigma_t, SonA, D, Dresume_t, SmIdent
    double precision,dimension(3,3)           :: Mrot = 0.0
    type(YLPResult)                           :: ylp_result
    !
    double precision                          :: fi1,phi,fi2, residual_resume
    integer     :: i,j, k, npoints,ofunit
    logical     :: useVMGuess
    !
    type(QRSOutputData) :: results
    !
    integer,parameter :: column_width = 15
    ! For display output:
    integer,parameter :: ncolumn_labels_display = 7, column_width_display = 14
    character(len=column_width-1),dimension(ncolumn_labels_display) :: display_column_labels = &
        [ character(len=column_width_display) ::  &
        'angle','rho','q-value','r-value','s-value','M-factor','residual' ]
    !
        info = criError
        !
        npoints = this%ptr_range%size()
        !
        RETURN_IF(info /= criSuccess, info = this%openOutputFile('.xqrs', ofunit))
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
        sigma_t%t(1,1) = root32/dsqrt(this%rho**2-this%rho+1.D0)
        sigma_t%t(2,2) = this%rho*sigma_t%t(1,1)
        !
        i = 0
        do while (this%ptr_range%next(fi2))
            i = i + 1
            !
            ! use von Mises guess as a default
            useVMGuess = .true.
            !
            if (doLogging(criLogDebug,this%output%verbosity)) then
                write(display_unit,800)
                write(display_unit,'(/,A,1X,I4,1X,A,1X,F8.3,A,/)')'Point:',i,'fi2 =',fi2, ' degs'
            endif
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
            info = this%findSolution(sigma, D, ylp_result, useVMGuess)
            if (info /= criSuccess) then
                write(display_unit,fmt=960)
                exit
            endif
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then
                info = printYLPResult(display_unit, ylp_result)
            endif
            !
            SonA%t = vec5D2tens(ylp_result%vSonA)
            SmIdent%t = vec5D2tens(ylp_result%vSonAn)
            
            if (doLogging(criLogInfo,this%output%verbosity)) then
                write(display_unit,400)
                do j=1,3
                    ! would be just:  write(display_unit,401) sigma(j,:),SmIdent(j,:),Dmcoord(j,:)
                    write(display_unit,401) (sigma%t(j,k),k=1,3), (SmIdent%t(j,k),k=1,3), (D%t(j,k), k=1,3)
                enddo
            endif
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
            associate(r => results, &
                      phis => r%phis(i), qrsvalues => r%qrsvalues(i), &
                      sigmas_x => r%sigmas_x(i), mfactors => r%mfactors(i), &
                      residuals => r%residuals(i))
                !
                phis = rad2deg(fi2)
                qrsvalues = calculateQRS(D_t%t,ylp_result%scal_s)
                sigmas_x = S_t%t(1,1) - S_t%t(3,3)
                residuals = ylp_result%R
                ! Optional: Taylor factor can be retrieved
                if (this%calculate_MFactor) then
                    call getTaylorFactor(1, mfactors, info)
                    if (info /= 0) then
                        write(display_unit,980)
                        exit
                    endif
                endif
                !
                if (doLogging(criLogInfo,this%output%verbosity)) then
                    write(display_unit,fmt=601) !
                    write(display_unit,fmt=600) (centered(display_column_labels(j)), j=1,ncolumn_labels_display) 
                    write(display_unit,fmt=610) phis, this%rho, qrsvalues, mfactors,residuals
                    write(display_unit,fmt=601)
                endif
            end associate
            !
            info = criSuccess
        enddo
        ! 
        ! End of the main loop, check what's the status of the last operation
        if (info /= criSuccess) return
        !
        info = this%fileOutput(ofunit, results, header=.true.)
        close(ofunit)
        !
        !
        400 format('| sigma',T40,'| SmIdent',T80,'|Dmcoord')
        401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
        ! Formats for the display output
        600 format(1X, 7(A14,    1X))
        601 format('|',7(14('-'),'|'))
        610 format(1X, 7(F14.6,  1X))

#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end subroutine


    !> Write out results to the output file
    integer function QRSModule_fileOutput(this, iounit, data_record, header) result(info)
    implicit none
    class(QRSModule),intent(in)                 :: this
    integer,intent(in)                          :: iounit !< Output IO unit
    type(QRSOutputData),intent(in),optional     :: data_record !< Data to be written out
    logical,intent(in),optional                 :: header !< Header to be written out
    !
    integer :: i, npoints, left, right, stride, ierr
    !
    integer,parameter :: ncolumn_labels = 8, column_width = 18
    character(len=column_width),dimension(ncolumn_labels) :: column_names = &
        [ character(len=column_width) ::  &
        'angle','rho','q-value','r-value','s-value','sigma_xx','M-factor','residual' ]
    !
        info = criErr_BadArgs
        if (optionalDefault(header,.false.)) then
            info = writeStandardHeader(iounit, column_names, [column_width])
            if (info /= criSuccess) return
        endif
        !
        if (present(data_record)) then
            info = criErr_IOWrite
            ! FIXME: flawed assumption, other arrays may have different size
            ALLOCATED_SIZE(npoints, data_record%phis)
            ! Write output file
            if (this%fold_symmetry) then
                ! \todo Use FCRI::criArray::fold_array for this task. It allows multiple folds!
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
        info = criSuccess
        !
        ! Formats for the output file
        710 format(1X, 8(E18.9,1X))
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
