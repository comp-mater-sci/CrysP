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
!
!
!> dmcQRS calculates plastic anisotropic properties, expressed in terms of q-values,
!> directly from texture data, presented in form of SMT, CUR or CUB files.
module dmcQRS
!use nllsTR
use alamYLP
!use alamEval, only: alamEval_objFx_call_count
use qrsTypes
use dmcUtils
use dmcBasicModule
use commonConfig
use commonUtils
use criMathUtils
use criRange
use criLog
use criAlgorithm
use fngVec5D

implicit none

    type,extends(BasicModule) :: QRSModule
        class(range_type),pointer                 :: ptr_range

        double precision                          :: rho = 0.D0
            
        logical                                   :: calculate_Mfactor = .false.

        logical                                   :: reuse_previous = .false.
        logical                                   :: reuse_strainrate = .false.
        logical                                   :: fold_symmetry = .false.
            
    contains
      
        procedure,pass(this)    :: readConfig => QRSModule_ReadConfig

        procedure,pass(this)    :: printConfig => QRSModule_printConfig

        procedure,pass(this)    :: run => QRSModule_run

    end type


contains

    integer function QRSModule_ReadConfig(this,cnfunit) result(info)
    implicit none
    class(QRSModule),intent(inout)              :: this
    integer,intent(in)                        :: cnfunit
    !
    logical :: use_default_settings, use_stability_improvements
    !
        use_stability_improvements = .false.
        use_default_settings = .false.
        info = BasicModule_ReadConfig(this,cnfunit)
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
                if (.not. readValue(cnfunit, use_stability_improvements)) return
                if (use_stability_improvements) then
                    this%reuse_previous = .true.
                    this%reuse_strainrate = .true.
                endif
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
        info = BasicModule_printConfig(this,outunit)
        if (info /= 0) return
        !
        info = -1
        ! Print banner
        write(outunit,'(A)') 'QRSModule: $Rev$'
        if (doLogging(criLogInfo,this%output%verbosity)) then
            ! Print-out summary of the configuration 
            !write(display_unit,fmt=fmtMsg2Other//'2(F8.3,1X))',iostat=ioerr) 'Angular range:', this%fi2min, this%fi2max
            write(outunit,fmt=fmtMsg2Int,iostat=ioerr)   'Number of points:', this%ptr_range%size() 
            write(outunit,fmt=fmtMsg2Float,iostat=ioerr) 'Stress ratio', this%rho 
            !
            write(outunit,fmt='(A,\)') 'Info:'
            if (this%reuse_previous) then
                if (this%reuse_strainrate) then
                    write(outunit,'(1X,A,\)') 'Strain rate'
                else
                    write(outunit,'(1X,A,\)') 'Stress'
                endif
                write(outunit,'(1X,A)') 'from the previous solution will be re-used.'
            else
                write(outunit,'(1X,A)') 'von Mises guess will be used.'
            endif
        endif
        info = 0
    !
    end function



    subroutine QRSModule_Run(this,info)
    implicit none
    class(QRSModule),intent(inout)              :: this
    integer,intent(out)                       :: info

    ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
    ! coordinate system
    double precision,dimension(3,3)           :: Dmcoord, SmIdent, Xmcoord_resume, Xtcoord_resume
    type(SRTensor)                            :: D_t, S_t, sigma, sigma_t, SonA
    double precision,dimension(3,3)           :: Mrot = 0.0
    !
    double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
    double precision                          :: fi1,phi,fi2
    double precision                          :: SonA_len, scal_s
    double precision                          :: R
    integer     :: i,j, k, npoints
    logical     :: useVMGuess
    double precision,dimension(:),allocatable       :: residuals,mfactors,phis, sigmas_x
    type(qrsData),dimension(:),allocatable          :: qrsvalues
    !
    integer                 :: left, right, stride
    integer                 :: ioerr
    integer,parameter       :: cnfunit = 90, ofunit = 91
    !
    integer,parameter :: ncolumn_labels = 8, column_width = 15
    ! For file output
    character(len=column_width),dimension(ncolumn_labels) :: file_column_labels = &
        [ character(len=column_width) ::  &
        'angle','rho','q-value','r-value','s-value','sigma_xx','M-factor','residual' ]
      
    ! For display output:
    integer,parameter :: ncolumn_labels_display = 7, column_width_display = 14
    character(len=column_width-1),dimension(ncolumn_labels_display) :: display_column_labels = &
        [ character(len=column_width_display) ::  &
        'angle','rho','q-value','r-value','s-value','M-factor','residual' ]
    !
        info = 1
        !
        !
        npoints = this%ptr_range%size()
        ! 
        open(unit=ofunit,file=trim(this%output%outputPrefix)//'.xqrs',iostat=ioerr)
        if (ioerr /= 0) then
            write(display_unit,fmt=952)
            return 
        endif
        write(ofunit,701) (centered(i,column_width), i = 1, ncolumn_labels)
        write(ofunit,700) (centered(file_column_labels(i)), i=1,ncolumn_labels) 

        !
        ! Apply correction to the configuration of the search procedure:
        ! there will be no need to use the full model in the last call unless 
        ! the average Taylor factor is requested.
        this%ylp%evaluate_full_model  = this%calculate_MFactor
      

        ! Make space for the results      
        allocate(qrsvalues(npoints), residuals(npoints), sigmas_x(npoints), mfactors(npoints), phis(npoints))
        residuals = 0.D0
        mfactors = 0.D0
      
        fi1 = 0.D0
        phi = 0.D0
        !
        ! Set sigma_t in such way that deviatoric part is of unit length
        sigma_t%t = 0.D0
        sigma_t%t(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(this%rho**2-this%rho+1)
        sigma_t%t(2,2) = this%rho*sigma_t%t(1,1)
        !
        ! use von Mises guess as a default
        useVMGuess = .true.
        i = 0
        do while (this%ptr_range%next(fi2))
            i = i + 1
            if (doLogging(criLogDebug,this%output%verbosity)) then
                write(display_unit,800)
                write(display_unit,'(/,A,1X,I4,1X,A,1X,F8.3,A,/)')'Point:',i,'fi2 =',fi2, ' degs'
            endif
            !
            phis(i) = fi2
            !
            fi2 = deg2rad(fi2)
            ! Calculate rotation matrix
            Mrot = rotmat(fi1,phi,fi2)

            ! Rotate from "tensile" to material coordinate system
            sigma = rotateSRTensorTo(sigma_t, Mrot)
            
            !            
            vS = tens2vec5D(sigma%t) 
            if ((this%reuse_previous) .AND. (i > 1))  then
                ! Reuse previously stored result in new coordinate system
                ! Type of result (strain rate or stress) is decided in line mared with (***)
                ! Rotate Xtcoord_resume to new coordinate system
                Xmcoord_resume = rotateSRTensorTo(Xtcoord_resume,Mrot)
                ! Set starting point
                vA = tens2vec5D(Xmcoord_resume)
                vA = vA / vec_norm2(vA)
                ! Disable Von Mises guess in multilevelYLP: vA will be used as a starting point
                useVMGuess = .false.
            endif
            !
            call multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,this%ylp,verbose=this%output%verbosity)
            if (info /= 0) then
                write(display_unit,fmt=960)
                exit
            endif
            
            residuals(i) = R
            ! 
            if (this%calculate_MFactor) then
                call getTaylorFactor(1,mfactors(i),info)
                if (info /= 0) then
                    write(display_unit,980)
                    exit
                endif
            endif
            ! Calculate normalized stess
            SonA_len = vec_norm2(vSonA)
            scal_s = SonA_len / vec_norm2(vS)
            vSonAn = vSonA / SonA_len
            !
            if (doLogging(criLogInfo,this%output%verbosity)) call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
            ! Convert AONSET vector to tensor form
            Dmcoord = vec5D2tens(vA)

            SonA%t = vec5D2tens(vSonA)
            SmIdent = vec5D2tens(vSonAn)
            
            if (doLogging(criLogInfo,this%output%verbosity)) then
                write(display_unit,400)
                do j=1,3
                    ! would be just:  write(display_unit,401) sigma(j,:),SmIdent(j,:),Dmcoord(j,:)
                    write(display_unit,401) (sigma%t(j,k),k=1,3), (SmIdent(j,k),k=1,3), (Dmcoord(j,k), k=1,3)
                enddo
            endif
            ! Rotate back to the "tensile test" coordinate system  
            D_t%t = rotateSRTensorFrom(Dmcoord, Mrot)
            S_t = rotateSRTensorFrom(SonA, Mrot)
            !
            !(***) Prepare next iteration if re-using is requested.
            if (this%reuse_previous) then
                ! Re-used data are always in "tensile" coordinate system (initial coordinate system) 
                if (this%reuse_strainrate) then
                    Xtcoord_resume = D_t%t
                else
                    ! Rotate stresses to "tensile" coordinate system 
                    Xtcoord_resume = rotateSRTensorFrom(SmIdent, Mrot)
                endif
            endif
            !            
            ! Calculate output variables
            qrsvalues(i) = calculateQRS(D_t%t,scal_s)
            sigmas_x(i) = S_t%t(1,1) - S_t%t(3,3)
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then
                write(display_unit,fmt=601) !
                write(display_unit,fmt=600) (centered(display_column_labels(j)), j=1,ncolumn_labels_display) 
                write(display_unit,fmt=610) phis(i), this%rho, qrsvalues(i), mfactors(i),residuals(i)
                write(display_unit,fmt=601)
            endif
            !
            info = 0
        enddo
        ! 
        ! End of the main loop, check what's the status of the last operation
        if (info /= 0) return
        !
        if (doLogging(criLogErr,this%output%verbosity)) then
            ! Write complete output to the terminal
            write(display_unit,800)
            write(display_unit,fmt=700)
            do i=1,npoints
                    write(display_unit,fmt=710) phis(i), this%rho, qrsvalues(i), mfactors(i), residuals(i)
            enddo
        endif
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
                write(ofunit,fmt=710) phis(left), this%rho,                     &
                                      avgQRS(qrsvalues(left:right:stride)),     &
                                      average(sigmas_x(left:right:stride) ),    &
                                      average(mfactors(left:right:stride) ),    &
                                      average(residuals(left:right:stride) )
                left = left + 1
                right = right -1
            enddo
        else
            ! Output complete set of points
            do i=1,npoints
                write(ofunit,fmt=710) phis(i), this%rho, qrsvalues(i), sigmas_x(i) ,mfactors(i),residuals(i)
            enddo
        endif
        !
        deallocate(qrsvalues, residuals,mfactors,phis)
        close(ofunit)
        !
        info = 0
        !
        400 format('| sigma',T40,'| SmIdent',T80,'|Dmcoord')
        401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
        ! Format for header file
        500 format('#Material:',1X,A,/,'#Generated by QRSModule $Revision$')

        ! Formats for the display output
        600 format(1X, 7(A14,    1X))
        601 format('|',7(14('-'),'|'))
        610 format(1X, 7(F14.6,  1X))
          
        ! Formats for output file
        700 format(1X, 8(A18,  1X))
        701 format('#',8(A18,  1X))
        710 format(1X, 8(E18.9,1X))

#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
    !
    end subroutine


end module
