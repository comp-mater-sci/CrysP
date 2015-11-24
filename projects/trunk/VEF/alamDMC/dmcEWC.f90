!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2015-11-19
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> DMC Equi-Work Contour
!>
! #include "criMacros.fpp"
    
module dmcEWC
use nllsTR
use alamYLP
use alamEval, only: alamEval_objFx_call_count
!use dmcUtils
use altaySub
use dmcBasicModule
use commonConfig
use commonUtils
use criMathUtils
use criPath
use criAlgorithm
use criRange
use criLog
use criUncomment
use criNumerics
use fngVec5D
implicit none



    integer,parameter :: scalingStrainTensor = 0, &
                         scalingStrainTensorComponent = 1, &
                         scalingPlasticWork = 2

    integer,parameter :: incrementFixed = 0, &
                         incrementAuto = 1

    type :: IncrementationControlVariables
        
        integer :: increment = 0
        
        !> Plastic work in the current increment
        double precision    :: plastic_work_inc = 0.D0
        
        !> Total plastic work
        double precision    :: plastic_work_total = 0.D0
        
        !> Increment of plastic strain
        double precision,dimension(alamEval_vSD_dim)    :: vP_inc = 0.D0
        
        !> Total plastic strain:
        double precision,dimension(alamEval_vSD_dim)    :: vP = 0.D0
        
        
        !> Sum of absolute plastic strain increments:
        !> \f[
        !>    vP_{norms} = \sum \| vE_{inc} \|
        !> \f]
        double precision,dimension(alamEval_vSD_dim)    :: vP_norms
        
    end type
    
    
    type,extends(IncrementationControlVariables) :: IncrementationControl
        
    contains
    
        procedure,pass(this)        :: update => IncrementationControl_update
        
    end type
    
    
    type :: OutputRecord
         
        double precision :: vm_strain = 0.D0
        double precision :: vm_strain_total = 0.D0
        
        double precision :: P_abs_sum = 0.D0
        
        double precision :: plastic_potential = 0.D0
        double precision :: taylor_factor = 0.D0
        double precision :: scal_s = 0.D0
        double precision :: norm_SonA = 0.D0
        double precision :: R = 0.D0

        type(SRTensor) :: A
        type(SRTensor) :: SonA

        type(IncrementationControlVariables) :: icv
        
    end type
    
    
    
    type :: EvolutionOutput
        type(OutputRecord),dimension(:),allocatable      :: values
    end type
    
   
    !
    
    
    type :: IncrementationControlSettings
        
        integer         :: scaling_type = scalingStrainTensor
        
        integer         :: incrementation_type = incrementFixed
        
        double precision :: increment_size = 0.D0
        
        double precision :: step_size = 0.D0

    end type


    type,extends(BasicModule),abstract :: StressDrivenEvolutionModule
        
        type(IncrementationControlSettings) :: control
        
    contains
        procedure,pass(this)    :: calculateStressPath => StressDrivenEvolutionModule_calculateStressPath
        
    end type


    type,extends(StressDrivenEvolutionModule) :: EWCModule
        
        type(EulerAngles)                       :: reference_frame
        
        character(len=max_pathlen)              :: output_fname = ''
        
        double precision,dimension(sr_symm_voigt_dim)   :: reference_stress_mode = 0.D0
        
        !> Range of angles that provide stress ratios
        class(range_type),pointer               :: ptr_theta_range => null()
        
        class(range_type),pointer               :: ptr_contourlevel_range => null()
        
        logical                                :: report_state = .false.
        
        !> Number of data points per contour level along regular evolution lines
        !>
        !> \todo Consider changing it into double: much less limiting control over the number
        !> of data points
        integer                                 :: n_intervals = 0
    contains
      
        procedure,pass(this)    :: readConfig => EWCModule_ReadConfig
            
        procedure,pass(this)    :: run => EWCModule_run
        
        procedure,pass(this)    :: fileOutput => EWCModule_fileOutput

        procedure,pass(this)    :: fileOutputMeta => EWCModule_fileOutputMeta
        
    end type
      
     
contains

      integer function EWCModule_ReadConfig(this,cnfunit) result(info) 
      implicit none
      class(EWCModule),intent(inout)            :: this
      integer,intent(in)                        :: cnfunit
      !
      integer :: ioerr
      !
            info = BasicModule_ReadConfig(this,cnfunit) 
            if (info /= criSuccess) return
            info = criErr_IORead
            ! Read parameters specific for the ASRModule
            read(cnfunit,fmt=*,iostat=ioerr) this%reference_frame
            !
            ! Evolution along the reference stress mode
            read(cnfunit,fmt=*,iostat=ioerr) this%reference_stress_mode
            read(cnfunit,fmt=*,iostat=ioerr) this%control%scaling_type, this%control%step_size, this%control%increment_size
            !
            ! Contour lines
            this%ptr_theta_range => rangeFromConfig(cnfunit,info)
            if (info /= criSuccess .or. .not. associated(this%ptr_theta_range)) return
            this%ptr_contourlevel_range => rangeFromConfig(cnfunit,info)
            if (info /= criSuccess .or. .not. associated(this%ptr_contourlevel_range)) return
            read(cnfunit,*,iostat=ioerr) this%n_intervals
            ! read(cnfunit,fmt='(A)',iostat=ioerr) this%output_fname
            ! call stripComment(this%output_fname)
            ! read(cnfunit,fmt='(L2)',iostat=ioerr) this%report_state
            if (ioerr /= 0) then
                  write(display_unit,fmt=902) 'EWCModule'
                  return
            endif
            info = criSuccess
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end function


    subroutine EWCModule_Run(this,info)
    implicit none
    class(EWCModule),intent(inout)            :: this
    integer,intent(out)                       :: info
    !
    integer :: i, j
    double precision :: theta
    
    type(SRTensor)  :: S
    
    type(EvolutionOutput) :: ref_output, output
    double precision,dimension(:,:),allocatable :: results ! Shape is: [1:n_countours,1:n_theta]
    type(IncrementationControlSettings) :: evolution_control
    
    !> \fixme The variables below should be promoted to configuration parameters
    double precision,dimension(sr_symm_voigt_dim,2)   :: base_vectors
    double precision,dimension(sr_symm_voigt_dim) :: S_vector
    !
    double precision,dimension(:),allocatable :: vEquivalentStrainLevels, vPlasticWorkLevels
    double precision,dimension(:),allocatable :: vPlasticWork, vNormSonA, vEquivalentStrain
    
    double precision,dimension(:),allocatable :: vTheta
    
    type(BarycentricInterpolator) :: bi
    integer,parameter :: interpolation_order = 2
    integer :: n_theta, n_contours
    logical :: tmp_flag
    !
        ! Default base vectors: uniaxial s11 and s22
        base_vectors = 0.D0
        base_vectors(1,1) = 1.D0
        base_vectors(2,2) = 1.D0
        !
        ! Prepare the input data: array of increments, and
        ! array of results.
        n_theta = this%ptr_theta_range%size()
        n_contours = this%ptr_contourlevel_range%size()
        allocate(vTheta(n_theta))
        allocate(results(n_contours, n_theta))
        allocate(vPlasticWorkLevels(n_contours), vEquivalentStrainLevels(n_contours))
        do i = 1, n_contours
            tmp_flag = this%ptr_contourlevel_range%next(vEquivalentStrainLevels(i))
        enddo

        !
        ! Evaluate the reference mode
        S%t = Vec6ToMat33(this%reference_stress_mode)
        info = this%calculateStressPath(S, this%control, ref_output)
        if (info /= criSuccess) return
        !
        ! Calculate work levels that correspond to the requested levels of 
        ! equivalent plastic strain.
        vEquivalentStrain = ref_output%values(:)%vm_strain_total
        vPlasticWork = ref_output%values(:)%icv%plastic_work_total
        call BarycentricInterpolator_init(bi, 2, vEquivalentStrain, vPlasticWork, info)
        if (info /= criSuccess) then
            info = criError
            return
        endif
        do i = 1, n_contours
            vPlasticWorkLevels(i) = interpolate(bi, vEquivalentStrainLevels(i))
        enddo
        !
        ! prepare controls for evolution lines
        evolution_control%scaling_type = scalingPlasticWork
        evolution_control%step_size = maxval(vPlasticWorkLevels)
        evolution_control%increment_size = evolution_control%step_size / dble(this%n_intervals * n_contours)
        !
        ! Transform: main loop over the theta angles. Calculate the evolution
        !            of the state. Reset the state at the end of each iteration.
        !            Note: the iterations of the main loop are conceptually independent
        !            of each other. Current implementation of the back-end CP model
        !            prevents exploiting that.
        i =  0
        do while (this%ptr_theta_range%next(theta))
            i = i + 1
            vTheta(i) = theta
            theta = deg2rad(theta)
            !
            ! Calculate S by combining the base vectors
            S_vector = base_vectors(:,1)*cos(theta) + base_vectors(:,2)*sin(theta)
            S%t = Vec6ToMat33(S_vector)
            
            ! Re-initialize AlTay
            call finalizeAltay(info)
            call initAltay(this%altay,info) 
            info = this%calculateStressPath(S, evolution_control, output) 
            !
            vPlasticWork = output%values(:)%icv%plastic_work_total
            vNormSonA = output%values(:)%scal_s
            call BarycentricInterpolator_init(bi, 2, vPlasticWork, vNormSonA, info)
            do j = 1, size(vPlasticWorkLevels)
                results(j,i) = interpolate(bi, vPlasticWorkLevels(j))
            enddo
            
        enddo
        if (info /= 0) return
        !
        info = this%fileOutput(vTheta, vEquivalentStrainLevels, results)
        info = this%fileOutputMeta('contours',vEquivalentStrainLevels, vPlasticWorkLevels)
        info = this%fileOutputMeta('reference',vEquivalentStrainLevels, vPlasticWorkLevels)
        !
    end subroutine


    !> Post-process the result and generate the output.
    integer function EWCModule_fileOutput(this,vTheta, vLevels, results) result(info)
    implicit none
    class(EWCModule),intent(inout)              :: this
    double precision,dimension(:),intent(in)    :: vTheta, vLevels
    double precision,dimension(:,:),intent(in)  :: results
    !
    character(len=64) :: fmt_res
    integer :: iounit, ierr, i, n_theta, n_contours
    !
        info = criErr_BadArgs
        n_theta = size(vTheta)
        n_contours = size(results,dim=1)
        if ((size(vLevels) /= n_contours) .or. (size(results,dim=2) /= n_theta)) return
        info = criErr_IOOpen
        open(newunit=iounit, file=trim(this%output%outputPrefix)//'.ewc', status='replace', iostat=ierr)
        if (ierr /= 0) return
        info = criErr_IOWrite
        ! Make format strings for the header and the data
        write(fmt_res, '(A,1X,I0,A)') '(F8.2,1X',n_contours,'(E15.6,1X))'
        do i = 1, n_theta
            write(iounit,fmt=fmt_res,iostat=ierr) vTheta(i), results(:,i)
            if (ierr /= 0) exit
        enddo
        if (ierr == 0) info = criSuccess
        close(iounit)
    !
    end function


    !> Create meta-data output file.
    integer function EWCModule_fileOutputMeta(this, prefix, vEquivalentStrainLevels, vPlasticWorkLevels) result(info)
    implicit none
    class(EWCModule),intent(inout)              :: this
    character(len=*),intent(in)                 :: prefix
    double precision,dimension(:),intent(in)    :: vEquivalentStrainLevels, vPlasticWorkLevels
    !
    integer :: iounit, ierr, i
    !
        info = criErr_BadArgs
        if (size(vEquivalentStrainLevels) /= size(vPlasticWorkLevels)) return
        info = criErr_IOOpen
        open(newunit=iounit, file=trim(this%output%outputPrefix)//'_'//trim(prefix)//'.ewcm', &
             status='replace', iostat=ierr)
        if (ierr /= 0) return
        info = criErr_IOWrite
        ! Make format strings for the header and the data
        write(iounit,'(A15,1X,A15)') 'eps_vM', 'W(eps_vM)'
        do i = 1, size(vPlasticWorkLevels)
            write(iounit,fmt='(E15.7,1X,E15.7)',iostat=ierr) vEquivalentStrainLevels(i), vPlasticWorkLevels(i)
            if (ierr /= 0) exit
        enddo
        if (ierr == 0) info = criSuccess 
        close(iounit)
    !
    end function

    
    !> \todo optional initial icv should be provided as a parameter
    integer function StressDrivenEvolutionModule_calculateStressPath(this, S, control, outputs) result(info)
    implicit none
    class(StressDrivenEvolutionModule),intent(in) :: this
    type(SRTensor),intent(in)   :: S
    class(IncrementationControlSettings),intent(inout) :: control
    type(EvolutionOutput),intent(out)   :: outputs
    
    type(SRTensor) :: D, De, Se
    double precision :: scaling_factor, taylor_factor
    type(YLPResult) :: ylp
    double precision,dimension(alamEval_vSD_dim) :: vDe, vSe
    type(IncrementationControl) :: icv
    !
    !> \fixme Shortcut: array that is "lage enough" to keep the outputs. 
    !>        To be replaced by list or another dynamic storage.
    integer,parameter :: max_records = 100
    type(OutputRecord),dimension(max_records) :: tmp_records
    integer :: idx
    !
        !
        ! Follow the evolution line along S
        !
        Se%t = 0.D0
        De%t = 0.D0
        !> \fixme Get rid of the big array
        ! -->>
        idx = 0
        ! <<--
        !
        do
            ! Calculate the strain rate mode
            info = this%findSolution(S, D, ylp)
            if (info /= criSuccess) exit
            ! -->>
            write(*,100)
            100 format('.',\)
            ! <<--
            !
            ! Calculate incrementation control variables
            !
            ! Calculate increment of plastic strain to be imposed for texture evolution: 
            select case(control%scaling_type)
            case(scalingStrainTensor)
                !! -> Scale the vA in order to get ||vA|| = NormIter
                scaling_factor = (control%increment_size / norm2(ylp%vA))
                !
            case(scalingPlasticWork)
                scaling_factor = (control%increment_size / ylp%plast_pot)
            !case(scaleTensileComponent)
            !      control_variable = abs(P_t(1,1))
            !      !! -> Scale the D_t in order to get ||Dt_11|| equal to NormIter
            !      scaling_factor = (this%NormIter / abs(D_t(1,1)))
            case default
                info = criErr_BadArgs
                exit
            end select
            !
            ! Calculate strain increment for material state evolution
            vDe = ylp%vA * scaling_factor
            De%t = vec5D2tens(vDe)
            ! Update material state
            call makeTextureUpdateStep(De%t,Se%t,taylor_factor,this%output%outputRequest,info)
            if (info /= 0) exit !< \fixme Literal constant in makeTextureUpdateStep
            vSe = tens2vec5D(Se%t)
            !
            ! Add output record to the list
            !> \fixme Get rid of the big array
            ! -->>
            idx = idx + 1
            call setOutputRecord(tmp_records(idx), icv%IncrementationControlVariables, ylp, taylor_factor, info)
            if (idx >= max_records) exit
            ! <<--
            !
            info = criSuccess
            select case(control%scaling_type)
            case(scalingStrainTensor)
                if (norm2(icv%vP) >= control%step_size) exit
            !
            case(scalingPlasticWork)
                if (icv%plastic_work_total >= control%step_size) exit
            !
            end select
            !
            ! Update icv
            !
            call icv%update(vDe, vSe, info)
        !
        enddo
        ! -->>
        write(*,200)
        200 format('*')
        ! <<--
        !> \fixme Get rid of the big array. It should be asArray on list.
        ! -->>
        outputs%values = tmp_records(1:idx)
        ! <<--
    !
    end function
    

    
    subroutine setOutputRecord(this, icv, ylp, taylor_factor, info)
    implicit none
    type(OutputRecord),intent(out)              :: this
    type(IncrementationControlVariables),intent(in) :: icv
    type(YLPResult),intent(in)                  :: ylp
    double precision,intent(in)                 :: taylor_factor
    integer,intent(out)                         :: info
    !
        this%vm_strain = root23 * norm2(icv%vP_inc)
        this%vm_strain_total = root23 * norm2(icv%vP)
        this%P_abs_sum = sum(icv%vP_norms)
        !
        this%plastic_potential = ylp%plast_pot
        this%scal_s = ylp%scal_s
        this%norm_SonA = norm2(ylp%vSonA)
        this%R = ylp%R
        
        this%taylor_factor = taylor_factor

        this%A%t = vec5D2tens(ylp%vA)
        this%SonA%t = vec5D2tens(ylp%vSonA)

        this%icv = icv
        
        info = criSuccess
        
    end subroutine
    
    
    subroutine IncrementationControl_update(this, vDe, vSe, info)
    implicit none
    class(IncrementationControl),intent(inout)      :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: vDe, vSe
    integer,intent(out)                             :: info
    !
        ! Plastic work in the current increment
        this%plastic_work_inc = dot_product(vDe,vSe)
        ! Total plastic work
        this%plastic_work_total = this%plastic_work_total + this%plastic_work_inc
        ! Increment of plastic strain
        this%vP_inc = vDe
        ! Total plastic strain:
        this%vP = this%vP + this%vP_inc
        ! Sum of absolute plastic strain increments:
        this%vP_norms = this%vP_norms + abs(this%vP_inc)
        this%increment = this%increment  + 1
        info = criSuccess
    !
    end subroutine
 
end module
