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
use altaySub
use alamYLP
use dmcStressDrivenEvolutionModule
use criMathUtils
use criRange
use criNumerics
use fngVec5D
implicit none


    integer,parameter,private :: n_base_vectors = 2


    type,extends(StressDrivenEvolutionModule) :: EWCModule
        
        type(EulerAngles)                       :: reference_frame
        
        character(len=max_pathlen)              :: output_fname = ''
        
        double precision,dimension(sr_symm_voigt_dim)   :: reference_stress_mode = 0.D0

        double precision,dimension(sr_symm_voigt_dim,n_base_vectors)   :: base_vectors = 0.D0

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
      integer :: ioerr, i
      !
            info = BasicModule_ReadConfig(this,cnfunit) 
            if (info /= criSuccess) return
            info = criErr_IORead
            ! Read parameters specific for the ASRModule
            read(cnfunit,fmt=*,iostat=ioerr) this%reference_frame
            do i = 1, size(this%base_vectors,dim=2)
                read(cnfunit,fmt=*,iostat=ioerr) this%base_vectors(:,i)
                if (ioerr /= 0) return
                if (norm2(this%base_vectors(:,i)) < epsilon(0.D0)) then
                    write(display_unit,fmt=900) 'Base vector must not be of length zero'
                endif
                this%base_vectors(:,i) = this%base_vectors(:,i) / norm2(this%base_vectors(:,i))
            enddo
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
    
    type(SRTensor)  :: sigma
    
    type(EvolutionOutput) :: ref_output, output
    double precision,dimension(:,:),allocatable :: results ! Shape is: [1:n_countours,1:n_theta]
    type(IncrementationControlSettings) :: evolution_control
    
    double precision,dimension(sr_symm_voigt_dim) :: sigma_vector
    !
    double precision,dimension(:),allocatable :: vEquivalentStrainLevels, &
                                                 vPlasticWorkLevels, &
                                                 vPlasticWork_ref, &
                                                 vEquivalentStrain_ref, &
                                                 vPlasticWork, &
                                                 vScalS

    double precision,dimension(:),allocatable :: vTheta
    
    type(BarycentricInterpolator) :: bi
    integer,parameter :: interpolation_order = 2
    integer :: n_theta, n_contours
    logical :: tmp_flag
    !
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
        sigma%t = Vec6ToMat33(this%reference_stress_mode)
        info = this%calculateStressPath(sigma, this%control, ref_output)
        if (info /= criSuccess) return
        !
        ! Calculate work levels that correspond to the requested levels of 
        ! equivalent plastic strain.
        vEquivalentStrain_ref = ref_output%values(:)%vm_strain_total
        vPlasticWork_ref = ref_output%values(:)%icv%plastic_work_total
        call BarycentricInterpolator_init(bi, 2, vEquivalentStrain_ref, vPlasticWork_ref, info)
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
            sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta)
            sigma%t = Vec6ToMat33(sigma_vector)
            !
            ! Re-initialize AlTay
            call finalizeAltay(info)
            if (info /= 0) exit
            call initAltay(this%altay,info)
            if (info /= 0) exit
            !
            if (this%calculateStressPath(sigma, evolution_control, output) /= criSuccess) then
                ! For a certain reason we cannot calculate this path.
                results(:,i) = 0.D0
                cycle
            endif
            !
            vPlasticWork = output%values(:)%icv%plastic_work_total
            vScalS = output%values(:)%scal_s
            call BarycentricInterpolator_init(bi, 2, vPlasticWork, vScalS, info)
            if (info == criSuccess) then
                do j = 1, size(vPlasticWorkLevels)
                    results(j,i) = interpolate(bi, vPlasticWorkLevels(j))
                enddo
            else
                ! something is wrong with the input data (size of arrays, content?)
                ! Let's ignore this line.
                results(:,i) = 0.D0
                cycle
            endif
            
        enddo
        if (info /= 0) return
        !
        info = this%fileOutput(vTheta, vEquivalentStrainLevels, results)
        info = this%fileOutputMeta('contours',vEquivalentStrainLevels, vPlasticWorkLevels)
        info = this%fileOutputMeta('reference',vEquivalentStrain_ref, vPlasticWork_ref)
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

end module
