!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-05-17 (under the name alamTSA)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> dmcUDSA (Uniaxially-Dominated Stress Analysis)  allows one to track anisotropic properties  
!> along deformation due to the uniaxial tension or compression stress.
module dmcUDSA
use alamYLP
use altaySub
use dmcUtils
use dmcStressDrivenEvolutionModule
use dmcIncrementationControl
use commonUtils
use qrsTypes
use criLog
use criAlgorithm
use criRange
use criNamedRange
use criConfigReader
use fngVec5D
implicit none

      !> \fixme Workaround: once UDSA makes use of dmcIncrementationControl, these
      !>        constants will no longer be needed.
      integer,parameter                         :: scaleFullTensor = scalingStrainTensor, &
                                                   scaleTensileComponent = scalingStrainTensorComponent

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
      
            procedure,pass(this)    :: readConfig => UDSAModule_ReadConfig
            
            procedure,pass(this)    :: run => UDSAModule_run

            procedure,pass(this)    :: printConfig => UDSAModule_printConfig
            
            procedure,private,pass(this)    :: createOutputFile => UDSAModule_createOutputFile
            
            procedure,pass(this)    :: outputPrefix => UDSAModule_outputPrefix
      end type

contains

      integer function UDSAModule_ReadConfig(this,cnfunit) result(info)
      implicit none
      integer,intent(in)                        :: cnfunit
      class(UDSAModule),intent(inout)            :: this
      !
      logical :: use_default_settings
      double precision,dimension(3) :: arr_euler      
      !
            info = BasicModule_ReadConfig(this,cnfunit)
            if (info /= criSuccess) return
            info = criErr_IORead
            ! Read parameters specific for the UDSAModule program
            if (.not. readKeyword(cnfunit, sample_orientation_types, this%orientation_type_id)) return
            ! Read the sub-options
            select case(this%orientation_type_id)
            case(sample_orientation_inplane_id)
                  this%ptr_orientation_range => rangeFromConfig(cnfunit, info)
                  if (info /= criSuccess .or. .not. associated(this%ptr_orientation_range)) return
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
            if (info /= criSuccess) return
            !
            if (.not. readKeyword(cnfunit, stress_states, this%stress_state_id)) return
            use_default_settings = .true.
            if (.not. readValue(cnfunit, use_default_settings)) return
            if (.not. use_default_settings) then
                if (.not. readValue(cnfunit, this%rho)) return
            endif
            info = criSuccess
      !
      end function

      
      integer function UDSAModule_printConfig(this,outunit) result (info)
      implicit none
      class(UDSAModule),intent(in)         :: this
      integer,intent(in)                  :: outunit
      !
      character(len=32)       :: description, orientation
      !
            info = BasicModule_printConfig(this,outunit)
            if (info /= 0) return
            info = criErr_BadArgs
            ! Introduce youself ;-)
            write(outunit,'(A)') 'UDSA, $Rev$'
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then 
                  if (this%stress_state_id == tension_state) then
                        description = 'uniaxial tensile'
                  else
                        description = 'uniaxial compression'
                  endif
                  write(outunit,fmt=fmtMsg2Msg) 'Test type:', description
                  
                  if (resolveId(sample_orientation_types, this%orientation_type_id, orientation)) then
                        write(outunit,fmt=fmtMsg2Other//'A)') 'Orientation of the sample:', orientation
                  endif
                  write(outunit,fmt=fmtMsg2Other//'G0.4)') 'Stress ratio:', this%rho
                  !
                  select case(this%control%scaling_type)
                        case(scaleFullTensor)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling full tensor'
                        case(scaleTensileComponent)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling tensile component'
                        case default
                              write(outunit,*) 'Unknown scaling type, full tensor will be used'
                  end select
            endif
            info = criSuccess
      !
      end function
      

      subroutine UDSAModule_Run(this,info)
      implicit none
      class(UDSAModule),intent(inout)            :: this
      integer,intent(out)                        :: info
      !
      ! Note about naming convention for variables:
      !    - All variables for vectors and tensors suffixed with _t are expressed 
      !      in the "tensile sample coordinate system".
      !    - All other variables are implicitly expressed in the "material coordinate system"
      type(SRTensor) :: sigma, sigma_t, S_t, D_t, P_t, P_t_end
      double precision,dimension(rot_matrix_dim,rot_matrix_dim) :: Mrot = 0.0
      type(EvolutionOutput) :: output
      type(qrsData)     :: qrsvalue, qrsvalue_accum
      type(EulerAngles) :: sample_orientation
      double precision  :: angle, stress_direction, Tnorm, TSNorm
      integer :: test_run, n_test_runs, increment, ierr, ofunit
      !
      ! Check the preconditions
      !
      info = criErr_BadArgs
      if (.not. associated(this%ptr_orientation_range)) return
      
      !
      ! Prepare the input data      
      ! Take uniaxial/{slightly biaxial} tensile stress, to be rotated to the given sample
      ! orientation.
      !
      !> Uniaxial stress state. Negative value denotes compressive state; 
      !> non-negative values are used for tensile state.
      sigma_t%t = 0.D0
      stress_direction = merge(-1.D0,1.D0,(this%stress_state_id == compression_state))
      sigma_t%t(1,1) = stress_direction * sqrt(3.D0/2.D0)/dsqrt(this%rho**2-this%rho+1)
      sigma_t%t(2,2) = this%rho*sigma_t%t(1,1)
      !
      ! Loop over test set
      !
      test_run = 0
      
      n_test_runs = this%ptr_orientation_range%size()
      
      test_run_loop: do while (this%ptr_orientation_range%next(angle))
            test_run = test_run + 1
            !
            if (doLogging(criLogDebug,this%output%verbosity))  write(display_unit,800)
            !
            ! Come back to the initial material state if needed
            ! Re-initialize altay 
            if (n_test_runs > 1) then
                  ! Re-initialize AlTay
                  call finalizeAltay(info)
                  if (info /= 0) exit
                  this%altay%output_prefix = this%outputPrefix(angle)
                  call initAltay(this%altay,info)
                  if (info /= 0) exit
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

                  
            info = this%calculateStressPath(sigma, this%control, output, Mrot)
            if (info /= criSuccess) then
                write(display_unit,fmt=960)
                exit
            endif
            !
            ! Process the output evolution path and produce result file
            !
            qrsvalue = qrsData(0.D0, 0.D0, 0.D0) 
            qrsvalue_accum = qrsData(0.D0, 0.D0, 0.D0)
            !
            do increment = 1, size(output%values) - 1
                  ! Total plastic strain at the _begining_ of the inrement.

                  associate(v => output%values(increment))
                        
                        ! Rotate back to the "tensile test" coordinate system   
                        D_t = rotateSRTensorFrom(v%A, Mrot)
                        S_t = rotateSRTensorFrom(v%SonA, Mrot)
                        
                        ! Total deviatoric strain (Note: the total, not per-step) 
                        P_t%t = vec5D2tens(v%icv%vP_total) ! at the beginning of the increment
                        P_t_end%t = P_t%t + v%P_inc_evol%t ! at the end of the increment
                        P_t = rotateSRTensorFrom(P_t, Mrot)
                        P_t_end = rotateSRTensorFrom(P_t_end, Mrot)
                        !
                        ! Tensile strain and stress
                        TNorm = abs(P_t%t(1,1))
                        TSNorm = abs(S_t%t(1,1))
                        !
                        ! Calculate output variables
                        !
                        ! Calculate q and r in tensile reference frame
                        qrsvalue = calculateQRS(D_t%t,v%scal_s)
                        qrsvalue_accum = calculateQRS(P_t_end%t,v%scal_s)
                        !
                        ! Write out the result
                        write(ofunit,710,iostat=ierr) increment, &
                                                      v%vm_strain, &
                                                      v%P_abs_sum, &
                                                      TNorm, &
                                                      TSnorm, &
                                                      v%icv%plastic_work_total, &
                                                      v%dotWonA, &
                                                      v%taylor_factor, & 
                                                      v%norm_SonA, &
                                                      qrsvalue, &
                                                      qrsvalue_accum%qvalue, &
                                                      qrsvalue_accum%rvalue, &
                                                      v%R
                        if (ierr /= 0) then
                              info = criErr_IOWrite
                              exit
                        endif
                  !
                  end associate 
            enddo            
            !
            close(ofunit)
            if (info /= criSuccess) then     
                  write(display_unit, fmt=900) 'Unable to store results for the current virtual tests'
            endif
      !
      end do test_run_loop

      710 format(1X, 1(I9,1X),14(E18.9,1X))
          
          
#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
      
      end subroutine
      
 
      integer function UDSAModule_createOutputFile(this, iounit, tag_number) result(info)
      implicit none
      class(UDSAModule),intent(in)              :: this
      integer,intent(out)                       :: iounit
      double precision,intent(in),optional      :: tag_number
      !
      integer :: i, ierr
      character(len=max_pathlen) :: datafile_path
      !      
      integer,parameter :: ncolumn_labels = 15, column_width = 15, short_column_width = 9
      character(len=column_width),dimension(ncolumn_labels) :: file_column_labels = [ character(len=column_width) ::  &
           'increment','eps_vM','Pnorm','Tnorm','S(A)_11','W','dotW(A)','M-factor','||S(A)||','q-value','r-value','s-value', &
           'q-valueA', 'r-valueA','residual']
      !

            iounit = 0
            info = criErr_IOWrite
            datafile_path = this%outputPrefix(tag_number)
            open(newunit=iounit,file=trim(datafile_path)//'.uds', status='replace', iostat=ierr)
            if (ierr /= 0) return
            write(iounit,701,iostat=ierr) centered(1,short_column_width), &
                                          (centered(i,column_width), i = 2, ncolumn_labels)
            if (ierr /= 0) return
            write(iounit,700,iostat=ierr) file_column_labels(1)(1:short_column_width), &
                                          (centered(file_column_labels(i)), i=2,ncolumn_labels) 
            if (ierr == 0) info = criSuccess
            !
            ! Formats for the output file
            700 format(1X, 1(A9,1X),14(A18,  1X))
            701 format('#',1(A9,1X),14(A18,  1X))
      !
      end function
      
      
      function UDSAModule_outputPrefix(this, tag_number) result(path)
      implicit none
      character(len=max_pathlen)                :: path
      class(UDSAModule),intent(in)              :: this
      double precision,intent(in),optional      :: tag_number
      !
      character(len=max_pathlen) :: datafile_tag
      !
            if (present(tag_number)) then
                  ! Make a decoration string based on angle.
                  ! Substitute '.' with '_'
                  write(datafile_tag, '(F10.3)') tag_number
                  datafile_tag = '_' // trim(adjustl(datafile_tag))
                  datafile_tag = replaceAll(datafile_tag, '.', '_')
            else
                  datafile_tag = ''
            endif
            path = trim(this%output%outputPrefix)//datafile_tag
      !  
      end function
      
#ifdef UDSA_STDOUT_FIXED      
    subroutine UDSAModule_onIncrementEnd(this, output_record)
    implicit none
    class(UDSAModule),intent(in) :: this
    type(OutputRecord),intent(in)                 :: output_record

      !
      ! For display output
      integer,parameter :: ncolumn_labels_display = 12, column_width_display = 12, short_column_width_display = 5
      character(len=column_width_display),dimension(ncolumn_labels_display) :: display_column_labels = [ character(len=14) ::  &
         'iter','eps_vM','Pnorm','Tnorm','W','dotW(A)','M-factor','||S(A)||','q-value','r-value','s-value', 'residual']

      
                  dotWonA = dot_product(vA, vSonA)
                  norm_sona = vec_norm2(vSonA)
                  scal_s = norm_sona / vec_norm2(vS)
                  ! Calculate normalized stess
                  vSonAn = vSonA / vec_norm2(vSonA) 
                  !
                  if (doLogging(criLogInfo,this%output%verbosity)) call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
                  !
                  D = vec5D2tens(vA)
                  SmIdent = vec5D2tens(vSonAn)
                  !
                  if (doLogging(criLogDebug,this%output%verbosity)) then
                        write(display_unit,400)
                        do j=1,3
                              write(display_unit,401) S(j,:),SmIdent(j,:),D(j,:)
                        enddo
                        write(display_unit,*)
                  endif
      
                  if (doLogging(criLogInfo,this%output%verbosity)) then
                        write(display_unit,601)
                        write(display_unit,600) display_column_labels(1)(1:short_column_width), & 
                                                (trim(display_column_labels(i)), i=2,size(display_column_labels)) 
                        write(display_unit,610) step, root23*normP, Pnorm, TNorm, plastic_work_total, dotWonA, &
                                                taylor_factor, &
                                                norm_sona, qrsvalue, R
                        write(display_unit,601)
                  endif
                  !!
                  ! Terminate if requested to do so.
                  if (control_variable >= this%NormMax)  exit
                  !
                  !! -> Impose De as ALAMEL input, advance the state of texture
                  !
                  if (doLogging(criLogInfo,this%output%verbosity)) then
                        write(display_unit,'(A)') 'Strain to be imposed for texture evolution De = '
                        write(display_unit,500) De
                        write(display_unit,*)
                  endif
                  
                  !
                  if (doLogging(criLogErr,this%output%verbosity)) then
                        write(display_unit,'(A)') 'Total strain P:'
                        write(display_unit,500) P
                        write(display_unit,'(A)') 'Total strain P_t (in tensile test reference frame):'
                        write(display_unit,500) P_t
                        write(display_unit,'(A,1X,F12.6)') '||P|| =', normP
                        write(display_unit,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
                        write(display_unit,'(A,1X,E12.5)') 'Wtot =', plastic_work_total
                        write(display_unit,*)
                  endif
         
      400 format('|Smcoord',T42,'|SmIdent',T86,'|Dmcoord')
      401 format(3(E11.4,1X),T42,3(E11.4,1X),T86,3(E11.4,1X))

      500 format(2(3(E12.5,1X),/),(3(E12.5,1X)))
      
      501 format(3(E12.5,1X),/,3(E12.5,1X),/,3(E12.5,1X))
      
      510 format('Strain increment',T40,'Deviatoric stress')
      511 format(3(E10.3,1X),T40,3(E10.3,1X))               
                  
      ! Formats for the display
      600 format(1(1X,A5),11(A12,1X))
      601 format(1('|',5('-')),'|',11(12('-'),'|'))
      610 format(1(1X,I5),11(F12.6,1X))


      end subroutine
#endif ! UDSA_STDOUT_FIXED    
      
end module
