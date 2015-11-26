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
use nllsTR
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use dmcUtils
use dmcBasicModule
use commonUtils
use qrsTypes
use criLog
use criAlgorithm
use fngVec5D
implicit none

      integer,parameter                         :: scaleFullTensor = 0, scaleTensileComponent = 1

      type,extends(BasicModule) :: UDSAModule
            double precision  :: angle = 0.D0, NormMax = 0.D0, NormIter = 0.D0

            integer           :: scalingID = scaleFullTensor
            
            !> Uniaxial stress state. Negative values denote compressive state; non-negative values are used for tensile state.
            double precision  :: stress_state = 1.D0
            
            !> Stress ratio
            double precision  :: rho = 0.D0 

      contains
      
            procedure,pass(this)    :: readConfig => UDSAModule_ReadConfig
            
            procedure,pass(this)    :: run => UDSAModule_run

            procedure,pass(this)    :: printConfig => UDSAModule_printConfig
            
      end type

contains

      integer function UDSAModule_ReadConfig(this,cnfunit) result(info)
      implicit none
      integer,intent(in)                        :: cnfunit
      class(UDSAModule),intent(inout)            :: this
      !
      integer :: ioerr
      logical :: input_ok = .false.
      !
            info = BasicModule_ReadConfig(this,cnfunit)
            if (info /= 0) return
            input_ok = .false.
            info = -1
            ! Read parameters specific for the UDSAModule program
            read(cnfunit,fmt=*,iostat=ioerr)  this%angle
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%scalingID, this%NormMax, this%NormIter
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%stress_state
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%rho
            if (ioerr /= 0) return
            ! Validate input
            select case(this%scalingID)
                  case(scaleFullTensor,scaleTensileComponent)
                        input_ok = .true.
            end select
            this%stress_state = merge(-1.D0,1.D0,(this%stress_state < 0.0))
            if ((ioerr /= 0) .or. (.not. input_ok)) return
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end function

      
      integer function UDSAModule_printConfig(this,outunit) result (info)
      implicit none
      class(UDSAModule),intent(in)         :: this
      integer,intent(in)                  :: outunit
      !
      character(len=32)       :: description
      !
            info = BasicModule_printConfig(this,outunit)
            if (info /= 0) return
            info = -1
            ! Introduce youself ;-)
            write(outunit,'(A)') 'UDSA, $Rev$'
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then 
                  if (this%stress_state > 0.0) then
                        description = 'uniaxial tensile'      
                  else
                        description = 'uniaxial compression'
                  endif
                  write(outunit,fmt=fmtMsg2Msg) 'Test type:', description
                  write(outunit,fmt=fmtMsg2Other//'F10.4)') 'Orientation of the sample:', this%angle
                  write(outunit,fmt=fmtMsg2Other//'F10.4)') 'Stress ratio:', this%rho
                  !
                  select case(this%scalingID)
                        case(scaleFullTensor)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling full tensor'
                        case(scaleTensileComponent)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling tensile component'
                        case default
                              write(outunit,*) 'Unknown scaling type, full tensor will be used'
                  end select
            endif
            info = 0
      !
      end function
      

      subroutine UDSAModule_Run(this,info)
      implicit none
      class(UDSAModule),intent(inout)            :: this
      integer,intent(out)                       :: info      
      ! Note about naming convention for variables:
      !    - All variables for vectors and tensors suffixed with _t are expressed in the "tensile sample coordinate system".
      !    - All other variables are implicitly expressed in the "material coordinate system"
      double precision,dimension(3,3)           :: P,D,De,De_t,Se,S,S_t, SmIdent, D_t, P_t
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: plast_pot, scal_s, norm_sona
      type(qrsData)                             :: qrsvalue = qrsData(0.D0, 0.D0, 0.D0), qrsvalue_accum = qrsData(0.D0, 0.D0, 0.D0)
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vDe,vSe
      double precision                          :: fi1,phi,fi2
      double precision                          :: Pnorm, normP, Tnorm
      double precision                          :: control_variable
      double precision                          :: scaling_factor
      double precision                          :: R
      double precision                          :: plastic_work_inc = 0.D0, plastic_work_total = 0.D0
      double precision                          :: taylor_factor
      integer     :: step,i,j 
      !
      integer,parameter       :: thisunit = 90, ofunit = 91, histunit = 92
      double precision,parameter :: dt = 1.D0 ! Time step length
      ! For file output
      integer,parameter :: ncolumn_labels = 14, column_width = 15, short_column_width = 7
      character(len=column_width),dimension(ncolumn_labels) :: file_column_labels = [ character(len=column_width) ::  &
           'iter','eps_vM','Pnorm','Tnorm','W','plast_pot','M','||SonA||','q-value','r-value','s-value', 'q-valueA', &
           'r-valueA','R'  ]
      ! For display output
      integer,parameter :: ncolumn_labels_display = 12, column_width_display = 12, short_column_width_display = 5
      character(len=column_width_display),dimension(ncolumn_labels_display) :: display_column_labels = [ character(len=14) ::  &
         'iter','eps_vM','Pnorm','Tnorm','W','plast_pot','M','||SonA||','q-value','r-value','s-value', 'R'    ]

      !
      info = 1
      !
      ! Open and initialize result files
      open(unit=ofunit,file=trim(this%output%outputPrefix)//'.uds',status='replace')
      write(ofunit,701) centered(1,short_column_width), (centered(i,column_width), i = 2, ncolumn_labels)
      write(ofunit,700) file_column_labels(1)(1:short_column_width), (centered(file_column_labels(i)), i=2,ncolumn_labels) 
      !
      open(unit=histunit,file=trim(this%output%outputPrefix)//'.hts',status='replace')
       
      fi1 = 0.0
      phi = 0.0
      ! Convert angle from degs to rads
      fi2 = deg2rad(this%angle)

      !---------------------------------------------------------------------------------
      ! Prepare the input data      
      !! -> Take uniaxial/slightly biaxial tensile stress, rotate it to given direction     
      S_t = 0.D0
      S_t(1,1) = this%stress_state * sqrt(3.D0/2.D0)/dsqrt(this%rho**2-this%rho+1)
      S_t(2,2) = this%rho*S_t(1,1)
            
      ! Rotate from "tensile" to material coordinate system
      ! Calculate rotation matrix
      Mrot = rotmat(fi1,phi,fi2)
      !
      S = rotateSRTensorTo(S_t,Mrot)
      !
      vS = tens2vec5D(S)
      ! Enforce unit length of vS
      vS = vS / vec_norm2(vS)
      !---------------------------------------------------------------------------------
      ! Initialize state & control variables
      control_variable = 0.D0
      !
      P_t = 0.D0
      Pnorm = 0.D0 ! sum||P||
      normP = 0.D0 ! ||P||
      Tnorm = 0.D0
      plastic_work_inc = 0.D0
      plastic_work_total = 0.D0
      step = 0
      do 
            if (doLogging(criLogDebug,this%output%verbosity))  write(display_unit,800)
            !
            !! -> Calculate corresponding strain rate vA
            call multilevelYLP(vS,vA,vSonA,R,info,.true.,this%ylp,verbose=this%output%verbosity)
            if (info /= 0) then
                  write(display_unit,fmt=960)
                  exit
            endif
            !
            plast_pot = dot_product(vA, vSonA)
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
            
            ! Rotate back to the "tensile test" coordinate system   
            D_t = rotateSRTensorFrom(D,Mrot)

            !
            ! Calculate control values
            !
            ! Calculate increment of plastic strain to be imposed for texture evolution: 
            ! There are two quantities to be calculated: vDe and De_t
            select case(this%scalingID)
            case(scaleFullTensor)
                  control_variable = normP
                  !! -> Scale the vA in order to get ||vA|| = NormIter
                  scaling_factor = (this%NormIter / vec_norm2(vA))
                  !
            case(scaleTensileComponent)
                  control_variable = abs(P_t(1,1))
                  !! -> Scale the D_t in order to get ||Dt_11|| equal to NormIter
                  scaling_factor = (this%NormIter / abs(D_t(1,1)))
            case default
                  write(display_unit,*) 'Unknown scaling type, full tensor will be used'
                  scaling_factor = 1.0
            end select
            !
            De = D * scaling_factor
            De_t = D_t * scaling_factor
            !            
            P_t = P_t + De_t
            !
            ! Calculate output variables
            !
            ! Calculate q and r in tensile reference frame
            qrsvalue = calculateQRS(D_t,scal_s)
            qrsvalue_accum = calculateQRS(P_t,scal_s)
            ! 
            taylor_factor = 0.D0
            call getTaylorFactor(1,taylor_factor,info)
            if (info /= 0) then
                  write(display_unit,fmt=980) 
                  exit
            endif
            !! -> Report the results
            write(ofunit,710) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
                              taylor_factor, & 
                              norm_sona, qrsvalue, qrsvalue_accum%qvalue, qrsvalue_accum%rvalue, R
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then
                  write(display_unit,601)
                  write(display_unit,600) display_column_labels(1)(1:short_column_width), & 
                                          (trim(display_column_labels(i)), i=2,size(display_column_labels)) 
                  write(display_unit,610) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
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
            ! Update the texture
            call makeTextureUpdateStep(De,Se,taylor_factor,this%output%outputRequest,info)
            if (info /= 0) then
                  write(display_unit,fmt=970) 
                  exit
            endif
            vSe = tens2vec5D(Se)
            vDe = tens2vec5D(De)
            !! -> Calculate total strain
            P = P + De
            normP = norm2(P)
            Pnorm = Pnorm + norm2(vDe)
            TNorm = abs(P_t(1,1))
            !Tnorm = Tnorm + abs(De(1,1))
            ! Calculate increment of plastic work (strain * deviatoric_stress)
            plastic_work_inc = dot_product(vDe,vSe) 
            plastic_work_total = plastic_work_total + plastic_work_inc
            ! Write history of deformations
            write(histunit,800)
            write(histunit,'(A,1X,I4)') 'Step:', step
            write(histunit,510)
            do j=1,3
                  write(histunit,511) De(:,j), Se(:,j)
            enddo
            write(histunit,'(A,T35,F12.8)') 'Increment of plastic work:', plastic_work_inc
            write(histunit,'(A,T35,F12.8)') 'Total of plastic work:', plastic_work_total
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
            !
            step = step + 1 
      enddo            
      !
      close(ofunit)
      close(histunit)

      info = 0
      
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

      ! Formats for the output file
      700 format(1X, 1(A7,1X),13(A18,  1X))
      701 format('#',1(A7,1X),13(A18,  1X))
      710 format(1X, 1(I7,1X),13(E18.9,1X))
          
          
#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

#ifdef OUTHEADER
#undef OUTHEADER
#endif      
      end subroutine
      
end module