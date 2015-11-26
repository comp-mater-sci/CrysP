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
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
!use dmcUtils
use dmcBasicModule
use commonConfig
use commonUtils
use criMathUtils
use criPath
use criAlgorithm
use criLog
use criUncomment
use fngVec5D
implicit none

      type,extends(BasicModule) :: ASRModule
            type(EulerAngles)                         :: rotframe
            character(len=max_pathlen)                :: data_fname = ''
            logical                                   :: update_texture = .false.
            integer                                   :: scalingID = 0
            double precision                          :: NormMax = 0.D0
            double precision                          :: NormIter = 0.D0
      contains
      
            procedure,pass(this)    :: readConfig => ASRModule_ReadConfig
            
            procedure,pass(this)    :: run => ASRModule_run
            
      end type
      
      integer,parameter :: scaleStrainTensor = 0, scalePlasticWork = 1
     
contains

      integer function ASRModule_ReadConfig(this,cnfunit) result(info) 
      implicit none
      class(ASRModule),intent(inout)            :: this
      integer,intent(in)                        :: cnfunit
      !
      integer :: ioerr
      !
            info = BasicModule_ReadConfig(this,cnfunit) 
            if (info /= 0) return
            info = -1
            ! Read parameters specific for the ASRModule
            read(cnfunit,fmt=*,iostat=ioerr)  this%rotframe
            read(cnfunit,fmt='(A)',iostat=ioerr) this%data_fname
            call stripComment(this%data_fname)
            read(cnfunit,fmt='(L2)',iostat=ioerr) this%update_texture 
            read(cnfunit,fmt=*,iostat=ioerr)  this%scalingID, this%NormMax, this%NormIter
            if (ioerr /= 0) then
                  write(display_unit,fmt=902) 'ASRModule'
                  return
            endif
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end function


      subroutine ASRModule_Run(this,info)
      implicit none
      class(ASRModule),intent(inout)            :: this
      integer,intent(out)                       :: info      
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: D, De, Se, Sm, SonA, SmIdent, Pressure, P, TotalP
      double precision,dimension(3,3)           :: St, Stdev, StonA, StIdent, Dt
      double precision,dimension(3,3)           :: Mrot = 0.0, MI = 0.0
      !
      double precision                          :: plast_pot, scal_s, norm_sona, vS_norm
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vP, vTotalP, vDe, vSe

      double precision                          :: Pnorm, normP, normDe, totalPnorm
      double precision                          :: plastic_work_inc = 0.D0, plastic_work_total = 0.D0
      double precision                          :: R
      double precision                          :: taylor_factor
      double precision                          :: control_variable
      integer     :: i,j,point = 0, npoints = 0, increment = 0
      !
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91, histunit = 92, dtaunit = 93
      integer,dimension(2),parameter :: teeunits = [display_unit,histunit]
      
      integer,parameter :: ncolumn_labels = 35, column_width = 15, short_column_width = 7
      character(len=column_width),dimension(ncolumn_labels) :: file_column_labels = [ character(len=14) ::  &
            'point','incr','eps_vM','Pnorm','totalP_vM','W','plast_pot','M','scal_s','||SonA||','R', & 
            'SonA_11','SonA_22','SonA_33','SonA_12','SonA_23','SonA_13','A_11','A_22','A_33','A_12','A_23','A_13', & 
            'P_11','P_22','P_33','P_12','P_23','P_13', 'Ptot_11','Ptot_22','Ptot_33','Ptot_12','Ptot_23','Ptot_13' ]
      character(len=14),dimension(9) :: display_column_labels = [ character(len=14) ::  &
            'point','incr','eps_vM','Pnorm','totalP_vM','W','scal_s','||SonA||','R' ]
      !
      info = 1
      !
      MI = 0.D0
      do i=1,3
            MI(i,i) = 1.D0
      enddo
      !
      ! Introduce youself ;-)
      write(display_unit,'(A)') 'ASR, $Rev$'
      !
      ! Open input file
      open(unit=dtaunit,file=trim(this%data_fname),status='old',form='formatted',iostat=ioerr)
      if ( ioerr /= 0) then
            write(display_unit,*) 'Cannot open the data file: ', trim(this%data_fname)
            stop
      endif
      read(dtaunit,fmt='(I5)',iostat=ioerr) npoints
      if ((ioerr /= 0) .or. (npoints <= 0)) then
            write(display_unit,*) 'Wrong header of the data file'
            stop
      endif
      !
      ! Open and initialize result files
      open(unit=ofunit,file=trim(this%output%outputPrefix)//'.asr',status='replace')
      ! write header lines
      write(ofunit,701) (centered(i,short_column_width), i = 1,2), (centered(i,column_width), i = 3, ncolumn_labels)
      write(ofunit,700) (file_column_labels(i)(1:short_column_width), i=1,2), (centered(file_column_labels(i)), i=3,ncolumn_labels) 
      !
      open(unit=histunit,file=trim(this%output%outputPrefix)//'.hsr',status='replace')
      !      
      ! Calculate rotation matrix
      Mrot = rotmat(deg2rad(this%rotframe))
      !
      vTotalP = 0.D0
      TotalP = 0.D0
      totalPnorm = 0.D0
      plastic_work_inc = 0.D0
      plastic_work_total = 0.D0
      control_variable = 0.D0
      !
      do  point = 1, npoints
            write(display_unit,800)
            vP = 0.D0
            P = 0.D0
            Pnorm = 0.D0 ! sum||P||
            normP = 0.D0 ! ||P||
            !! Acquire full stress tensor St
            St = 0.D0
            read(dtaunit,*) ! Read separator line
            do j=1,3
                  read(dtaunit,*) St(:,j) ! to be symmetrized, so column/row does not matter.
            enddo
            ! Make sure the tensor is symmetrical
            St = 0.5*(St + transpose(St)) 
            Pressure = ((St(1,1) + St(2,2) + St(3,3))/3.0) * MI
            Stdev = St - Pressure
            !!! Print the input data:
            if (doLogging(criLogInfo,this%output%verbosity)) then
                  write(display_unit,'(A)') 'Input stress tensor, in the original reference frame'
                  write(display_unit,400) 'Total stress', 'Deviatoric', 'Pressure'
                  do j=1,3
                        write(display_unit,411) St(:,j),Stdev(:,j),Pressure(:,j)
                  enddo
            endif
            ! Rotate from the original reference frame to the superimposed coordinate system
            !
            Sm = matmul(transpose(Mrot),matmul(Stdev,Mrot))
            !            
            call KMAT2VEC5D(Sm,vS) 
            ! Enforce unit length of vS
            vS_norm = vec_norm2(vS)
            if (abs(vS_norm) < epsilon(0.D0)) then
                  write(display_unit,*) 'Norm of the stress cannot be zero, skipping'
                  cycle
            endif
            vS = vS / vS_norm
            ! Loop for evolution of texture            
            increment  = 1
            do 
                  call outputSeparator(teeunits)
                  !! -> Calculate corresponding strain rate vA
                  call multilevelYLP(vS,vA,vSonA,R,info,.true.,this%ylp,verbose=this%output%verbosity)
                  if (info /= 0) then
                        write(display_unit,fmt=960)
                        exit
                  endif
                  !
                  call getTaylorFactor(1,taylor_factor,info)
                  if (info /= 0) then
                        write(display_unit,fmt=980)
                        exit
                  endif
                  plast_pot = dot_product(vA, vSonA)
                  ! Calculate normalized stess
                  norm_sona = vec_norm2(vSonA)
                  scal_s = norm_sona / vS_norm
                  vSonAn = vSonA / vec_norm2(vSonA) 

                  ! Convert stresses and strain rates to [3x3] tensors
                  D = vec5D2tens(vA)
                  SonA = vec5D2tens(vSonA)
                  SmIdent = vec5D2tens(vSonAn)
                  !
                  ! Print vector form
                  if (doLogging(criLogDebug,this%output%verbosity)) call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
                  !
                  StonA = matmul(matmul(Mrot,SonA),transpose(Mrot))
                  StIdent = matmul(matmul(Mrot,SmIdent),transpose(Mrot))
                  Dt = matmul(matmul(Mrot,D),transpose(Mrot))
                  !! -> report the results to history file and to the screen
                  call outputIdentResults(teeunits)                  
                  !! -> Report the results to output file
                  write(ofunit,710) point, increment , root23*normP, Pnorm, root23*totalPnorm, &
                                    plastic_work_total, plast_pot, taylor_factor, scal_s, norm_sona, R, &
                                    Mat33ToVec6(StonA),Mat33ToVec6(D),Mat33ToVec6(P),Mat33ToVec6(TotalP)
                  if (doLogging(criLogInfo,this%output%verbosity)) then
                        ! write the header line
                        write(display_unit,610)
                        write(display_unit,600) (trim(display_column_labels(i)), i=1,size(display_column_labels)) 
                        write(display_unit,601) point, increment , root23*normP, Pnorm, root23*totalPnorm, &
                                                plastic_work_total, scal_s, norm_sona, R 
                        write(display_unit,610)
                  endif
                  !
                  ! Calculate strain increment; set a value for the control variable
                  select case(this%scalingId)
                  case(scaleStrainTensor)
                        !! -> Scale the vA in order to get ||vA|| = NormIter
                        vDe = vA * (this%NormIter / vec_norm2(vA))
                        control_variable = PNorm
                        continue
                  case(scalePlasticWork)
                        vDe = vA * (this%NormIter / plast_pot)
                        control_variable = plastic_work_total
                        continue
                  end select
                  ! Check termination condition: 
                  ! skip the rest if no texure update is requested, or if requested control threshold is exceeded.
                  if ((.not. this%update_texture) .or. (control_variable >= this%NormMax)) exit
                  !
                  normDe = vec_norm2(vDe)
                  if (doLogging(criLogInfo,this%output%verbosity)) write(display_unit,'(A,1X,F12.6)') 'Norm of vDe = ', normDe 
                  ! Calculate strain increment for texture evolution           
                  De = vec5D2tens(vDe)
                  !
                  !! -> Impose De as ALAMEL input, advance the state of texture
                  !
                  ! Write history of deformations
                  call outputImposedStrain(teeunits)
                  !
                  ! Update the texture
                  call makeTextureUpdateStep(De,Se,taylor_factor,this%output%outputRequest,info)
                  if (info /= 0) then
                        write(display_unit,fmt=970) 
                        exit
                  endif
                  call KMAT2VEC5D(Se,vSe)
                  ! Calculate increment of plastic work (strain * deviatoric_stress)
                  plastic_work_inc = dot_product(vDe,vSe) 
                  plastic_work_total = plastic_work_total + plastic_work_inc
                  !! -> Calculate total strain
                  vP = vP + vDe
                  vTotalP = vTotalP + vDe
                  P = vec5D2tens(vP)
                  TotalP = vec5D2tens(vTotalP) 
                  !
                  normP = vec_norm2(vP)
                  Pnorm = Pnorm + normDe
                  totalPnorm = vec_norm2(vTotalP)
                  increment = increment  + 1 
                  !
                  call outputStrainProgress(teeunits)
                  !
                  info = 0
            enddo
            if (info /= 0) exit                  
      enddo            
      !
      close(ofunit)
      close(histunit)
      
      info = 0
      
      400 format(A,T40,A,T80,A)
      
      410 format('| SmScaled',T40,'| SmIdent',T80,'|SonA')
      411 format(3(E12.5,1X),T40,'|',3(E12.5,1X),'|',T80,3(E12.5,1X))

      420 format('| Dm')
      421 format(3(E12.5,1X))

      500 format(3(3(E12.5,1X),/))
      501 format(3(E12.5,1X),/,3(E12.5,1X),/,3(E12.5,1X))
      ! Formats for output file
      700 format(1X, 2(A7,1X),9(A18,  1X),5X,24(A18,1X))
      701 format('#',2(A7,1X),9(A18,  1X),5X,24(A18,1X))
      710 format(1X, 2(I7,1X),9(E18.9,1X),5X,24(E18.9,1X))
      ! Formats for the display
      600 format(2(1X,A5),7(A14,1X))
      601 format(2(1X,I5),7(F14.6,1X))
      610 format(2('|',5('-')),'|',7(14('-'),'|'))
      ! Header of the file output
      !
#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

      
#ifdef OUTHEADER
#undef OUTHEADER
#endif      


      ! Internal subroutines
      contains
      
            subroutine outputIdentResults(teeunits)
            implicit none
            integer,dimension(:),intent(in) :: teeunits
            !
            integer :: j, n
                  do j = 1,size(teeunits)
                        n = teeunits(j)
                        write(n,'(A,1X,I3,1X,A,1X,I3)') 'Point:',point,'Increment:',increment 
                        write(n,'(A)') 'In the rotated reference frame:' 
                        call printIdentResultsT(n,Sm,SmIdent*vS_norm,SonA,D,info)
                        !
                        write(n,'(A)') 'In the original reference frame:' 
                        call printIdentResultsT(n,Stdev,StIdent*vS_norm,StonA,Dt,info)
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
      
end module
