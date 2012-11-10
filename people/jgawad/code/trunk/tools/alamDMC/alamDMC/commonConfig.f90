! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> This module contains subroutines, data structures and common variables 
!> for shared configuration features of all alamDMC programs
module commonConfig
use alamYLP
use alamUtils
use altayConfig, only: fname_len
use fngRange
use fngLinearMap
use fngNamedRange

      character(len=fname_len),save       :: outputPrefix = ''

      logical,save                  :: outputRequest = .false.

      type(multilevelYLPConfig),save:: ylpCnf

      character(len=20),parameter   :: fmtMsg2Msg   = '(A,T35,A)'
      character(len=20),parameter   :: fmtMsg2Int   = '(A,T35,I4)'
      character(len=20),parameter   :: fmtMsg2Float = '(A,T35,F12.8)'

      character(len=20),parameter   :: fmtMsg2Any   = '(A,T35)'
      character(len=20),parameter   :: fmtMsg2Other = '(A,T35,'  ! Note: user is responsible for finishing the format string      


      

contains

      subroutine readAlamelConfigSection(cnfunit,cnf,info)
      use altayConfig
      use altayHard, only: hard_none, hard_voce, hard_pebp
      implicit none
      integer,intent(in)                  :: cnfunit
      type(altayConfigData),intent(inout) :: cnf
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr, simtype, model_id
      !
            info = -1
            simtype = -1; ioerr = -1; model_id = -1
            !
            read(cnfunit,'(I2,1X,A)',iostat=ioerr) cnf%texture%input_type
            if (.not. ioStatusOK(ioerr)) return
            select case(cnf%texture%input_type)
                  case(1,3)     ! SMT or CUB
                        read(cnfunit,'(A)',iostat=ioerr) cnf%texture%input_fname
                  case(2)       ! CUR file    
                        read(cnfunit,'(I2,1X,A)',iostat=ioerr) cnf%texture%block_id, cnf%texture%input_fname
                  case default
                        write(*,*) 'Incorrect texture type: ', cnf%texture%input_type    
            end select
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(cnf%texture%input_fname)
            !
            read(cnfunit,fmt=*,iostat=ioerr)  simtype
            if (.not. ioStatusOK(ioerr)) return
            read(cnfunit,'(A)' ,iostat=ioerr) outputPrefix
            if (.not. ioStatusOK(ioerr)) return
            call stripComment(outputPrefix)
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%slipsystem%input_fname 
            call stripComment(cnf%slipsystem%input_fname)
            read(cnfunit,'(A)' ,iostat=ioerr) cnf%micros_fname
            call stripComment(cnf%micros_fname)
            read(cnfunit,'(L)' ,iostat=ioerr) outputRequest
            if (.not. ioStatusOK(ioerr)) return
            if (outputRequest)   cnf%output_config%nfile = 1
            !
            read(cnfunit,fmt=*,iostat=ioerr) cnf%slipsystem%kost
            if (.not. ioStatusOK(ioerr)) return
            select case(cnf%slipsystem%kost)
            case(hard_none)
                  ! no action needed
                  continue
            case(hard_Voce)
                  ! Read one line
                  read(cnfunit,fmt=*,iostat=ioerr) cnf%hardening%VoceCnf
            case(hard_pebp)
                  call readPEPBhardening(cnfunit,cnf%hardening%PEBPCnf,info)
                  if (info /= 0) return
                  if (outputRequest) then
                        cnf%output_config%npebp = 1
                        !cnf%output_config%nmss = 1 
                  endif
            case default
                  info = -1
                  return
            end select
            if (.not. ioStatusOK(ioerr)) return
            !
            cnf%output_prefix = trim(outputPrefix)
            !
            info = 0
            select case(simtype)
            case(0)     ! 0 - alamel
                  model_id = modelAlamel
            case(1)     ! 1 - FC Taylor
                  model_id = modelFCTaylor
            case(2)     ! 2 - MAS-Al
                  model_id = modelMASAL
            case default
                  info = -1
            end select
            if (info == 0) call setModelType(cnf,model_id,info)
      !
      end subroutine

      
      subroutine readPEPBhardening(cnfunit,hc,info)
      use altayConfig
      use KOST1x, only: ReadPar11
      implicit none
      integer,intent(in)                  :: cnfunit
      type(PEBPConfig),intent(out)        :: hc
      integer,intent(out)                 :: info
      !
      integer                       :: ioerr
      character(len=fname_len)      :: tmp_fname
      integer                       :: tmp,nparunit
      !
            info = -1
            read(cnfunit,fmt='(A)',iostat=ioerr) tmp_fname
            call stripComment(tmp_fname)
            ! Interpret the fname
            open(newunit=nparunit,file=tmp_fname,iostat=ioerr)
            if (ioerr /= 0) return
            info = ReadPar11(nparunit,hc%params)
            close(nparunit)
            if (info /= 0) return
            read(cnfunit,fmt='(I5,A)',iostat=ioerr) tmp, tmp_fname
            if (ioerr /= 0) return
            if (tmp >= 0) then
                  hc%read_state = .true.
                  call stripComment(tmp_fname)
                  hc%input_fname = tmp_fname
                  hc%block_id = tmp
            endif
            info = 0
      !
      end subroutine
      

      subroutine readYLPConfigSection(cnfunit,info)
      implicit none
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      integer                       :: ioerr
      !
            info = -1
            read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%jacobi_eps, ylpCnf%linearize
            read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%default_eps, ylpCnf%obj_func_eps
            if (ioStatusOK(ioerr)) info = 0
      end subroutine

      


      logical function ioStatusOK(ioerr)
      implicit none
      integer,intent(in) :: ioerr
            ! Status 
            if (ioerr /= 0) then
                  write(*,*) 'An error has occured while reading config file'
                  ioStatusOK = .false.  
            endif
            ioStatusOK = .true.
      end function

      !> Factory function that returns an instance appropriate range type depending on 
      !> the the input read from the  cnfunit IO unit
      function rangeFromConfig(cnfunit,info) result(inst)
      implicit none
      class(range_type),pointer     :: inst
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      !
      character(len=32) :: keyword
      type(bias_t),dimension(:),allocatable :: vBiases
      double precision :: rbegin, rend, ratio, rstep
      integer :: i, ierr, id, nranges, npoints
      !
            info = -1
            nullify(inst)
            keyword = ''
            id = -1
            ! Read the keyword
            read(cnfunit,*,iostat=ierr) keyword
            if (.not. ioStatusOK(ierr)) return
            if (resolveName(range_name_map,trim(keyword),id)) then
            
                  select case(id)
                  case(range_uniform_id)
                        ! Read: begin end step
                        read(cnfunit,*,iostat=ierr) rbegin, rend, rstep
                        if (.not. ioStatusOK(ierr)) return
                        allocate(uniformRange :: inst)
                        select type(inst)
                        type is (uniformRange)
                              inst = uniformRange(rbegin, rend, rstep)
                        end select
                  !
                  case(range_biased_id)
                        ! Read: begin end ratio
                        read(cnfunit,*,iostat=ierr) rbegin, rend, ratio, npoints
                        if (.not. ioStatusOK(ierr)) return
                        allocate(biasedRange :: inst)
                        select type(inst)
                        type is (biasedRange)
                              inst = biasedRange(rbegin, rend, ratio, npoints)      
                        end select
                        !
                  !
                  case(range_doublebiased_id)
                        ! Read: begin end ratio npoints
                        read(cnfunit,*,iostat=ierr) rbegin, rend, ratio, npoints
                        if (.not. ioStatusOK(ierr)) return
                        ! 
                        allocate(multiBiasedRange :: inst)
                        select type(inst)
                        type is (multiBiasedRange)
                              inst = centralBiasedRange(rbegin, rend, ratio, npoints)      
                        end select
                  !
                  case(range_multibiased_id)
                        ! Read: begin nranges
                        read(cnfunit,*,iostat=ierr) rbegin, nranges
                        if (.not. ioStatusOK(ierr)) return
                        ! Read: definitions of biases 
                        if (nranges > 0) then
                              allocate(vBiases(nranges))
                              do i = 1, nranges
                                    read(cnfunit,*,iostat=ierr) vBiases(i)
                                    if (.not. ioStatusOK(ierr)) return
                              enddo
                        endif
                        allocate(multiBiasedRange :: inst)
                        select type(inst)
                        type is (multiBiasedRange)
                              inst = multiBiasedRange(rbegin,vBiases)      
                        end select
                  !
                  end select
            endif
            
            info = 0
      !
      end function
      

      subroutine displayConfig(outunit,info)
      use altayConfig
      implicit none
      integer,intent(in)            :: outunit
      integer,intent(out)           :: info
      !
            info = -1
            
            select case (acnf%model_id)
            case(modelAlamel)
                  write(*,fmt=202) 'ALAMEL'
            case(modelFCTaylor)
                  write(*,fmt=202) 'FC Taylor'
            end select
            !
      
            ! Print configuration     
            select case(acnf%texture%input_type)
                  case(1)     ! SMT or CUB
                        write(outunit,fmt=200) 'SMT'
                  case(2)       ! CUR file    
                        write(outunit,fmt=200) 'CUR'
                  case(3)
                        write(outunit,fmt=200) 'CUB'                       
            end select
            write(outunit,fmt=201) trim(acnf%texture%input_fname)
            
            write(outunit,fmt=101) 'Prefix for output files:', trim(outputPrefix) 
            write(outunit,fmt=101) 'Slip systems definition:', trim(acnf%slipsystem%input_fname)
            !
            if (ylpCnf%linearize) then
                  write(*,100) 'Info: the program will first attempt to linearize the identification problems.'
            else
                  write(*,100) 'Info: The program will attempt to solve the nonlinear problems.'
            endif
            !
            info = 0
            !
            !!!!     
            100 format(/,A,/)
            101 format(A,T35,A)
            !
            200 format('Input texture format:', T35,A)
            201 format('Input texture file:', T35,A)
            202 format('Multilevel model:', T35,A)
      
      end subroutine


end module
