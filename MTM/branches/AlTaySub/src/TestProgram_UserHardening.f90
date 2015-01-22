      program TestProgram_UserHardening
    
      use altayHardLaw_KocksMecking    
      implicit none
          
      integer :: i=0, info=-1
      integer, parameter :: iParFile=22, iOutFile=23, iSlipFile=24
      logical :: WithHeader = .false.
      
      double precision :: delta_T = 0.
      double precision, dimension(12) ::   sliprate = 0.
      double precision, dimension(2,12) :: CurCrss = 0.
      
      type(StatVar) :: CurState !A derived type defined in module, which contains all state variables of a single grain.
      type(StatVar) :: SV_inc_start, SV_inc_end
      
      type(StateDerivedVars) :: SDV
      
      !Initialization of module:
      open(unit=iParFile,file='Par.txt',status='old') !open parameter file.
      open(unit=iSlipFile,file='FCC.PRE',status='old') !open slip system file.
      info = InitModuleAltayHardLaw_KocksMecking(iParFile,iSlipFile)
      if (info /= 0) then
           write(*,*)"Error initializing module. Error code:", info
           pause
           stop
      endif 

      !Open the file 'out.txt' for output purposes.
      open(unit=iOutFile,file='out.txt',status='replace',iostat=info)
      if (info /= 0) then
           write(*,*)"Error opening 'out.txt'. Error code:", info
           pause
           stop
      endif
      
      !Let's first write the built-in header explaining about the meaning of state variable output.
      info = writeHeadSVfile(iOutFile)
      if (info /= 0) then
           write(*,*)"Error writing header of state variable output. Error code:", info
           pause
           stop
      endif            

      !Retrieve the set of state variables for an annealed state.
      call GetInitStatVar(CurState,info)
      if (info /= 0) then
           write(*,*)"Error retrieving annealed state. Error code:", info
           pause
           stop
      endif
      
      !Let's write this annealed state to 'out.txt'.     
      write(iOutFile,*) "" !empty line
      write(iOutFile,*) "The annealed state looks as follows:"
      info = writeSVfile(iOutFile,CurState)
      if (info /= 0) then
           write(*,*)"Error writing output. Error code:", info
           pause
           stop
      endif
      write(iOutFile,*) "" !empty line
      
      !Somehow, we know that the slip rates on the individual slip systems should be as follows:
      sliprate(1:12)=0.0
       !s.s. 3, 4, 5, 11 and 12 are active
      sliprate(3) = 0.05 !unit: second^(-1)
      sliprate(4) =-0.05
      sliprate(5) =-0.10
      sliprate(11)= 0.10
      sliprate(12)= 0.15
      
      !Now we make 5 time increments of 0.1second each, assuming these slip rates remain constant. 
      ! The state variables in 'CurState' will progressively be updated.
      delta_T=0.1 !time increment. unit: second
      SV_inc_start = CurState !set the state variables at start of 1st inc.
      do i=1,5 
        write(iOutFile,*) "Increment number ", i
        call MakeInc(SV_inc_start,sliprate,delta_T,SV_inc_end,info)
        !         IN: SV_inc_start, sliprate, delta_T
        !        OUT: SV_inc_end, info
        if (info /= 0) then
           write(*,*)"Error updating state variables. Error code:", info
           pause
           stop
        endif        
        CurState=SV_inc_end !update
        SV_inc_start=SV_inc_end !init. next inc.
      end do
      write(iOutFile,*) "" !empty line
      
      !Now that these 5 increments are done, lets write the new state to file
      write(iOutFile,*) "The current state looks as follows:"
      info = writeSVfile(iOutFile,CurState)
      if (info /= 0) then
           write(*,*)"Error writing output. Error code:", info
           pause
           stop
      endif
      write(iOutFile,*) "" !empty line
      
      !Critical Resolved Shear Stresses (crss) can be accessed directly in any StatVar:
      CurCrss = CurState%crss
      write(iOutFile,*)     "Here is a print-out of crss:"
      write(iOutFile,*)     "crss in positive sense [MPa]: "
      write(iOutFile,fmt=11) CurCrss(1,:)
      write(iOutFile,*)     "crss in negative sense [MPa]: "
      write(iOutFile,fmt=11) CurCrss(2,:)
      write(iOutFile,*) "" !empty line
      
      !Some state-derived variables can be extracted for a Particular state (in casu CurState):
      call GetStateDerivedVar(CurState,SDV,info)
      
      !The obtained SDV can be printed to file with accompanying header
      WithHeader = .true.
      write(iOutFile,*)"State-derived variables:"
      info = writeSDV(iOutFile,SDV,WithHeader)
      if (info /= 0) then
           write(*,*)"Error writing SDV to file using function writeSDV. Error code:", info
           pause
           stop
      endif
      write(iOutFile,*) "" !empty line
      
11    format(12(f6.1,X))
      
      end program TestProgram_UserHardening