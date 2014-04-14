      program KOST1xTest
    
      use KOST1x    
      use altayKOST1xState
      implicit none
          
      integer :: i, iError
      integer, parameter :: iParFile=22, iOutFile=23, iSlipFile=24
      integer, parameter :: KOST=11 !ID number of hardening model. 
      					!KOST=11 !original Peeters hardening model (edge disl. slip); Cf.  PhD thesis B. Peeters , MTM, KU Leuven, 2002.
      					!KOST=12 !modified: screw disl. slip
      					!KOST=13 !modified: slip by dislocation loops

      double precision :: delta_T
      double precision, dimension(24) ::   sliprate
      double precision, dimension(2,24) :: CurCRSS
      
      Type(StatVar) :: CurState !A derived type defined in module KOST1x, containing all state variables of a single grain.
      Type(StatVar) :: SV_inc_start, SV_inc_end
      
      Type(StateDerivedVars) :: SDV
      
      !Initialization of module:
      open(unit=iParFile,file='par.txt',status='old') !open parameter file.
      open(unit=iSlipFile,file='BCCBP.PRE',status='old') !open slip system file.
      iError = InitModuleKOST1x(iParFile,KOST,iSlipFile)
      if (iError /= 0) then
           write(*,*)"Error initializing module KOST1x. Error code:", iError
           stop
      endif 
      
      !Retrieve the state variables for an annealed state.
      call GetInitStatVar(CurState,iError)
        !        OUT: CurState  
      if (iError /= 0) then
           write(*,*)"Error retrieving annealed state. Error code:", iError
           stop
      endif
      
      !Let's write this annealed state to file.
      iError = KS_openStateFile(iOutFile,'out.txt','w')
      write(iOutFile,*) "The annealed state looks as follows:"
      iError = WriteSVfile(iOutFile,CurState)
      if (iError /= 0) then
           write(*,*)"Error writing output. Error code:", iError
           stop
      endif
      
      !Somehow, we know that the slip rates on the individual slip systems should be as follows:
      sliprate(1:24)=0.0
       !s.s. 3, 4, 5, 20 and 21 are active
      sliprate(3) =0.05 !unit: second^(-1)
      sliprate(4) =0.05
      sliprate(5) =0.10
      sliprate(20)=0.10
      sliprate(21)=0.15
      
      !Now we make 5 time increments of 0.1second each, assuming these slip rates remain constant. 
      ! The state variables in 'CurState' will progressively be updated.
      delta_T=0.1 !time increment. unit: second
      SV_inc_start = CurState !set the state variables at start of 1st inc.
      do i=1,5 
        write(iOutFile,*) "Increment number ", i
        call MakeInc(SV_inc_start,sliprate,delta_T,SV_inc_end,iError)
        !         IN: SV_inc_start, sliprate, delta_T
        !        OUT: SV_inc_end, iError
        if (iError /= 0) then
           write(*,*)"Error updating state variables. Error code:", iError
           stop
        endif        
        CurState=SV_inc_end !update
        SV_inc_start=SV_inc_end !init. next inc.
      end do
      
      !After these 5 increments, lets write the new state to file
      write(iOutFile,*) "The current state looks as follows:"
      iError = WriteSVfile(iOutFile,CurState)
      if (iError /= 0) then
           write(*,*)"Error writing output. Error code:", iError
           stop
      endif
      
      !Critical Reseolved Shear Stresses (CRSS) can be accessed directly in any StatVar:
      CurCRSS = 0. ! init.
      CurCRSS = CurState%CRSS
      Write(iOutFile,*)"Here is a print-out of CRSSs:"
      Write(iOutFile,*)"CRSS in positive sense: ", CurCRSS(1,:)
      Write(iOutFile,*)"CRSS in negative sense: ", CurCRSS(2,:)
      
      !Some state-derived variables can be extracted for a particular state (in casu CurState):
      call GetStateDerivedVar(CurState,SDV,iError)
      if (iError /= 0) then
           write(*,*)"Error extracting state-derived variables. Error code:", iError
           stop
      endif
      Write(iOutFile,*)     "These dislocation densities were extracted from the state: CurState"
      Write(iOutFile,fmt=10)" ..in the cell boundaries of the grain:                                 ", SDV%rho_CBs,    " /m^2"
      Write(iOutFile,fmt=10)" ..in the cell block boundaries of the grain:                           ", SDV%rho_CBBs,   " /m^2"
      Write(iOutFile,fmt=10)" ..of polarized dislocations at the cell block boundaries of the grain: ", SDV%rho_polCBBs," /m^2"
      Write(iOutFile,fmt=10)" ..in the grain (volume-average):                                       ", SDV%rho_avg,    " /m^2"

10    format(A72,e10.3,A5)
      
      end program KOST1xTest