! $Id$

    !> Test program for the Kocks-Mecking hardening module 'altayHardLawKM'.
    program TestProgram_altayHardLawKM
    use altayHardLaw_KM
    use altayCRSSTypes
    implicit none

    
    ! Variable declarations
    !> Unit number for parameter-file
    integer, parameter                :: iParFile= 22
    !> Unit number for output-file
    integer, parameter                :: iOutFile= 23
    !> Number of slip systems
    integer, parameter                :: nss= 12
    integer                           :: info= 0, i= 0
    logical                           :: WriteHeader= .false.
    logical                           :: WriteValues= .false.
    double precision                  :: delta_T= 0.0
    double precision, dimension(nss)  :: SlipRate= 0.0
    type(CRSSData)                    :: crss
    type(KMParameters)                :: Paraset
    type(KMStateVariables)            :: CurState, OldState, NewState
    type(KMStateDerivedVariables)     :: CurStateDerivedVars

    
    ! 1. By making an initialisation call, we retrieve a parameter set that is
    !    read from parameter input file. All parameters are supposedly  
    !    physically meanungfill, if not an error is flagged.
    open(unit=iParFile,file='Par.txt',status='old')  !open parameter file.
    info = KMParameters_init(Paraset, iParFile)
    if (info /= 0) then
         write(*,*)"Error initializing parameter set. Error code:", info
         pause
         stop
    endif 
    
    ! 2. Let's retrieve an initialised set of state variables into variable  
    !    CurState. Note: For Kocks-Mecking module, the 'initial' state 
    !    corresponds to the fully annealed condition.
    call KMStateVariables_init(CurState,ParaSet,info)
    if (info /= 0) then
        write(*,*)"Error initialising. Error code:", info
        pause
        stop
    endif
      
    ! 3. Opening an output text file, to write to it an output recording of 
    !    CurState (predeced by a header). 
    open(unit=iOutFile,file='out.txt',status='replace',iostat=info)
    WriteHeader= .true.
    WriteValues= .true.
    info = KMStateVariables_write(CurState, iOutFile, WriteHeader, WriteValues)
    if (info /= 0) then
        write(*,*)"Error writing output. Error code:", info
        pause
        stop
    endif
    write(iOutFile,*) "" !empty line
    
    ! 4. Next, we make 5 time increments, assuming (for simplicity) that the 
    !    slip rates (in array SlipRate) remain unchanged from one increment to
    !    the next. The known active slip systems are: 3, 4, 5, 11 and 12; 
    !    with corresponding slip rates:
    SlipRate(3) = 0.05 !unit: second^(-1)
    SlipRate(4) =-0.05
    SlipRate(5) =-0.10
    SlipRate(11)= 0.10
    SlipRate(12)= 0.15
    ! Note: the order of slip systems in SlipRate array follows the convention 
    ! of the DAT-file that is specified for altay simulation.
    !
    delta_T=0.1 !time increment. unit: second
    OldState= CurState
    do i=1,5  
        call KMStateVariables_update(NewState, OldState, ParaSet, SlipRate, &
                                     delta_T, info)
        if (info /= 0) then
            write(*,*)"Error updating state variables in increment ", i, &
                      ". Error code:", info
            pause
            stop
        endif
        CurState= NewState
        OldState= NewState
        !Output the state variable set after each increment 
        write(iOutFile,*) "After inc ", i
        WriteHeader= .false.
        WriteValues= .true.
        info = KMStateVariables_write(CurState, iOutFile, WriteHeader, &
                                      WriteValues) 
        if (info /= 0) then
            write(*,*)"Error writing output. Error code:", info
            pause
            stop
        endif
    enddo
    write(iOutFile,*) "" !empty line
    
    ! 5. For current state, the Critical Resolved Shear Stresses (crss) are 
    !    queried and outputted.
    call CRSSData_init(crss, nss, info) !initialization with 'nss' slip systems
    if (info /= 0) then
        write(*,*)"Error initialising crss", info
        pause
        stop
    endif
    call KMStateVariables_getCRSS(CurState, ParaSet, crss, info)
    if (info /= 0) then
        write(*,*)"Error querying crss. Error code:", info
        pause
        stop
    endif
    !Ad-hoc printout of crss to file :
    write(iOutFile,*)     "Here is a print-out of crss:"
    write(iOutFile,*)     "crss in positive sense [MPa]: "
    write(iOutFile,fmt=11) crss%crss(1,:)
    write(iOutFile,*)     "crss in negative sense [MPa]: "
    write(iOutFile,fmt=11) crss%crss(2,:)
    write(iOutFile,*) "" !empty line
11  format(12(f6.1,X)) !assuming 12 slip systems in this format specifier
    
    ! 6. For current state, extract a set of state-derived variables and print
    !    to file with header
    call KMStateDerivedVariables_calculate(CurStateDerivedVars, CurState, &
                                           ParaSet, info)
    if (info /= 0) then
        write(*,*)"Error extracting state-derived variables. Error code:", info
        pause
        stop
    endif   
    WriteHeader= .true.
    WriteValues= .true.
    info = KMStateDerivedVariables_write(CurStateDerivedVars, iOutFile, &
                                         WriteHeader, WriteValues)
    if (info /= 0) then
        write(*,*)"Error writing output. Error code:", info
        pause
        stop
    endif
    
    end program TestProgram_altayHardLawKM

