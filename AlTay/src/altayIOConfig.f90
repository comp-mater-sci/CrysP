!> This module contains the configuration parameters of IO operations
!> as well as other IO-related entities, such as IO unit numbers.
!>
module altayIOConfig
    implicit none
    !> Maximal length of any path (filenames, directrories etc.)
    integer,parameter :: pathlength = 512

    !>@{ \name IO units
    integer :: LEC = 4   !< data set with slip systems
    integer :: KLEC = 5  !< data set with parameters
    integer :: IMP = 3   !< printer
    integer :: IMP1 = 7  !< output-file with successive "current situations"
    integer :: IMP2 = 8  !< output-file with successive "responses to imposed strain"
    integer :: IMP3 = 11 !< output-file with twinning information

    integer :: IMP4 = 110     !< output file for state variables of KOST11
    integer :: IMP5 = 111     !< output of stress-strain or slip-stress
    integer :: IMP6 = 112     !< output of report file
    integer :: IDISK1 = 12    !< work file
    integer :: NDAT1 = 9      !< input texture file
    integer :: NDAT2 = 10     !< input microstructure file
    integer :: IPEBPSTAT = 60 !< input file for state variables of KOST11
    integer :: IPEBPSDV = 61  !< Output file for state-derived variables of KOST11
    !>@}

    !>@{ \name Output control switches
    integer :: NLIST = 0 !< This value controls amount of output sent to the IMP unit.
    integer :: IPR = 0   !< Printing switch (formerly in TEXTUR common block)
    integer :: NRES = 0  !< This value control amount of output that is sent to IMP2 unit.
    integer :: NPEBP = 0 !< Control of the state file in PEBP (KOST11 module)
    integer :: NMSS = 0  !< Control of the output with homogenized strain-stress (IMP5)
    !>@}

end module
