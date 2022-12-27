!> This module contains the configuration parameters of IO operations
!> as well as other IO-related entities, such as IO unit numbers.
!>
!> \note The module replaces /ES/ and /ES1/ common blocks and a subset of /TEXTUR/ block.
module altayIOConfig
      implicit none
      !> Maximal length of any path (filenames, directrories etc.)
      integer,parameter :: pathlength = 512

      !>@{ \name IO units

      !> LEC= data set with slip systems
      integer :: LEC = 4

      !> KLEC= data set with parameters
      integer :: KLEC = 5

      !> IMP= printer
      integer :: IMP = 3

      !> IMP1=output-file with successive "current situations"
      integer :: IMP1 = 7

      !> IMP2=output-file with successive "responses to imposed strain"
      integer :: IMP2 = 8

      !> IMP3=output-file with twinning information
      integer :: IMP3 = 11

#ifdef PEBP_ENABLED
      !> IMP4= output file for state variables of KOST11
      integer :: IMP4 = 110
#endif

      !> IMP5= output of stress-strain or slip-stress
      integer :: IMP5 = 111

      !> IMP6= output of report file
      integer :: IMP6 = 112

      !> IDISK1= work file
      integer :: IDISK1 = 12

      !> NDAT1= input texture file
      integer :: NDAT1 = 9

      !> NDAT2= input microstructure file
      integer :: NDAT2 = 10

#ifdef PEBP_ENABLED
      !> IPEBPSTAT= input file for state variables of KOST11
      integer :: IPEBPSTAT = 60

      !> Output file for state-derived variables of KOST11
      integer :: IPEBPSDV = 61
#endif


      !>@}


      !>@{ \name Output control switches

      !> Control of the listing file (formerly in TEXTUR common block)
      !>
      !> This value controls amount of output sent to the IMP unit.
      integer :: NLIST = 0

      !> Printing switch (formerly in TEXTUR common block)
      integer :: IPR = 0

      !> Control of the result file
      !>
      !> This value control amount of output that is sent to IMP2 unit.
      integer :: NRES = 0

#ifdef PEBP_ENABLED
      !> Control of the state file in PEBP (KOST11 module)
      integer :: NPEBP = 0
#endif

      !> Control of the output with homogenized strain-stress (IMP5)
      integer :: NMSS = 0

      !>@}

end module
