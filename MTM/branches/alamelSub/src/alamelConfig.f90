!
! $Id$
!

module alamelConfig
implicit none

      integer,parameter  :: fname_len = 512

      type slipSystemData
            character(len=fname_len)                  :: slipsystem_fname    !< File containing definitions of slipsystems
            integer                                   :: ntau                !< Number of tau-crit values
            double precision,dimension(:),allocatable :: taucrit    
      end type

      type textureData
            !> Type of texture representaion: 1 - SMT, 2 - CUR, 3 - CUB
            integer                                   :: texture_type        
            character(len=fname_len)                  :: texture_fname
            integer                                   :: texture_block
      end type

      type simulData
            integer                                   :: output              !< Request for output
            integer                                   :: nsteps = 1          !< Number of steps per call
            integer                                   :: rlx1 = 1, rlx2 = 1  !< Selection of relaxations
            double precision, dimension(3,3)          :: dgf                 !< Deformation gradient tensor
      end type



contains

      subroutine initConfig()
      implicit none
      end subroutine

end module

