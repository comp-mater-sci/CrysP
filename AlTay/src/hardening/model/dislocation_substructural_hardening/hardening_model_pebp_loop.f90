module hardening_model_pebp_loop
    use hardening_model_dsh

      IMPLICIT NONE

    integer :: i

      PUBLIC :: HardeningModelPEBPLoop

      !NormDir(s,1:3): normalized slip plane normal vector of slip system s
      real(dp), SAVE, DIMENSION(24,3)::NormDir
      DATA (NormDir(01,i),i=1,3) /0.,p2,n2/ !s.s. 01
      DATA (NormDir(02,i),i=1,3) /n2,0.,p2/ !s.s. 02
      DATA (NormDir(03,i),i=1,3) /p2,n2,0./ !s.s. 03
      DATA (NormDir(04,i),i=1,3) /0.,n2,n2/ !s.s. 04
      DATA (NormDir(05,i),i=1,3) /p2,0.,p2/ !s.s. 05
      DATA (NormDir(06,i),i=1,3) /n2,p2,0./ !s.s. 06
      DATA (NormDir(07,i),i=1,3) /0.,p2,n2/ !s.s. 07
      DATA (NormDir(08,i),i=1,3) /p2,0.,p2/ !s.s. 08
      DATA (NormDir(09,i),i=1,3) /n2,n2,0./ !s.s. 09
      DATA (NormDir(10,i),i=1,3) /0.,n2,n2/ !s.s. 10
      DATA (NormDir(11,i),i=1,3) /n2,0.,p2/ !s.s. 11
      DATA (NormDir(12,i),i=1,3) /p2,p2,0./ !s.s. 12
      DATA (NormDir(13,i),i=1,3) /pd6,n6,n6/ !s.s. 13
      DATA (NormDir(14,i),i=1,3) /n6,pd6,n6/ !s.s. 14
      DATA (NormDir(15,i),i=1,3) /n6,n6,pd6/ !s.s. 15
      DATA (NormDir(16,i),i=1,3) /nd6,p6,n6/ !s.s. 16
      DATA (NormDir(17,i),i=1,3) /p6,nd6,n6/ !s.s. 17
      DATA (NormDir(18,i),i=1,3) /p6,p6,pd6/ !s.s. 18
      DATA (NormDir(19,i),i=1,3) /nd6,n6,n6/ !s.s. 19
      DATA (NormDir(20,i),i=1,3) /p6,pd6,n6/ !s.s. 20
      DATA (NormDir(21,i),i=1,3) /p6,n6,pd6/ !s.s. 21
      DATA (NormDir(22,i),i=1,3) /pd6,p6,n6/ !s.s. 22
      DATA (NormDir(23,i),i=1,3) /n6,nd6,n6/ !s.s. 23
      DATA (NormDir(24,i),i=1,3) /n6,p6,pd6/ !s.s. 24


    type, extends(HardeningModelDSH) :: HardeningModelPEBPLoop
    contains
        procedure :: init           => pebp_loop_init
    end type

      CONTAINS

        subroutine pebp_loop_init(this, config)
            class(HardeningModelPEBPLoop), intent(inout) :: this
            type(HardeningData), intent(in) :: config
            integer :: s,i

            call dsh_init(this,config)
              !Calculate "Wall-effectivity"-matrices
          do s=1,24
            do i=1,6
              eff(s,i)=DOT_PRODUCT( NormDir(s,:) , CBBnormal(i,:) )
              if (abs(eff(s,i)) >= 0.99999D0) then !treat as "1" or "-1"
                  eff(s,i)=0.0D0
              else
                  eff(s,i)=sqrt(1.0D0-(eff(s,i))**2)
              endif
            end do
          end do
            effslashb       = eff / P%b
      alfa_G_b= P%alfa* P%G * P%b
      alfa_G_b_eff    = alfa_G_b * eff
      alfa_G_b_ABSeff = ABS(alfa_G_b_eff)
        end subroutine pebp_loop_init
        
      END MODULE hardening_model_pebp_loop
