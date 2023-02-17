module hardening_model_bp
    use hardening_model_dsh

    IMPLICIT NONE

    integer :: i


    !EdgeDir(s,1:3): normalized movement vector of EDGE disl. on slip system s
    !               (it equals the normalized burgers vector of slip system s)
    real(dp), SAVE, DIMENSION(24,3)::EdgeDir
    DATA (EdgeDir( 1: 3,i),i=1,3) /3*p3,3*p3,3*p3/ !s.s. 1 to 3
    DATA (EdgeDir( 4: 6,i),i=1,3) /3*n3,3*n3,3*p3/ !s.s. 4 to 6
    DATA (EdgeDir( 7: 9,i),i=1,3) /3*n3,3*p3,3*p3/ !..
    DATA (EdgeDir(10:12,i),i=1,3) /3*p3,3*n3,3*p3/ !..
    DATA (EdgeDir(13:15,i),i=1,3) /3*p3,3*p3,3*p3/ !..
    DATA (EdgeDir(16:18,i),i=1,3) /3*n3,3*n3,3*p3/ !..
    DATA (EdgeDir(19:21,i),i=1,3) /3*n3,3*p3,3*p3/ !..
    DATA (EdgeDir(22:24,i),i=1,3) /3*p3,3*n3,3*p3/ !s.s. 21 to 24

    type, extends(HardeningModelDSH) :: HardeningModelBP
    contains
        procedure :: init           => bp_init
    end type

    public :: HardeningModelBP

CONTAINS
    
    subroutine bp_init(this, config)
        class(HardeningModelBP), intent(inout) :: this
        type(HardeningData), intent(in) :: config
        integer :: s,i

        call dsh_init(this,config)
        
        !Calculate "Wall-effectivity"-matrices
        do s=1,24
              do i=1,6
                eff(s,i)=DOT_PRODUCT(EdgeDir(s,:) , CBBnormal(i,:) )
              end do
            end do
        effslashb       = eff / P%b
        alfa_G_b= P%alfa* P%G * P%b
        alfa_G_b_eff    = alfa_G_b * eff
        alfa_G_b_ABSeff = ABS(alfa_G_b_eff)
    end subroutine bp_init
END MODULE hardening_model_bp
