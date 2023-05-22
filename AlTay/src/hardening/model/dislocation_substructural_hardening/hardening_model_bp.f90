module hardening_model_bp
    use definitions
    use parameters
    use hardening_model_dsh

    implicit none
    private

    !Normalized movement vector of EDGE disl. on slip system s
    !== normalized burgers vector of slip system s
    real(dp), dimension(24,3), parameter :: EDGEDIR = reshape([p3,p3,p3,n3,n3,n3,n3,n3,n3,p3,p3,p3,p3,p3,p3,n3,n3,n3,n3,n3,n3,p3,p3,p3, &
                                                               p3,p3,p3,n3,n3,n3,p3,p3,p3,n3,n3,n3,p3,p3,p3,n3,n3,n3,p3,p3,p3,n3,n3,n3, &
                                                               p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3,p3], shape(edgeDir))

    type, extends(HardeningModelDSH) :: HardeningModelBP
    contains
        procedure :: init           => bp_init
    end type

    public :: HardeningModelBP

contains
    
    subroutine bp_init(this, params)
        class(HardeningModelBP), intent(inout) :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        integer :: s,i

        call dsh_init(this, params)
        
        !Calculate "Wall-effectivity"-matrices
        do s=1,24
            do i=1,6
                this%eff(s,i) = DOT_PRODUCT(EdgeDir(s,:) , CBBnormal(i,:) )
            end do
        end do

        this%effslashb       = this%eff / this%b
        this%alfa_G_b = this%alfa * this%G * this%b
        this%alfa_G_b_eff    = this%alfa_G_b * this%eff
        this%alfa_G_b_ABSeff = abs(this%alfa_G_b_eff)
    end subroutine bp_init
end module hardening_model_bp
