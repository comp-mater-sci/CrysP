module hardening_model_pebp_loop
    use definitions
    use parameters
    use hardening_model_dsh
    

      IMPLICIT NONE


      PUBLIC :: HardeningModelPEBPLoop

      !NormDir(s,1:3): normalized slip plane normal vector of slip system s
       real(dp), dimension(24,3), parameter    ::  NORMDIR = reshape([ 0.D0,n2,p2,0.D0,p2,n2,0.D0,p2,n2,0.D0,n2,p2,pd6,n6,n6,nd6,p6,p6,nd6,p6,p6,pd6,n6,n6,    &
                                                                    p2,0.D0,n2,n2,0.D0,p2,p2,0.D0,n2,n2,0.D0,p2,n6,pd6,n6,p6,nd6,p6,n6,pd6,n6,p6,nd6,p6,    &
                                                                    n2,p2,0.D0,n2,p2,0.D0,n2,p2,0.D0,n2,p2,0.D0,n6,n6,pd6,n6,n6,pd6,n6,n6,pd6,n6,n6,pd6], shape(NORMDIR))


    type, extends(HardeningModelDSH) :: HardeningModelPEBPLoop
    contains
        procedure :: init           => pebp_loop_init
    end type

      CONTAINS

        subroutine pebp_loop_init(this, params)
            class(HardeningModelPEBPLoop), intent(inout) :: this
            type(Parameter), allocatable, intent(in) :: params(:)
            integer :: s,i

            call dsh_init(this, params)
              !Calculate "Wall-effectivity"-matrices
          do s=1,24
            do i=1,6
              this%eff(s,i)=DOT_PRODUCT( NormDir(s,:) , CBBnormal(i,:) )
              if (abs(this%eff(s,i)) >= 0.99999D0) then !treat as "1" or "-1"
                  this%eff(s,i)=0.0D0
              else
                  this%eff(s,i)=sqrt(1.0D0-(this%eff(s,i))**2)
              endif
            end do
          end do
        this%effslashb       = this%eff / this%b
        this%alfa_G_b= this%alfa* this%G * this%b
        this%alfa_G_b_eff    = this%alfa_G_b * this%eff
        this%alfa_G_b_ABSeff = abs(this%alfa_G_b_eff)
        end subroutine pebp_loop_init
        
END MODULE hardening_model_pebp_loop
