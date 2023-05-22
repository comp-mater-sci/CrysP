module hardening_model_pebp_screw
    use definitions
    use parameters
    use hardening_model_dsh
    
    IMPLICIT NONE
    

    PUBLIC    :: HardeningModelPEBPScrew
            !ScrewDir(s,1:3): normalized movement vector of SCREW disl. on slip system s
      !  If NormSS(s,:) denotes slip plane normal vector and x the cross product, then:
      !      ScrewDir(s,:) = EdgeDir(s,:) x NormSS(s,:)
   real(dp), dimension(24,3), parameter    ::  SCREWDIR = reshape([nd6,p6,p6,pd6,n6,n6,nd6,p6,p6,pd6,n6,n6,0.D0,n2,p2,0.D0,p2,n2,0.D0,n2,p2,0.D0,p2,n2,    &
                                                                    p6,nd6,p6,n6,pd6,n6,n6,pd6,n6,p6,nd6,p6,p2,0.D0,n2,n2,0.D0,p2,n2,0.D0,p2,p2,0.D0,n2,    &
                                                                    p6,p6,nd6,p6,p6,nd6,n6,n6,pd6,n6,n6,pd6,n2,p2,0.D0,n2,p2,0.D0,p2,n2,0.D0,p2,n2,0.D0], shape(SCREWDIR))




    type, extends(HardeningModelDSH) :: HardeningModelPEBPScrew
    contains
        procedure :: init           => pebp_screw_init
    end type

      CONTAINS

        subroutine pebp_screw_init(this, params)
            class(HardeningModelPEBPScrew), intent(inout) :: this
            type(Parameter), allocatable, intent(in) :: params(:)
            integer :: s,i

            call dsh_init(this, params)
            
            !Calculate "Wall-effectivity"-matrices
            do s=1,24
                do i=1,6
                    this%eff(s,i)=DOT_PRODUCT( ScrewDir(s,:) , CBBnormal(i,:) )
                end do
            end do
            this%effslashb  = this%eff / this%b
            this%alfa_G_b = this%alfa * this%G * this%b
            this%alfa_G_b_eff = this%alfa_G_b * this%eff
            this%alfa_G_b_ABSeff = ABS(this%alfa_G_b_eff)
        end subroutine pebp_screw_init
        
        
END MODULE hardening_model_pebp_screw
