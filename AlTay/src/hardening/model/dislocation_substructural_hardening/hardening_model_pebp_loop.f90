module hardening_model_pebp_loop
    use utils
    use parameters
    use hardening_model_dsh
    

      IMPLICIT NONE


      PUBLIC :: HardeningModelPEBPLoop

      !NormDir(s,1:3): normalized slip plane normal vector of slip system s
       real(DP), dimension(24,3), parameter    ::  NORMDIR = reshape([ 0.D0,n2,p2,0.D0,p2,n2,0.D0,p2,n2,0.D0,n2,p2,pd6,n6,n6,nd6,p6,p6,nd6,p6,p6,pd6,n6,n6,    &
                                                                    p2,0.D0,n2,n2,0.D0,p2,p2,0.D0,n2,n2,0.D0,p2,n6,pd6,n6,p6,nd6,p6,n6,pd6,n6,p6,nd6,p6,    &
                                                                    n2,p2,0.D0,n2,p2,0.D0,n2,p2,0.D0,n2,p2,0.D0,n6,n6,pd6,n6,n6,pd6,n6,n6,pd6,n6,n6,pd6], shape(NORMDIR))


    !DSH model assuming slip is carried by dislocation loops with equal slip realized by edge and screw segments.
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
              if (abs(this%eff(s,i)) >= 0.99999_DP) then !treat as "1" or "-1"
                  this%eff(s,i)=0._DP
              else
                  this%eff(s,i)=sqrt(1._DP-(this%eff(s,i))**2)
              endif
            end do
          end do
        
            call this%initstate()
        end subroutine pebp_loop_init
        
END MODULE hardening_model_pebp_loop
