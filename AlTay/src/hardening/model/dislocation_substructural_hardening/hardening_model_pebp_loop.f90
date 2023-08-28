module hardening_model_pebp_loop
    use utils
    use parameters
    use hardening_model_dsh
    use slip_systems
    

      IMPLICIT NONE


      PUBLIC :: HardeningModelPEBPLoop

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
            real(DP) :: normdir(24,3)
            

            normdir = transpose(normalize(BCC24(:,1,:)))

            call dsh_init(this, params)
              !Calculate "Wall-effectivity"-matrices
          do s=1,24
            do i=1,6
              this%eff(s,i)=DOT_PRODUCT(NormDir(s,:) , CBBnormal(i,:) )
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
