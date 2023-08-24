module hardening_model_pebp_screw
    use utils
    use parameters
    use hardening_model_dsh
    use slip_systems
    use criMathUtils
    
    IMPLICIT NONE
    

    PUBLIC    :: HardeningModelPEBPScrew

    !DSH model assuming all slip is carried by screw dislocations
    type, extends(HardeningModelDSH) :: HardeningModelPEBPScrew
    contains
        procedure :: init           => pebp_screw_init
    end type

      CONTAINS

        subroutine pebp_screw_init(this, params)
            class(HardeningModelPEBPScrew), intent(inout) :: this
            type(Parameter), allocatable, intent(in) :: params(:)
            real(DP) :: screwdir(24,3), &
                        prod(3)
            integer :: i

            !If NormSS(s,:) denotes slip plane normal vector and x the cross product, then:
            !ScrewDir(s,:) = EdgeDir(s,:) x NormSS(s,:)
            do i=1,24
                prod = cross(real(BCC24(:,2,i),DP), real(BCC24(:,1,i),DP))
                screwdir(i,:) = prod / norm2(prod)
            end do


            call dsh_init(this, params)
            this%eff = matmul(SCREWDIR, transpose(CBBNORMAL))
            call this%initstate()
        end subroutine pebp_screw_init


        
        
END MODULE hardening_model_pebp_screw
