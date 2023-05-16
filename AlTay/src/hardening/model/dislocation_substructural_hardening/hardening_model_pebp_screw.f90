module hardening_model_pebp_screw
    use definitions
    use parameters
    use hardening_model_dsh
    
    IMPLICIT NONE
    
    integer :: i

    PUBLIC    :: HardeningModelPEBPScrew
      !>@{
            !ScrewDir(s,1:3): normalized movement vector of SCREW disl. on slip system s
      !  If NormSS(s,:) denotes slip plane normal vector and x the cross product, then:
      !      ScrewDir(s,:) = EdgeDir(s,:) x NormSS(s,:)
      real(dp), SAVE, DIMENSION(24,3)::ScrewDir
      DATA (ScrewDir(01,i),i=1,3) /nd6,p6,p6/ !s.s. 01
      DATA (ScrewDir(02,i),i=1,3) /p6,nd6,p6/ !s.s. 02
      DATA (ScrewDir(03,i),i=1,3) /p6,p6,nd6/ !s.s. 03
      DATA (ScrewDir(04,i),i=1,3) /pd6,n6,p6/ !s.s. 04
      DATA (ScrewDir(05,i),i=1,3) /n6,pd6,p6/ !s.s. 05
      DATA (ScrewDir(06,i),i=1,3) /n6,n6,nd6/ !s.s. 06
      DATA (ScrewDir(07,i),i=1,3) /nd6,n6,n6/ !s.s. 07
      DATA (ScrewDir(08,i),i=1,3) /p6,pd6,n6/ !s.s. 08
      DATA (ScrewDir(09,i),i=1,3) /p6,n6,pd6/ !s.s. 09
      DATA (ScrewDir(10,i),i=1,3) /pd6,p6,n6/ !s.s. 10
      DATA (ScrewDir(11,i),i=1,3) /n6,nd6,n6/ !s.s. 11
      DATA (ScrewDir(12,i),i=1,3) /n6,p6,pd6/ !s.s. 12
      DATA (ScrewDir(13,i),i=1,3) /0.,p2,n2/ !s.s. 13
      DATA (ScrewDir(14,i),i=1,3) /n2,0.,p2/ !s.s. 14
      DATA (ScrewDir(15,i),i=1,3) /p2,n2,0./ !s.s. 15
      DATA (ScrewDir(16,i),i=1,3) /0.,n2,n2/ !s.s. 16
      DATA (ScrewDir(17,i),i=1,3) /p2,0.,p2/ !s.s. 17
      DATA (ScrewDir(18,i),i=1,3) /n2,p2,0./ !s.s. 18
      DATA (ScrewDir(19,i),i=1,3) /0.,n2,p2/ !s.s. 19
      DATA (ScrewDir(20,i),i=1,3) /n2,0.,n2/ !s.s. 20
      DATA (ScrewDir(21,i),i=1,3) /p2,p2,0./ !s.s. 21
      DATA (ScrewDir(22,i),i=1,3) /0.,p2,p2/ !s.s. 22
      DATA (ScrewDir(23,i),i=1,3) /p2,0.,n2/ !s.s. 23
      DATA (ScrewDir(24,i),i=1,3) /n2,n2,0./ !s.s. 24


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


