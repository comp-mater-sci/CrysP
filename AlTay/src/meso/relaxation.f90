module relaxation_module
    use utils

    implicit none
    private

    !Definition of relaxations in boundary frame
    integer, dimension(3, 3, 2), parameter:: RELAXATIONS = reshape([0, 0, 0, &
                                                                            0, 0, 0, &
                                                                            1, 0, 0, &
                                                                            0, 0, 0, &
                                                                            0, 0, 0, &
                                                                            0, 1, 0], shape(RELAXATIONS))

    !Relaxation is only defined at cluster level even though its components are associated to individual grains because defining it
    !at grain level would require extending the grain type for different mesoscopic models, which would clash with possible
    !extensions of the grain type for microscopic models.
    type:: Relaxation
        integer:: type    !< Type of the relaxation (see paper Van Houtte et. al.)
        real(DP), dimension(10):: taylor_coeffs  ! Defined in this way due to the structure of taylor_coeffs in
                                                                    !cluster type
        real(DP), dimension(6):: spin_coeffs  ! Defined in this way to be consistent with taylor_coeffs
        real(DP):: slip_rate = 0._DP
    contains
        procedure:: init => relaxation_init
        procedure:: update => relaxation_update
        procedure:: get_strain => relaxation_get_strain
        procedure:: get_spin => relaxation_get_spin
    end type

    public:: Relaxation

contains

    subroutine relaxation_init(this, type)
        class(Relaxation), target, intent(inout):: this
        integer, intent(in):: type                      !< Type of relaxation (See paper Van Houtte et. al.)

        this%type = type
    end subroutine

    subroutine relaxation_update(this, boundary_to_crystal)
        class(Relaxation), target, intent(inout):: this
        real(DP), dimension(3, 3, 2), intent(in):: boundary_to_crystal !< Rotation matrices from the boundary frame to each of the
                                                                       !  crystal frames of the cluster.

        integer:: i
        real(DP), dimension(3, 3):: relaxation_crystal_frame

        do i = 1, 2
            relaxation_crystal_frame = rotate_to(real(RELAXATIONS(:,:,this%type), DP), boundary_to_crystal(:,:,i))
            !Invert direction of relaxations for second grain
            if (i == 2) relaxation_crystal_frame = -relaxation_crystal_frame
            !Rotational component of relaxation
            this%spin_coeffs(3*(i-1)+1:3*i) = -convert_spin(relaxation_crystal_frame)
            !Deviatoric component of relaxation
            this%taylor_coeffs(5*(i-1)+1:5*i) = convert_stress_strain_space(symmetric_part(relaxation_crystal_frame))
        end do
    end subroutine

    function relaxation_get_strain(this, ind_grain) result(strain)
        class(Relaxation), intent(in):: this
        integer, intent(in):: ind_grain !> Index of the grain in the cluster for which the strain must be calculated.
        real(DP), dimension(5):: strain

        strain = this%slip_rate*this%taylor_coeffs(5*(ind_grain-1)+1:5*ind_grain)
    end function

    function relaxation_get_spin(this, ind_grain) result(spin)
        class(Relaxation), intent(in):: this
        integer, intent(in):: ind_grain !> Index of the grain in the cluster for which the strain must be calculated.
        real(DP), dimension(3):: spin

        spin = this%slip_rate*this%spin_coeffs(3*(ind_grain-1)+1:3*ind_grain)
    end function

end module relaxation_module


