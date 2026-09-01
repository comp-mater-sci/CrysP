module relaxation_module
    use base_defs
    use math_utils
    use conversions
    use crysp_serialization

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
    type, extends(State):: Relaxation
        integer:: type    !< Type of the relaxation (see paper Van Houtte et. al.)
        real(DP), dimension(10):: taylor_coeffs  ! Defined in this way due to the structure of taylor_coeffs in
                                                                    !cluster type
        real(DP), dimension(6):: spin_coeffs  ! Defined in this way to be consistent with taylor_coeffs
    contains
        procedure:: size => relaxation_size
        procedure:: init => relaxation_init
        procedure:: update => relaxation_update
        procedure:: serialize => relaxation_serialize
        procedure:: deserialize => relaxation_deserialize
    end type

    public:: Relaxation

contains

    pure function relaxation_size(this) result(size)
        class(Relaxation), intent(in):: this
        integer:: size

        size = 3
    end function

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
            relaxation_crystal_frame = real(RELAXATIONS(:,:,this%type), DP) .toframe. boundary_to_crystal(:,:,i)
            !Invert direction of relaxations for second grain
            if (i == 2) relaxation_crystal_frame = -relaxation_crystal_frame
            !Rotational component of relaxation
            this%spin_coeffs(3*(i-1)+1:3*i) = tensor_to_spin(relaxation_crystal_frame)
            !Deviatoric component of relaxation
            this%taylor_coeffs(5*(i-1)+1:5*i) = tensor_to_deviatoric(relaxation_crystal_frame)
        end do
    end subroutine

    pure function relaxation_serialize(this) result(params)
        class(Relaxation), target, intent(in):: this
        type(Parameter), dimension(this%size()):: params

        params(1) = this%type
        params(2) = this%taylor_coeffs
        params(3) = this%spin_coeffs
    end function

    subroutine relaxation_deserialize(this, params)
        class(Relaxation), target, intent(out):: this
        type(Parameter), dimension(:), intent(in):: params

        this%type = params(1)
        this%taylor_coeffs = params(2)
        this%spin_coeffs = params(3)
    end subroutine
end module


