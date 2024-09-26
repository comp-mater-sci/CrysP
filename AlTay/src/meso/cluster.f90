module cluster_module
    use utils
    use grain_module
    use relaxation_module
    use simplex

    implicit none

    integer, parameter::   INITIAL_BASIS_SYSTEMS_FCC(5) = [2, 5, 6, 7, 8], &
                           INITIAL_BASIS_SYSTEMS_BCC(5) = [1, 2, 4, 5, 7]

    type Cluster
        type(Grain), dimension(:), allocatable:: grains
        integer, dimension(:), allocatable:: ind_basis_systems
        real(DP):: weight                                    !> Measure of importance of the cluster with respect to the whole microstructure
        real(DP), dimension(3, 3):: boundary_reference_frame !> Rotation matrix for boundary frame in ACTIVE notation (for performance)
        real(DP), dimension(:,:), allocatable:: inverse_basis
        type(Relaxation), dimension(:), allocatable:: relaxations
        integer:: n_systems
    contains
        procedure:: init                            => cluster_init
        procedure:: get_imposed_strain_rate         => cluster_get_imposed_strain_rate
        procedure:: get_spin_coeffs_relaxations     => cluster_get_spin_coeffs_relaxations
        procedure:: set_slip_rates                  => cluster_set_slip_rates
        procedure:: get_taylor_coeffs               => cluster_get_taylor_coeffs
        procedure:: get_taylor_coeffs_relaxations   => cluster_get_taylor_coeffs_relaxations
        procedure:: update_relaxations              => cluster_update_relaxations
        procedure:: get_basis                       => cluster_get_basis
        procedure:: get_crss                        => cluster_get_crss
    end type

contains

    !Initializes cluster
    !@param this the cluster
    !@param orientations List of euler angle triplets (in radians) describing the orientations of the grains of this cluster
    !@param deformation_mechanism Miller indices of the slip system set used for all grains of this cluster
    !@param boundary Only passed for ALAMEL clusters. Contains triplet of Euler angles in radians describing the orientation of the
    !boundary between both grains of the cluster.
    subroutine cluster_init(this, orientations, deformation_mechanism, boundary, initial_deformation_gradient)
        class(Cluster), intent(inout):: this
        real(DP), dimension(:,:), intent(in):: orientations
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:), intent(in), optional:: boundary
        real(DP), dimension(:,:), intent(in), optional:: initial_deformation_gradient

        integer:: n_slip_systems_grain, &
                  start_systems, &
                  start_grain, &
                  cluster_size, &
                  i, j, k, &
                  ind_basis_systems_grain(5)

        n_slip_systems_grain = size(deformation_mechanism, 3)
        ind_basis_systems_grain = merge(INITIAL_BASIS_SYSTEMS_FCC, INITIAL_BASIS_SYSTEMS_BCC, n_slip_systems_grain == 12)

        !Temporary hack. Should move to FCTaylor and ALAMEL modules, respectively once they are available.
        if (present(boundary)) then  ! ALAMEL
            cluster_size = 2
            this%n_systems = 2*n_slip_systems_grain+2
            allocate(this%grains(2))
            allocate(this%ind_basis_systems(10))
            allocate(this%inverse_basis(10, 10), source = 0._DP)
            this%boundary_reference_frame = matmul(initial_deformation_gradient, transpose(from_euler_angles(boundary)))
            allocate(this%relaxations(2))
            this%ind_basis_systems(1:5) = ind_basis_systems_grain
            this%ind_basis_systems(6:10) = ind_basis_systems_grain+n_slip_systems_grain
            do i = 1, 2
                 call this%relaxations(i)%init(i)
            end do
        else  ! FCTaylor
            cluster_size = 1
            this%n_systems = n_slip_systems_grain
            allocate(this%grains(1))
            this%weight = 1._DP
            this%ind_basis_systems = ind_basis_systems_grain
        end if


        !Initialize each grain of the cluster.
        do i = 1, cluster_size
            call this%grains(i)%init(deformation_mechanism, orientations(:,i))
        end do

        this%inverse_basis = invert(this%get_basis())
    end subroutine

    pure function cluster_get_imposed_strain_rate(this, velocity_gradient) result(imposed_strain_rate)
        class(Cluster), intent(in):: this
        real(DP), dimension(3, 3), intent(in):: velocity_gradient
        real(DP), dimension(size(this%grains)*5):: imposed_strain_rate

        integer:: i

        do i = 1, size(this%grains)
            imposed_strain_rate((i-1)*5+1:i*5) = convert_stress_strain_space(velocity_gradient .toframe. this%grains(i)%orientation)
        end do
    end function

    function cluster_get_spin_coeffs_relaxations(this, ind_grain) result(spin_coeffs_relaxations)
        class(Cluster), intent(in):: this
        integer, intent(in):: ind_grain
        real(DP), dimension(3, 2):: spin_coeffs_relaxations

        integer:: i

        do i = 1, 2
            spin_coeffs_relaxations(:,i) = this%relaxations(i)%spin_coeffs((ind_grain-1)*3+1:ind_grain*3)
        end do
    end function

    function cluster_get_taylor_coeffs(this) result(taylor_coeffs)
        class(Cluster), intent(in):: this
        real(DP), dimension(5*size(this%grains), this%n_systems):: taylor_coeffs

        integer:: i, &
                  n_systems_grain

        taylor_coeffs = 0._DP

        do i = 1, size(this%grains)
            n_systems_grain = size(this%grains(i)%slip_systems)
            taylor_coeffs(5*(i-1)+1:5*i, n_systems_grain*(i-1)+1:n_systems_grain*i) = this%grains(i)%get_taylor_coeffs()
        end do

        if (size(this%grains)==2) then
            do i = 1, 2
                taylor_coeffs(:, this%n_systems-2+i) = this%relaxations(i)%taylor_coeffs
            end do
        end if
    end function

    function cluster_get_taylor_coeffs_relaxations(this, ind_grain) result(taylor_coeffs_relaxations)
        class(Cluster), intent(in):: this
        integer, intent(in):: ind_grain
        real(DP), dimension(5, 2):: taylor_coeffs_relaxations

        integer:: i

        do i = 1, 2
            taylor_coeffs_relaxations(:,i) = this%relaxations(i)%taylor_coeffs(5*(ind_grain-1)+1:5*ind_grain)
        end do
    end function

    function cluster_get_basis(this) result(basis)
        class(Cluster), intent(in):: this
        real(DP), dimension(size(this%ind_basis_systems), size(this%ind_basis_systems)):: basis

        integer:: i, &
                  n_systems_grain, &
                  ind_basis_system

        n_systems_grain = size(this%grains(1)%slip_systems)

        basis = 0._DP
        do i = 1, size(this%ind_basis_systems)
            ind_basis_system = this%ind_basis_systems(i)
            if (ind_basis_system <= n_systems_grain) then
                basis(1:5, i) = this%grains(1)%slip_systems(ind_basis_system)%taylor_coeffs
            else if (ind_basis_system <= 2*n_systems_grain) then
                basis(6:10, i) = this%grains(2)%slip_systems(ind_basis_system-n_systems_grain)%taylor_coeffs
            else
                basis(:,i) = this%relaxations(ind_basis_system-2*n_systems_grain)%taylor_coeffs
            end if
        end do
    end function

    function cluster_get_crss(this) result(crss)
        class(Cluster), intent(in):: this
        real(DP), dimension(2, this%n_systems):: crss

        integer:: i, &
                  n_systems_grain


        n_systems_grain = size(this%grains(1)%slip_systems)

        crss = 0._DP
        do i = 1, size(this%grains)
            crss(:,n_systems_grain*(i-1)+1:n_systems_grain*i) = this%grains(i)%get_crss()
        end do
    end function

    subroutine cluster_set_slip_rates(this, slip_rates)
        class(Cluster), intent(inout):: this
        real(DP), dimension(this%n_systems), intent(in):: slip_rates

        integer::   i, &
                    n_systems_grain

        do i = 1, size(this%grains)
            n_systems_grain = size(this%grains(i)%slip_systems)

            this%grains(i)%slip_systems%slip_rate = slip_rates((i-1)*n_systems_grain+1:i*n_systems_grain)

            if (size(this%grains) == 2) &
                this%relaxations(i)%slip_rate = slip_rates(size(slip_rates)-2+i)
        end do
    end subroutine

    subroutine cluster_update_relaxations(this, deformation_gradient)
        class(Cluster), intent(inout):: this
        real(DP), dimension(3, 3), intent(in):: deformation_gradient

        real(DP):: boundary_to_crystal(3, 3, 2), &
                   new_vec(10), &
                   dummy(10), &
                   new_boundary_frame(3, 3), &
                   basis(size(this%ind_basis_systems), size(this%ind_basis_systems))
        integer:: i, &
                  n_slip_systems_grain

        n_slip_systems_grain = size(this%grains(1)%slip_systems)


        !Calculate current boundary reference frame
        new_boundary_frame = matmul(deformation_gradient, this%boundary_reference_frame)
        new_boundary_frame(:,3) = new_boundary_frame(:,1) .cross. new_boundary_frame(:,2)
        new_boundary_frame(:,2) = new_boundary_frame(:,3) .cross. new_boundary_frame(:,1)
        new_boundary_frame = normalize(new_boundary_frame)

        !Transform relaxation from boundary frame to crystal frame
        !Composed of rotation from boundary to global frame and then from global to crystal frame.
        do i = 1, 2
            boundary_to_crystal(:,:,i) = matmul(this%grains(i)%orientation, new_boundary_frame)
        end do

        !For ALAMEL we may assume that the relaxations are part of the basis and they change with every time step. Therefore we
        !must always recalculate the inverse basis.
        do i = 1, 2
            call this%relaxations(i)%update(boundary_to_crystal)
        end do

        basis = this%get_basis()
        do i = 1, 10
            if (this%ind_basis_systems(i)> 2*n_slip_systems_grain) then
                new_vec = matmul(this%inverse_basis, basis(:,i))
                call update_inverse_basis(this%inverse_basis, new_vec, i, dummy)
            end if
        end do
    end subroutine
end module
