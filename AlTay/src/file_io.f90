module file_io
    use utils
    use logging
    use cluster_module
    use grain_module
    use altayConfig

    implicit none

    private
    public::    read_texture, &
                read_boundaries, &
                cur_write_title, &
                cur_write_block, &
                open_output_files

    character(*), parameter::   MOD_NAME = "file_io"
    integer, parameter:: IMP1=7

contains

    !Read list of grain orientations from file
    !@return list of Euler angle triplets (in radians) representing grain orientations
    function read_texture(fname) result(orientations)
        character(*), intent(in)::  fname

        integer::   nunit,  &
                    info,   &
                    nrec,   &
                    nstap,  &
                    i
        character(40):: title
        real(DP)::  stap,   &
                    weight, &
                    initial_sum_slip
        real(DP), dimension(:,:), allocatable:: orientations
        character(*), parameter:: PROC_NAME = 'load_texture'

        open(newunit = nunit, file = trim(fname), status='old',form='formatted',iostat = info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0
        read (nunit, 94, iostat = info) nrec, title
94      format(I5, 5x, A)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        if (nrec > 0) allocate(orientations(3, nrec))

        do i = 1, nrec
            read(nunit, 96, iostat = info) orientations(3, i), orientations(2, i), orientations(1, i), stap, nstap, weight, initial_sum_slip
96          format(4F10.0, I5, 5X, 2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
        enddo
        orientations = orientations/RAD_TO_DEG


        close(nunit)
    end function

    !Read grain boundaries from file
    !@return List of Euler angle triplets (in radians) representing boundary orientations
    function read_boundaries(file_name) result(boundaries)
        character(*), intent(in):: file_name
        real(DP), dimension(:,:), allocatable:: boundaries
        integer::   i,              &
                    file_handle,    &
                    n_boundaries
        character(len = 40)  :: TitMic !<Microstructure title

        !Read boundary orientations from file
        open (newunit = file_handle, file = file_name, status='old')
        read (file_handle, '(I5, 5x, A)') n_boundaries, TitMic  ! read number of grain boundaries and file title

        allocate(boundaries(3, n_boundaries))

        do i = 1, n_boundaries
            read (file_handle, '(3f10.0)') boundaries(3, i), boundaries(2, i), boundaries(1, i)  !read Euler angles from microstructure file in order: phi2, PHI, phi1
        enddo
        close(unit = file_handle)

        boundaries = boundaries/RAD_TO_DEG
    end function

    !> Write title line of the CUR format.
    subroutine cur_write_title(iounit, title, info)
        integer, intent(in)            :: iounit  !< IO unit number
        character(len=*), intent(in)   :: title !< Title line
        integer, intent(out)           :: info  !< exit code: 0 on success

        write(iounit, fmt='(A)',iostat = info) title
    end subroutine


    !Write the current state of all grains to file.
    subroutine cur_write_block(clusters, deformation_gradient)
        class(Cluster), dimension(:), intent(in)::  clusters !< List of all clusters which contains
                                                                                 !  state of all grains
        real(DP), dimension(3, 3), intent(in)::                         deformation_gradient

        integer:: n_grains, &
                  cluster_size, &
                  i, j, &
                  info
        real(DP):: euler_angles(3)

        !Calculate number of grains as number of clusters times number of grains per cluster
        cluster_size = size(clusters(1)%grains)
        n_grains = size(clusters) * cluster_size

        !Write general information
        write (IMP1, 402)
        write (IMP1, 403) n_grains, deformation_gradient
        write (IMP1, 401)

        !Write state of each grain
        do i = 1, size(clusters)
            do j = 1, cluster_size
                euler_angles = to_euler_angles(clusters(i)%grains(j)%orientation)*RAD_TO_DEG
                write(IMP1, 400, iostat = info)&
                    i*cluster_size+j, euler_angles(1), euler_angles(2), euler_angles(3)
                if (info /= 0) exit
            end do
        end do

         400 format (I6, 2X, 3f10.5)
         401 format (8X, 'phi1',6X, 'PHI',7X, 'phi2',6X, '  GAMMA')
         402 format (/,' Def. Step    ','Number of orientations',27X,          &
            2X, 'F(1, 1)',4X, 'F(2, 1)',4X, 'F(3, 1)',4X,                           &
            2X, 'F(1, 2)',4X, 'F(2, 2)',4X, 'F(3, 2)',4X,                           &
            2X, 'F(1, 3)',4X, 'F(2, 3)',4X, 'F(3, 3)')
         403 format(5X, i8, 41x, 3(2X, 3F10.6))

    end subroutine

    subroutine open_output_files(cnf, info)
        type(altayConfigData), intent(in)    :: cnf      !< configuration data
        integer, intent(out)                 :: info     !< exit code (altay_OK on success)

        character(len = fname_len):: fname_prefix, fname
        character(*), parameter:: PROC_NAME = 'openOutputFiles'

        fname_prefix = cnf%output_prefix
        info = VEF_ERROR

        if (cnf%output_config%nfile /= 0) then
            fname = trim(fname_prefix)//'.CUR'
            ! IMP1 = output file with successive "current situations"
            open (unit = IMP1, file = fname, status='replace',err = 9999)
        endif

        info = VEF_OK
        return

        9999 call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open file '//trim(fname))
    end subroutine
end module
