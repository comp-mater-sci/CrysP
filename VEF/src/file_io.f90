module file_io
    use conversions
    use logging
    use cluster_module
    use grain_module
    use macro

    implicit none

    private
    public:: MapItem, &
             tostring, &
             resolvename, &
             readvalue, &
             readKeyword, &
             read_texture, &
             read_boundaries, &
             cur_write_title, &
             cur_write_block, &
             open_output_files

    character, parameter::    COMMENT_SIGN = '#'
    character(*), parameter:: MOD_NAME = "file_io"
    integer, parameter::      IMP1 = 7
    integer, parameter::      CMAPNAMELEN = 32   !! Maximal length of strings that are used as keys in the map
    integer, parameter::      MAX_LINE_LEN = 512 !! Maximal length of a line

    !> Helper data structure for resolving name-identifier pairs
    !> MB: Maps (= structure arrays constructed from MapItem) are used to look up name and find corresponding ID and vice versa
    type MapItem
        character(len=CMAPNAMELEN):: name
        integer::                    id
    end type


      !> Read value from iounit and strip comments
      !> Arguments:
      !> \param[in] inunit The IO unit (type: integer)
      !> \param[out] val   The value being retrieved (type: one of the supported types
      !>                   (integer, logical, string, real(DP)) OR a vector of
      !>                   elements of supported types)
      !> \param[in] frmt  The format to be used in the read operation (type: character(len=*), optional)
      interface readValue
        module procedure read_scalar, read_array, read_tensor
      end interface




contains

    pure function toString(num) result(str)
        class(*), intent(in)    :: num
        character(12):: str

        select type(num)
            type is (integer)
                write (str, '(I0)') num
            type is (real(DP))
                write (str, '(G10.4)') num
        end select

        str = trim(str)
    end function

    !> Resolve symbolic name into an integer identifier.
    !>
    !> \return .true. if the name matches a name provided in the map, then id contains corresponding identifier
    !> \return .false. if the name doesn't match any map item, id and index (if present) are left unmodified.
    logical function resolveName(themap,name,id,index)
        character(len=*),intent(in)               :: name
        type(MapItem),dimension(1:),intent(in)    :: themap
        integer,intent(inout)                     :: id !MB: inout instead of only out since id only modified when name exists
        integer,intent(inout),optional            :: index !MB: inout instead of only out since index only modified when name exists
        !
        integer :: i
        character(len=cMapNameLen)       :: shortname
    !
        resolveName = .false.
        shortname = trim(adjustl(name)) ! Trim and store (make direct comparison)
        do i=1,size(themap) !MB: search for name in map
            if (shortname == trim(themap(i)%Name)) then
                id = themap(i)%ID
                resolveName = .true.
                exit
            endif
        enddo
        if (present(index) .and. resolveName) index = i !MB: store map index when name found
    end function

    logical function skipComment(nunit, buffer)
        integer, intent(in)::         nunit
        character(512), intent(out):: buffer

        logical:: next
        integer:: ios, &
                  hashidx

        next = .true.
        do while (next)
            read(nunit, fmt = 500, iostat = ios) buffer
            if (ios /= 0) then
                skipComment = .false.
                next = .false.
            endif
            if (.not. isComment(trim(adjustl(buffer))) ) then
                skipComment = .true.
                next = .false.
                ! sanitize output by removing '#'
                hashidx = index(buffer, comment_sign)
                if (hashidx /= 0) buffer(hashidx:) = ' '
            endif
        enddo
        500 format(A512)
    contains

        logical function isComment(buffer)
            character(len=*), intent(in)  :: buffer

            isComment = .false.
            if (len(buffer) > 0) then
                  if (buffer(1:1) == comment_sign) isComment = .true.
            endif
        end function
    end function

    logical function read_scalar(inunit, val) result(isOK)
        integer, intent(in):: inunit
        class(*), intent(out):: val

        character(MAX_LINE_LEN)   :: buffer
        integer:: ierr

        isOK = .false.
        if (skipComment(inunit, buffer)) then
            select type(val)
                type is (integer)
                    read(buffer, fmt=*,iostat = ierr) val
                type is (logical)
                    read(buffer, fmt=*,iostat = ierr) val
                type is (character(*))
                    read(buffer, fmt=*,iostat = ierr) val
                type is (real(DP))
                    read(buffer, fmt=*,iostat = ierr) val
            end select
        endif
        if (ierr == 0) isOK = .true.
    end function

    logical function read_array(inunit, val) result(isOK)
        integer, intent(in):: inunit
        real(DP), dimension(:), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        isOK = .false.
        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        if (ierr == 0) isOK = .true.
    end function
    logical function read_tensor(inunit, val) result(isOK)
        integer, intent(in):: inunit
        real(DP), dimension(3,3), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        isOK = .false.
        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        if (ierr == 0) isOK = .true.
    end function

    !> Read a keyword value and checks it against the map.
    !>
    !> \return
    !> If the keyword appears in the map, .true. is returned and the parameter value is set
    !> to the value associated to the keyword. Otherwise .false. is returned and value becomes undefined.
    logical function readKeyword(cnfunit,map,value) result(res)
        integer,intent(in)                        :: cnfunit
        type(MapItem),dimension(:),intent(in)     :: map
        integer,intent(out)                       :: value

        character(len=max_line_len) :: buffer

        value = 0
        res = .false.
        buffer = ''
        if (.not. readValue(cnfunit, buffer)) return
        res = resolveName(map, buffer, value)
    end function


    !Read list of grain orientations from file
    !@return list of Euler angle triplets (in radians) representing grain orientations
    function read_texture(fname) result(orientations)
        character(*), intent(in)::  fname

        integer::   nunit,  &
                    info,   &
                    nrec,   &
                    i
        real(DP), dimension(:,:), allocatable:: orientations
        character(*), parameter:: PROC_NAME = 'read_texture'

        open(newunit = nunit, file = trim(fname), status='old',form='formatted',iostat = info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0
        read (nunit, *, iostat = info) nrec
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        if (nrec > 0) allocate(orientations(3, nrec))

        do i = 1, nrec
            read(nunit, *, iostat = info) orientations(1, i), orientations(2, i), orientations(3, i)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read orientation')
        enddo

        orientations = deg_to_rad(orientations)

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

        boundaries = deg_to_rad(boundaries)
    end function

    !> Write title line of the CUR format.
    subroutine cur_write_title(iounit, title, info)
        integer, intent(in)            :: iounit  !< IO unit number
        character(len=*), intent(in)   :: title !< Title line
        integer, intent(out)           :: info  !< exit code: 0 on success

        write(iounit, fmt='(A)',iostat = info) title
    end subroutine

    !Write the current state of all grains to file.
    subroutine cur_write_block()
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
        write (IMP1, 403) n_grains
        write (IMP1, 401)

        !Write state of each grain
        do i = 1, size(clusters)
            do j = 1, cluster_size
                euler_angles = rad_to_deg(tensor_to_euler(clusters(i)%grains(j)%orientation))
                write(IMP1, 400, iostat = info)&
                    (i-1)*cluster_size+j, euler_angles(1), euler_angles(2), euler_angles(3)
                if (info /= 0) exit
            end do
        end do

         400 format (I6, 2X, 3f10.5)
         401 format (8X, 'phi1',6X, 'PHI',7X, 'phi2',6X, '  GAMMA')
         402 format (/,' Def. Step    ','Number of orientations',27X,          &
            2X, 'F(1, 1)',4X, 'F(2, 1)',4X, 'F(3, 1)',4X,                           &
            2X, 'F(1, 2)',4X, 'F(2, 2)',4X, 'F(3, 2)',4X,                           &
            2X, 'F(1, 3)',4X, 'F(2, 3)',4X, 'F(3, 3)')
         403 format(5X, i8)
    end subroutine

    subroutine open_output_files(prefix, nfile, info)
        character(len=fname_len), intent(in):: prefix
        integer, intent(in):: nfile

        integer, intent(out)                 :: info     !< exit code (altay_OK on success)

        character(len = fname_len):: fname
        character(*), parameter:: PROC_NAME = 'openOutputFiles'

        info = VEF_ERROR

        if (nfile /= 0) then
            fname = trim(prefix)//'.CUR'
            ! IMP1 = output file with successive "current situations"
            open (unit = IMP1, file = fname, status='replace',err = 9999)
        endif

        info = VEF_OK
        return

        9999 call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open file '//trim(fname))
    end subroutine

end module
