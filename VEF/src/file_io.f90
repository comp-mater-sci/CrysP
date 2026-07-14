module file_io
    use conversions
    use logging
    use cluster_module
    use grain_module
    use macro

    implicit none

    private
    public:: MapItem, &
             to_string, &
             handle_iostat, &
             resolvename, &
             read_value, &
             readKeyword, &
             read_texture, &
             write_texture, &
             read_boundaries, &
             write_standard_header

    character, parameter::    COMMENT_SIGN = '#'
    character(*), parameter:: MOD_NAME = "file_io"
    integer, parameter::      CMAPNAMELEN = 32   !! Maximal length of strings that are used as keys in the map
    integer, parameter::      MAX_LINE_LEN = 512 !! Maximal length of a line
    integer,parameter ::      FMT_STRING_LENGTH = 128

    !> Helper data structure for resolving name-identifier pairs
    !> MB: Maps (= structure arrays constructed from MapItem) are used to look up name and find corresponding ID and vice versa
    type MapItem
        character(len=CMAPNAMELEN):: name
        integer::                    id
    end type


    !> Read value from iounit and strip comments
    !>
    !> Supported types: integer, logical, real(DP), character(*), real(DP)(:), real(DP)(:,:)
    interface read_value
        module procedure read_scalar, read_array, read_tensor
    end interface

    !> Convert a numerical value to a string
    !>
    !> Suported types: integer, real(DP), real(DP)(:)
    interface to_string
        module procedure to_string_int, to_string_real, to_string_real_arr
    end interface

contains

    !> Convert an integer to a string
    pure function to_string_int(number) result(string)
        integer, intent(in):: number        !! Number to be converted
        character(:), allocatable:: string  !! Resulting string. Of minimal length to contain the number.

        character(FNAME_LEN):: buffer

        write (buffer, '(I0)') number
        string = trim(buffer)
    end function
    !> See [[to_string_int]]
    pure function to_string_real(number) result(string)
        real(DP), intent(in):: number
        character(:), allocatable:: string

        character(FNAME_LEN):: buffer

        write (buffer, '(E12.4E3)') number
        string = trim(buffer)
    end function
    !> See [[to_string_int]]
    pure function to_string_real_arr(number) result(string)
        real(DP), dimension(:), intent(in):: number
        character(:), allocatable:: string

        character(FNAME_LEN):: buffer

        write (buffer, '(' // to_string(size(number)) // '(E12.4E3))') number
        string = trim(buffer)
    end function


    !> Generic handler for I/O errors.
    !>
    !> To be called by any I/O operation to handle the IOSTAT error code returned by the Fortran intrinsic I/O routines.
    !> On error, the error code and any value read are reported and the program is terminated.
    subroutine handle_iostat(caller, code, buffer)
        character(*), intent(in):: caller !! Name of the calling routine
        integer, intent(in):: code        !! IOSTAT code returned by Fortran intrinsic I/O procedure.
        character(*), intent(in), optional:: buffer !! In the case of a read operation, the value that was read.

        character(FNAME_LEN):: msg

        if (code == VEF_OK) return

        msg = 'IO error with code ' // to_string(code) // '. Refer to Fortran IOSTAT documentation.'
        if (present(buffer)) &
            msg = msg // ' Read value: ' // trim(buffer)
        call log_error(MOD_NAME, caller, ERR_IO, msg)
    end subroutine

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
            if (len(buffer) > 0) &
                isComment = (buffer(1:1) == comment_sign)
        end function
    end function

    !>  Read scalar value from config file and skip comments.
    !>
    !> Crashes the program on error.
    subroutine read_scalar(inunit, val)
        integer, intent(in):: inunit  !! The IO unit
        class(*), intent(out):: val   !! Variable to be read to.

        character(MAX_LINE_LEN)   :: buffer
        integer:: ierr

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
        call handle_iostat('read_scalar', ierr, buffer)
    end subroutine
    !> See [[read_scalar]]
    subroutine read_array(inunit, val)
        integer, intent(in):: inunit
        real(DP), dimension(:), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        call handle_iostat('read_array', ierr, buffer)
    end subroutine
    !> See [[read_scalar]]
    subroutine read_tensor(inunit, val)
        integer, intent(in):: inunit
        real(DP), dimension(3,3), intent(out):: val
        character(max_line_len)   :: buffer
        integer:: ierr

        if (skipComment(inunit, buffer)) read(buffer, fmt=*,iostat = ierr) val
        call handle_iostat('read_tensor', ierr, buffer)
    end subroutine

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
        call read_value(cnfunit, buffer)
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

        open(newunit = nunit, file = trim(fname), status='old',form='formatted',iostat = info)
        call handle_iostat('read_texture (open)',info)

        nrec = 0
        read (nunit, *, iostat = info) nrec
        call handle_iostat('read_texture (header)', info, to_string(nrec))

        if (nrec <= 0) &
            call log_error(MOD_NAME, 'read_texture', ERR_VAL, 'Nomber of orientations must be greater than 0.')

        allocate(orientations(3, nrec))
        do i = 1, nrec
            read(nunit, *, iostat = info) orientations(:,i)
            call handle_iostat('read_texture (orientations)', info, to_string(orientations(:,i)))
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
                    info,           &
                    file_handle,    &
                    n_boundaries

        !Open file
        open (newunit=file_handle, file=file_name, status='old', iostat=info)
        call handle_iostat('read_boundaries (open)', info)
        read (file_handle, *, iostat=info) n_boundaries  ! read number of grain boundaries and file title
        call handle_iostat('read_boundaries (header)', info, to_string(n_boundaries))

        allocate(boundaries(3, n_boundaries))

        do i = 1, n_boundaries
            read (file_handle, '(3f10.0)') boundaries(3, i), boundaries(2, i), boundaries(1, i)  !read Euler angles from microstructure file in order: phi2, PHI, phi1
        enddo
        close(unit = file_handle)

        boundaries = deg_to_rad(boundaries)
    end function

    subroutine write_texture(state_file_prefix, clusters)
        character(*), intent(in):: state_file_prefix
        class(Cluster), dimension(:), intent(in):: clusters

        integer:: i, j, &
                  state_unit, &
                  info

        open (newunit = state_unit, file = state_file_prefix//'.CUR', status='replace',err = 9999, iostat=info)

        do i=1,size(clusters)
            do j=1, size(clusters(i)%grains)
                write (state_unit, '(3f8.4)') rad_to_deg(tensor_to_euler(clusters(i)%grains(j)%orientation))
            end do
        end do

        close(state_unit)
        return
        9999 call log_error(MOD_NAME, 'write_texture', ERR_IO, 'Cannot open state file.' )
    end subroutine

    !> Write out standard header: two lines: #1: column numbers, #2 column names
    subroutine write_standard_header(iounit, column_names)
        integer,intent(in)                    :: iounit        !< Output IO unit
        character(*),dimension(:), intent(in) :: column_names  ! Names of columns

        character(FMT_STRING_LENGTH) :: fmt_string
        integer :: i, &
                   info, &
                   ncolumns

        ncolumns = size(column_names)
        ! Format: two leading spaces, followed by columns
        fmt_string = '(2X,'// to_string(ncolumns) // '(A,1X))'
        write(iounit,fmt=fmt_string,iostat=info) (column_names(i), i = 1, ncolumns)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'write_standard_header', ERR_IO, 'Could not write output file header')
    end subroutine
end module
