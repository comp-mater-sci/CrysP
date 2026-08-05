!> Several program components require a list of mixed-type parameters. Both the size of the list and the type of parameters are not
!> known at compile time. Therefore, some kind of serialization process is needed to flatten the parameter list into a byte stream
!> that can be passed around between higher-level program components.
!>
!> Only the component that created the byte stream has to know how to decode it.

module serialization
    use base_defs


    implicit none

    public

    !> Supported types for serialization.
    !>
    !> Each type has a unique Fortran and JSON representation.
    enum, bind(C)
        enumerator:: TYPE_INTEGER       !! Integer
        enumerator:: TYPE_REAL          !! Real(DP)
        enumerator:: TYPE_ANGLES_LIST   !! Real(DP), dimension(2,:)
    end enum

    type:: JSONParser
        private
        character(:), pointer:: json
        integer:: index
    contains
        procedure:: init => parser_init
        procedure:: next => parser_next
    end type

    interface to_json
        module procedure int_to_json, real_to_json, angles_list_to_json
    end interface

    interface assignment(=)
        module procedure parser_init, parse_int, parse_real, parse_angles_list
    end interface

contains

    subroutine parse_int(data, parser)
        integer, intent(out):: data
        type(JSONParser), intent(inout):: parser

        data = json_to_int(parser%next())
    end subroutine

    subroutine parse_real(data, parser)
        real(DP), intent(out):: data
        type(JSONParser), intent(inout):: parser

        data = json_to_real(parser%next())
    end subroutine

    subroutine parse_angles_list(data, parser)
        real(DP), dimension(:,:), allocatable, intent(out):: data
        type(JSONParser), intent(inout):: parser

        data = json_to_angles_list(parser%next())
    end subroutine

    subroutine parser_init(this, json)
        class(JSONParser), intent(out):: this
        character(:), target, intent(in):: json

        this%json => json
        this%index = 2
    end subroutine

    function parser_next(this) result(element)
        class(JSONParser), intent(inout):: this
        character(:), pointer:: element

        logical:: in_string
        integer:: i, i_start, i_end, &
                  depth

        i_start = this%index
        i_end = 0
        depth = 0
        in_string = .false.

        i=0
        do while (i_end == 0)
            select case (this%json(i_start+i:i_start+i))
                case ('"')
                    in_string = .not. in_string
                case ('[', '{')
                    if (.not. in_string) depth = depth + 1
                case (']', '}')
                    if (.not. in_string) then
                        if (depth == 0) then
                            i_end = i_start+i-1
                        else
                            depth = depth - 1
                        end if
                    end if
                case (',')
                    if (.not. in_string .and. depth == 0) i_end = i_start+i-1
            end select
            i=i+1
        end do

        element => this%json(i_start:i_end)
        this%index = i_end + 2
    end function



    function int_to_json(data) result(json)
        integer, intent(in):: data
        character(:), allocatable:: json

        !The standard does not define the size of default integer. For common 4-byte integers 11 should suffice, but let's be safe.
        character(16):: buffer

        write (buffer, '(I0)') data
        json = trim(buffer)
    end function

    function real_to_json(data) result(json)
        real(DP), intent(in) :: data
        character(:), allocatable :: json

        !Minimum size to hold any double precision, given we round to 9 digits, which conforms to TOLERANCE
        character(17) :: buffer

        write(buffer, '(ES17.9)') data
        json = trim(adjustl(buffer))
    end function

    function angles_list_to_json(data) result(json)
        real(DP), dimension(:,:), intent(in):: data
        character(:), allocatable:: json

        character(:), allocatable:: dir
        integer:: i,j

        json = '[]'
        do i=1,size(data,2)
            do j=1,2
                call json_list_add(dir, real_to_json(data(j,i)))
            end do
            call json_list_add(json, dir)
            deallocate(dir)
        end do
    end function

    subroutine json_list_add(list, element)
        character(:), allocatable, intent(inout) :: list
        character(*), intent(in) :: element

        character(:), allocatable :: tmp

        if (.not. allocated(list)) then
            list = '[' // element // ']'
        else if (list == '[]') then
            list = '[' // element // ']'
        else
            tmp = list(:len(list)-1) // ',' // element // ']'
            call move_alloc(tmp, list)
        end if
    end subroutine

    pure integer function json_list_size(json) result(n)
        character(*), intent(in) :: json

        integer :: i, depth
        logical :: in_string

        n = 0
        depth = 0
        in_string = .false.

        do i = 2, len(json)-1   ! skip '[' and ']'
            select case (json(i:i))
                case ('"')
                    in_string = .not. in_string
                case ('[', '{')
                    if (.not. in_string) depth = depth + 1
                case (']', '}')
                    if (.not. in_string) depth = depth - 1
                case (',')
                    if (.not. in_string .and. depth == 0) n = n + 1
            end select
        end do

        if (len_trim(json) > 2) n = n + 1
    end function json_list_size

    pure integer function json_to_int(json) result(data)
        character(*), intent(in):: json

        read (json, *) data
    end function
    pure real(DP) function json_to_real(json) result(data)
        character(*), intent(in):: json

        read (json, *) data
    end function
    function json_to_angles_list(json) result(data)
        character(*), target, intent(in):: json
        real(DP), dimension(:,:), allocatable:: data

        integer:: i,j, &
                  n_dirs
        character(:), pointer:: dir
        type(JSONParser):: iter_dirs, &
                         iter_dir

        n_dirs = json_list_size(json)

        allocate(data(2,n_dirs))

        call iter_dirs%init(json)
        do i=1,n_dirs
            dir => iter_dirs%next()
            call iter_dir%init(dir)
            do j=1,2
                data(j,i) = json_to_real(iter_dir%next())
            end do
        end do
    end function
end module
