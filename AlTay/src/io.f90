module io
    use definitions
    use logging
    use altayDynfil
    use criMathUtils

    implicit none
    private

    character(*), parameter ::  MOD_NAME = "io"


    public :: load_texture

    


contains

    subroutine load_texture(fname)
        character(*), intent(in)    :: fname
        integer                     :: nunit, info, nrec, nstap, i
        character(40) :: title
        character(*), parameter :: PROC_NAME = 'load_texture'
        real(DP) :: angles(3), stap, weight, gam
        type(grain), dimension(:), allocatable :: grains

        open(newunit=nunit,file=trim(fname),status='old',form='formatted',iostat=info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0 
        read (nunit, 94, iostat=info) nrec,title 
94      format(I5,5x,A)
        if (info /= VEF_OK) then
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        else if (nrec > 0) then 
            allocate(grains(nrec))
        end if
        
        do i = 1, nrec
            read(nunit,96,iostat=info) angles(3),angles(2),angles(1),stap,nstap,weight,gam
96          format(4F10.0,I5,5X,2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
            angles = angles * pi_deg
            grains(i) = grain(tgew=weight, tgam=gam, tt=rotmat(angles), ttax = mf%tax0, tZero = 0._DP)
        enddo
    
        close(nunit)
        
        call dynfil_init(grains)
    end subroutine
end module io
