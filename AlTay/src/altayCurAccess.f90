module altayCurAccess
    use altayDynfil
    use utils
    use criMathUtils

    implicit none

contains

    !> Write title line of the CUR format.
    subroutine CURwriteTitle(iounit, title, info)
        integer, intent(in)            :: iounit  !< IO unit number
        character(len=*), intent(in)   :: title !< Title line
        integer, intent(out)           :: info  !< exit code: 0 on success

        write(iounit, fmt='(A)',iostat = info) title

    end subroutine


    ! Write the current contents of the dynfil
    subroutine CURwriteBlock(iounit, info)
        integer, intent(in)      :: iounit !< IO unit number
        integer, intent(out)     :: info !< exit code: 0 on success
        integer:: npoint, i
        real(DP):: eu(3)

        npoint = size(DFIL)

        write (iounit, 402)
        write (iounit, 403) NRSTEP, npoint, deformation_gradient
        write (iounit, 401)

        do i = 1, npoint
            eu = rotation_matrix_to_euler_angles(DFIL(i)%tT)*RAD_TO_DEG
            write(iounit, 400, iostat = info)&
               i, DFIL(i)%tGEW, eu(1), eu(2), eu(3), DFIL(i)%tGAM
            if (info /= 0) exit
        enddo

 400 format (I6, f10.5, 2X, 3f10.5, 2X, f10.5)
 401 format (' CRYSTAL WEIGHT ',5X, 'phi1',6X, 'PHI',7X, 'phi2',6X, '  GAMMA')
 402 format (/,' Def. Step    ','Number of orientations',27X,          &
    2X, 'F(1, 1)',4X, 'F(2, 1)',4X, 'F(3, 1)',4X,                           &
    2X, 'F(1, 2)',4X, 'F(2, 2)',4X, 'F(3, 2)',4X,                           &
    2X, 'F(1, 3)',4X, 'F(2, 3)',4X, 'F(3, 3)')
 403 format(I6, 5X, i8, 41x, 3(2X, 3F10.6))

    end subroutine

end module
