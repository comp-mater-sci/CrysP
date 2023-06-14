!> Container for miscellaneous utility routines.
module altayMiscutils
    use definitions
    implicit none
    !>@{ \name Exit codes that are returned to the OS on various stop contitions

    integer,parameter :: stopcode_OK = 0            !< OK, succsssful termination
    integer,parameter :: stopcode_inputerror = 1    !< Error, input parameters are wrong
    integer,parameter :: stopcode_ioerror = 2       !< Error, an IO operation has failed.
    integer,parameter :: stopcode_runtimeerror = 10 !< Run-time error condition occured.
    !>@}

    contains

    subroutine writeMSSHeader(ounit,info)
        integer,intent(in)      :: ounit
        integer,intent(out)     :: info

        write(ounit,fmt=554,iostat=info)
    554 format(T5,'Eps_vM',T21,'Eps_vM^Tot',T37,'GAMMA_H',T53,'GAMMA_H^Tot',T69,'Sigma_HvM',T90,'Sigma_11',&
               T106,'Sigma_22',T122,'Sigma_33',T138,'Sigma_23',T154,'Sigma_31',T170,'Sigma_12',T186,'TayFac_avg',T202,'StrnRatHet')

    end subroutine


    subroutine writeMSSRecord(ounit,meps,mepstot,hgamcall,hgamtot,shsam,mavg,srh,info)
        integer,intent(in)                      :: ounit
        real(dp),intent(in)                     :: meps,mepstot,hgamcall,hgamtot,mavg,srh
        real(dp),dimension(3,3),intent(in)      :: shsam
        integer,intent(out)                     :: info

        write(ounit,fmt=555,iostat=info) meps,mepstot,hgamcall,hgamtot,   &
        sqrt(3.D0/2.D0*sum(shsam*shsam)),                                     &
        shsam(1,1),shsam(2,2),shsam(3,3), shsam(2,3),shsam(3,1),shsam(1,2), &
        mavg,srh

    555  format(5(E15.6,1X),5X,8(E15.6,1X))

    end subroutine

end module
