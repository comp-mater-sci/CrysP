module extendedYld
      
      double precision,parameter,private :: pi = acos(-1.D0)
      
      double precision, parameter         :: pi_deg = pi / 180.D0
      double precision, parameter         :: deg_pi = 180.D0 / pi
      
      type :: yldDesc
            double precision :: theta = 0.D0 !< In radians
            double precision :: S = 0.D0
            double precision :: Sx = 0.D0, Sy = 0.D0
            double precision :: beta = 0.D0 !< In radians
            ! Normal to the yield surface contour, pointing outwards
            double precision,dimension(2) :: vS = [0.D0, 0.D0]
      end type
      
      type(yldDesc),dimension(:),allocatable,save     :: ref_yld
      
      interface outputYldDescription
            module procedure outputYldDescription_points, outputYldDescription_range
      end interface
      
      ! penalty factors
      double precision,save :: penalty_mlt = 4.D0, penalty_angle = 0.5*pi
      
      ! regularization factor. It prevents the optimization method to propose
      ! solutions with excessively large norm. The factor should be small, otherwise
      ! it distorts the objective function and the search algorithms prefers solution
      ! with small norm. the Recommended values: from 0. to 0.001.
      double precision,save :: regularization_factor = 0.001
      
contains
      
      !> For a given angle theta, calculate stress S(theta) and the
      !> angle between the x-axis and the normal to the yield surface.
      !>
      !> The tangent to the yield surface is approximated by the secant
      !> on S(theta-dtheta), S(theta+dtheta)
      subroutine getYldPoint(point, info, dtheta)
      implicit none
      type(yldDesc),intent(inout)   :: point
      integer, intent(out)          :: info
      double precision,intent(in),optional :: dtheta !< In radians, default: 1 degree
      !
      ! 
      integer,parameter :: npoints = 3
      double precision,dimension(npoints) :: thetas 
      double precision,dimension(npoints,2) :: sigmas
      double precision,dimension(2) :: u
      double precision :: theta, deltatheta, sig12_frac, sig12, u_norm
      integer :: i
      !
            info = -1
            deltaTheta = 1.D0 * pi_deg
            if (present(dtheta)) deltatheta = dtheta
            ! Make the bracketing points 
            theta = point%theta
            thetas = [theta - deltatheta, theta, theta + deltatheta]
            sig12_frac = 0.D0
            do i = 1, npoints
                  call GET_S11_S22_S12(thetas(i), sig12_frac, sigmas(i,1), sigmas(i,2), sig12, info)
                  if (info /= 0) exit
            enddo
            if (info /= 0) return
            ! build the secant vector 
            point%vS = sigmas(3,:) - sigmas(1,:)
            ! build the normal vector: [-u_y, u_x]
            ! by clockwise rotation by 90degs
            point%vS = -[-point%vS(2), point%vS(1)]
            u_norm = norm2(point%vS)
            point%beta = 0.D0
            if (u_norm > epsilon(0.D0)) then
                  point%beta = acos(point%vS(1) / u_norm)
                  ! Let the vectors that point "downwards" have beta angle > 180deg
                  if (point%vS(2) < 0.D0) point%beta = 2.0*pi - point%beta
                  point%vS = point%vS / u_norm
            endif
            !
            point%Sx = sigmas(2,1)
            point%Sy = sigmas(2,2)
            point%S = norm2(sigmas(2,:))
            !
            info = 0
      end subroutine

      subroutine getYldBBCDescription(points, info, status)
      implicit none
      type(yldDesc),dimension(:),intent(inout) :: points
      integer,intent(out) :: info
      logical,dimension(:),optional :: status
      integer :: i
      !
      logical :: use_status
      !
            info = -2
            use_status = .false.
            if (present(status)) then
                  if (size(status) /= size(points)) return
                  use_status = .true.
                  status = .true.
            endif
            info = -1
            do i = 1, size(points)
                  call getYldPoint(points(i), info)
                  if (info /= 0) then
                        if (use_status) then
                              status(i) = .false.
                              cycle
                        endif
                        return
                  endif
            enddo
            info = 0
      !     
      end subroutine
      
      subroutine outputYldDescription_points(points, fpath, info)
      implicit none
      type(yldDesc),dimension(:),intent(in) :: points
      character(len=*),intent(in)           :: fpath
      integer,intent(out) :: info
      !
      integer :: i, yld_unit
      !
            open(newunit=yld_unit, file=fpath, status='replace', iostat=info)
            if (info /= 0) return
            do i =1, size(points)
                  associate (p=>points(i))
                  write(yld_unit, fmt='(7(F12.6,1X))') &
                        p%theta * deg_pi, p%Sx, p%Sy, p%S, p%vS, p%beta * deg_pi
                  end associate
            enddo
            close(yld_unit)
            info = 0
      end subroutine
      
      subroutine outputYldDescription_range(npoints, theta0, dtheta, fpath, info)
      implicit none
      integer,intent(in)                  :: npoints
      double precision,intent(in)         :: theta0, dtheta !< in radians
      character(len=*),intent(in)         :: fpath
      integer,intent(out) :: info
      !
      type(yldDesc),dimension(:),allocatable :: yld_points
      double precision :: theta
      integer :: i
      !
            info= -1
            if (npoints < 0) return
            ! Select the control points
            allocate(yld_points(npoints))
            theta = theta0
            do i = 1, npoints
                  yld_points(i)%theta = theta
                  theta = theta + dtheta
            enddo
            !
            call getYldBBCDescription(yld_points, info)
            if (info == 0) call outputYldDescription_points(yld_points, fpath, info)
      !
      end subroutine
      
      
      
      subroutine GET_IDENT_RSD_MOD (num_rsd_term, num_var, var, rsd, gbl_flag)
      implicit none
!*----------------------------------------------------------------------*
!* Parameters                                                           *
!*----------------------------------------------------------------------*
      integer ::  num_rsd_term, num_var, gbl_flag
      double precision :: var(16), rsd(16)
!*----------------------------------------------------------------------*
!* Variables                                                            *
!*----------------------------------------------------------------------*
      integer :: dbl_k, dbl_k_mns_one, i, j, lcl_flag
      double precision :: &
      y_term(8), r_term(8), u_ang(7), &
      bbc_parm(16), w, inv_w, w_mns_one, &
      w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, &
      f, g, r, s
!*----------------------------------------------------------------------*
!* Common blocks                                                        *
!*----------------------------------------------------------------------*
      common &
     &  /MAT_DATA/ y_term, r_term, u_ang, &
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one, &
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
      !
      
!*----------------------------------------------------------------------*
!* Check the validity of the call.                                      *
!*----------------------------------------------------------------------*
      if ((num_rsd_term /= 16) .or. (num_var /= 16)) then
        gbl_flag = -1
        return
      end if
!*----------------------------------------------------------------------*
!* Evaluate the residual-terms used to generate the error-function.     *
!*----------------------------------------------------------------------*
      do i = 1, 16
        bbc_parm(i) = var(i)
      end do
      lcl_flag = 0
      j = 1
      do i = 1, 7
            call GET_FG_U (u_ang(i), f, g, lcl_flag)
            if (lcl_flag /= 0) then
                  r = r_term(i) * penalty_mlt
                  s = y_term(i) * penalty_mlt
            else
                  r = f / g - 1.D0
                  s = 1.D0 / f
            endif
            rsd(j) = (1.D0 - s/(y_term(i)))**2
            rsd(j + 1) = (1.D0 - r / r_term(i))**2
            j = j + 2
      end do
      if (lcl_flag /= 0) then
        gbl_flag = -1
        return
      end if
      call GET_FG_B (f, g, lcl_flag)
      if (lcl_flag /= 0) then
            r = r_term(8) * penalty_mlt
            s = y_term(8) * penalty_mlt
      else
            r = f / g - 1.D0
            s = 1.D0 / f
      end if
      rsd(j) = (1.D0 - s/(y_term(8)))**2
      rsd(j + 1) = (1.D0 - r / r_term(8))**2
      return
      end subroutine GET_IDENT_RSD_MOD

      
      
      subroutine GET_IDENT_RSD_EXT (num_rsd_term, num_var, var, rsd, gbl_flag)
      implicit none
      integer ::  num_rsd_term, num_var, gbl_flag
      double precision :: var(num_var), rsd(num_rsd_term)
      !
      type(yldDesc),dimension(:),allocatable :: yld
      integer :: i, j, nyld, nterms_ext, info
      integer,parameter :: nterms = 16
      double precision,dimension(:),allocatable :: yld_res
      logical, dimension(:),allocatable :: yld_status
      !
            ! Note: gbl_flag should not be changed unless
            ! an error condition is detected.
            nyld = size(ref_yld)
            nterms_ext = 2* nyld + 1
            if ((num_rsd_term < num_var) .or. (num_var < nterms) .or. &
                (nterms + nterms_ext) /= num_rsd_term) then
                  gbl_flag = -1
                  return
            endif
            !
            call setBBC2008Params(var)
            !
            ! This call requires m=16 and n=16
            call GET_IDENT_RSD_MOD(nterms, nterms, var, rsd, gbl_flag)
            if (gbl_flag < 0) return
            !
            if (nyld > 0) then
                  allocate(yld(nyld), yld_status(nyld))
                  yld%theta = ref_yld%theta
                  call getYldBBCDescription(yld, info, yld_status)
                  if (info /= 0) then
                        ! Severe error
                        gbl_flag = -1
                        return
                  endif
                  ! Apply penalties 
                  where (.not.yld_status)
                        yld%S =  ref_yld%S * penalty_mlt
                        yld%beta =  ref_yld%beta + penalty_angle
                  endwhere
                  ! Calculate residuals: two per point in ref_yld
                  allocate(yld_res(2* nyld))
                  j = 1
                  do i = 1, size(yld_res), 2
                        yld_res(i) = (1.D0 - yld(j)%S / ref_yld(j)%S)**2
                        yld_res(i+1) =  (1.D0 - cos(ref_yld(j)%beta - yld(j)%beta))**2
                        j = j + 1
                  enddo
                  
                  rsd(nterms+1:) = yld_res
                  ! Regularization term: the norm of the solution vector should be minimized
                  rsd(num_rsd_term) = norm2(var) * regularization_factor
            endif
      !
      end subroutine
      ! <<--
      
      
end module