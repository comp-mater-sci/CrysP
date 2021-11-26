!
! $Id$
!

!> Microstructure representation in AlTay
module altayMesostructure
use altayAlgorithms
use altayMiscutils, only: terminate, stopcode_runtimeerror
use altaySmtAccess
use criErrcodes
use criMathUtils
implicit none

    type :: srTensor
        
        double precision, dimension(3,3) :: matrix = unit_sr_matrix
    
    end type
    
    !> Description of inter-grain (inter/intra-phase) boundary (interface)
    type :: InterfaceData
        
        !> Euler angles of each interface in the dataset. 
        !> The corresponding 3rd axis refers to the interface normal.
        !> It has dimension(n_interfaces).
        type(EulerAngles) :: eulerangles
        
        !> Transformation matrix of each interface in the dataset. 
        !> The 3rd column refers to the interface normal.
        !> It has dimension(n_interfaces).
        type(srTensor)  :: trafo
        
    end type


    !> Wrapper for a pointer to InterfaceData object.
    type :: PtrInterfaceData
        type(InterfaceData),pointer     :: ptr => null()
    end type


    !> Description of mesostructure per a pair of phases.
    !> This also covers the where the two phases are identical (mesostructure
    !> of the phase itself)
    type :: MesostructureData
        
        !>Title of interface dataset
        character(len=40) :: title = ''

        type(InterfaceData),dimension(:), allocatable      :: interfaces
    end type


    !> Wrapper for a pointer to MesostructureData object
    type :: PtrMesostructureData
        type(MesostructureData),pointer :: ptr => null()
    end type
    
    interface initialize
        module procedure MesostructureData_readfromSMTfile
    end interface
    
      
contains

    !> Reading of "microstructure" (Euler angles defining 
    !> grain boundary segments) and initializing instance 'this' of InterfaceDataset.
    subroutine MesostructureData_readfromSMTfile( this, SMTfilename, info)
    use altayMiscutils
    use altayIOConfig
    implicit none
    type(MesostructureData), intent(out)    :: this
    character(len=*),intent(in)             :: SMTfilename
    integer, intent(out)                    :: info
    !
    type(RawFileContext) :: context
    type(SMTFileAccess) :: smt_access
    type(DiscreteODF) :: smt_data
    integer          :: i, ierr, n_interfaces
    !
        context%path = SMTfilename
        call smt_access%initialize(context, .true., info)
        if (info /= criSuccess) return
        info = smt_access%read(0,smt_data)
        n_interfaces = size(smt_data)
        if ((info /= criSuccess) .or. (n_interfaces <= 0)) return
        this%title = smt_data%title
        !
        allocate(this%interfaces(n_interfaces),stat=ierr)
        if (ierr /= 0) return
        !
        associate (odf => smt_data%orientations)
            do i= 1, n_interfaces
                this%interfaces(i)%eulerangles = odf(i)%euler
                ! Calculate the transformation matrix
                this%interfaces(i)%trafo%matrix = transpose(rotmat(odf(i)%euler))
            enddo
        end associate
        !
        info = criSuccess
    !
    end subroutine
    

      
      !> Calculation of a weight factor associated to the cluster, i.e.
      !>   - for Taylor: weight = 1.0
      !>   - for Alamel: weight is suspectedly given by the "Appendix A" in: 
      !>                   "P. Van Houtte et al, IJP 21, pp. 589-624 (2005), 
      !>                   doi: 10.1016/j.ijplas.2004.04.011",
      !>                   or a variant of the described algorithm.
      subroutine mesostr_clusterweightfactor(NGR,undeformedaxes,mesostructure_deformationgradient,weight,info) 
      use altayIOConfig, only: IPR,NLIST,IMP
      use altayMacroKinematic
      !
      implicit none
      integer,intent(in)                              :: NGR
      type(srTensor),intent(in)                       :: undeformedaxes
      double precision,dimension(3,3),intent(in)      :: mesostructure_deformationgradient 
      double precision,intent(out)                    :: weight
      integer,intent(out)                             :: info
      !
      double precision, dimension(3,3) :: deformedaxes
      double precision, dimension(3)   :: AL, AA
      double precision                 :: Vpar, u
      integer                          :: i
      integer, parameter               :: i1 = 1, i3 = 3            
      !
      !
      !
      !
      info = -1
      select case (NGR)
          case (1) !Taylor
              weight = 1.0D0
              info = 0
          case (2) !Alamel
              !
              deformedaxes = matmul(mesostructure_deformationgradient,undeformedaxes%matrix)
              !
              !Calculation of volume affected by the surface
              do i=1,3,1
                  AL(i) = vec_norm2(deformedaxes(i1:i3,i))
              enddo 
              !calculate volume of parallelepiped defined by the 3 column 
              ! vectors of deformedaxes, using dot & vector products
              Vpar = abs( dot_product( deformedaxes(i1:i3,1), &
                vector_product( deformedaxes(i1:i3,2) , deformedaxes(i1:i3,3) ) ) )
              !
              u = Vpar * 0.25D0 / (AL(1)*AL(2)*AL(3))
              !Note: The factor 0.25 is there so that for equiaxed grains, weight 
              !below becomes 1/3; for very flattened grains, it should tend to 1.
              ! 
              !re-order the basisvectors so that AA(1)>=AA(2)>=AA(3)
              !find out which one of these corresponds to the original AL(3)
              !Case 1----- is AL(3) the longest? 
              if (AL(2).le.AL(3).and.AL(1).le.AL(3)) then
                  AA(1)=AL(3)
                  if(AL(2).ge.AL(1))then
                      AA(2)=AL(2)
                      AA(3)=AL(1)
                  else
                      AA(2)=AL(1)
                      AA(3)=AL(2)
                  endif
                  weight=u*(2.0D0*(AA(2)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.0D0)
              else 
              !Case 2----- is AL(3) the shortest?
                  if (AL(3).le.AL(1).and.AL(3).le.AL(2)) then
                      AA(3)=AL(3)
                      if(AL(1).ge.AL(2))then
                          AA(1)=AL(1)
                          AA(2)=AL(2)
                      else
                          AA(1)=AL(2)
                          AA(2)=AL(1)
                      endif
                      weight=u*(4.D0*(AA(1)-AA(3))*(AA(2)-AA(3))*AA(3)  &
                              +2.0D0*(AA(2)-AA(3))*AA(3)**2+2.0D0*(AA(1)-AA(3))*AA(3)**2 &
                              +4.D0*AA(3)**3/3.D0)
                  else
                  !Case 3----- AL(3) is neither shortest nor longest      
                      AA(2)=AL(3)
                      if(AL(1).ge.AL(2))then
                          AA(1)=AL(1)
                          AA(3)=AL(2)
                      else
                          AA(1)=AL(2)
                          AA(3)=AL(1)
                      endif
                      weight=u*(2.D0*(AA(1)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.D0)
                  endif
              endif
              !
              if ((IPR.gt.0) .and. (NLIST.eq.1)) then
                  write (IMP,103) weight
              end if 
              103 format (/,' GEWF ',3d15.7,/) 
              !
              info = 0
              !
          case default
              !
              info = criErr_BadArgs
              return
              !
      end select
      !
      end subroutine               

      
      !> Calculate the transforation matrix for the cluster reference frame 
      !> (with respect to the macro reference frame), i.e.
      !>   - for Taylor: unity matrix.
      !>   - for Alamel: transformation matrix of the grain boundary reference
      !>                   frame, which has:
      !>                    (1) Its 3rd axis normal to the grain boundary.
      !>                    (2) Its 1st and 2nd axes such that the 2nd Alamel-type
      !>                        relaxation (in the local (2,3)-plane) is guaranteed
      !>                        orthogonal with respect to the currently imposed 
      !>                        macroscopic strain mode. Note that the 1st Alamel-type 
      !>                        relaxation can be parallel, orthogonal, or neither.
      subroutine mesostr_clustertrafo(NGR,undeformedaxes,MacroDefRate,mesostructure_deformationgradient,T_cluster,info) 
      use altayIOConfig, only: IPR,NLIST,IMP
      use criMathUtils
      use altayMacroKinematic
      !
      implicit none
      integer,intent(in)                              :: NGR
      type(srTensor),intent(in)                       :: undeformedaxes
      type(DeformationRate),intent(in)                :: MacroDefRate     
      double precision,dimension(3,3),intent(in)      :: mesostructure_deformationgradient      
      double precision,dimension(3,3),intent(out)     :: T_cluster
      integer,intent(out)                             :: info 
      !
      double precision, dimension(3,3) :: deformedaxes, orthoaxes, T_ortho, mode_local, T_phi 
      double precision                 :: x, Sphi, Cphi, phi
      double precision, parameter      :: epsi = 10.D0 * epsilon(x)
      integer                          :: i, j
      integer, parameter               :: i1 = 1, i3 = 3      
      !
      !
      !
      !
      info = -1
      select case (NGR)
      case (1) !Taylor
              !
              T_cluster = unit_sr_Matrix
              info = 0
              !
          case (2) !Alamel
              !
              deformedaxes = matmul(mesostructure_deformationgradient,undeformedaxes%matrix)
              !
              if ((IPR.gt.1) .and.(NLIST.eq.1)) then
                  write (IMP,409) 
                  409 format (' ') 
                  do i=1,3 
                        write (IMP,407) (undeformedaxes%matrix(j,i),j=1,3)
                  enddo
                  407 format (' TmatGr ',3d15.7)
                  do i=1,3 
                        write (IMP,408) (deformedaxes(j,i),j=1,3)
                  enddo
                  408 format (' deformedaxes  ',3d15.7)
              endif 
              !
              if ((IPR.gt.0) .and. (NLIST.eq.1) )then
                  write (IMP,100)
                  100  format (//,' CLUSTER1')
              end if
              !
              !1st axis (1st column) in orthoaxes is 1st axis of deformedaxes
              orthoaxes(i1:i3,1) = deformedaxes(i1:i3,1)
              !
              !3rd axis (3rd column) in orthoaxis is the vector product of 1st and 2nd axis of deformedaxes
              orthoaxes(i1:i3,3) = vector_product( deformedaxes(i1:i3,1) , deformedaxes(i1:i3,2) )
              !
              !2nd axis (2nd column) in orthoaxis is the vector product of 3rd and 1st axis of orthoaxes
              orthoaxes(i1:i3,2) = vector_product( orthoaxes(i1:i3,3) , orthoaxes(i1:i3,1) )
              !
              !Normalization of axes (columns) in orthoaxes
              do i=i1,i3
                  x = vec_norm2(orthoaxes(i1:i3,i))
                  if (abs(x) <= epsilon(x) ) then
                      info = criErr_NumNaN
                      return
                  end if
                  orthoaxes(i1:i3,i) = orthoaxes(i1:i3,i) / x
              enddo  
              !
              !The corresponding transformation matrix is given by the transpose
              T_ortho = transpose(orthoaxes)
              !           
              if ( (IPR.gt.0) .and. (NLIST.eq.1) ) then 
                  do i=1,3
                      write (IMP,102) (T_ortho(i,j),j=1,3)
                  end do
              end if
              102 format (' TGrb ',3d15.7)         
              !
              !Transform MacroDefRate%StrainModevM to the T_ortho reference frame
              mode_local = rotateSRTensorFrom(MacroDefRate%StrainModevM,T_ortho)
              !
              !Imposed shear deformation out of the grain boundary plane is
              ! found in the (1,3)- and (2,3)-components.
              Sphi=mode_local(2,3)
              Cphi=mode_local(1,3)
              !Note: Sphi^2 + Cphi^2 <= 1.0
              !
              if (abs(Sphi).lt.epsi.and.abs(Cphi).lt.epsi) then
                  !This need not trigger any exception.
                  !For any choice of phi, both relaxation will be orthogonal.
                  phi = 0.0D0
              else
                  phi = atan2(Sphi,Cphi)
              endif  
              !
              !T_phi is the additional transformation matrix to a cluster reference
              !frame for which the 2nd relaxation is allways perpendicular to the 
              !imposed strain mode.
              T_phi= 0.D0
              T_phi(1,1)= cos(phi)
              T_phi(2,2)= cos(phi)
              T_phi(3,3)= 1.D0
              T_phi(1,2)= sin(phi)
              T_phi(2,1)= -sin(phi)
              !
              !T_cluster is the transformation matrix T_ortho followed by T_phi       
              T_cluster = matmul(T_phi,T_ortho)
              !
          case default
              !
              info = criErr_BadArgs
              return
              !
      end select
      !
      end subroutine

      
      
      
end module
    
