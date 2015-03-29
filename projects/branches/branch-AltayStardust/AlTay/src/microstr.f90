!> $Id$
      
!> Microstructure representation in AlTay
module altayMesostructure
use altayAlgorithms
use altayMiscutils, only: terminate, stopcode_runtimeerror
implicit none

      !> Transformation matrix associated to the grain boundary reference frame 
      !> in the initial state.
      !> Shape is: [3,3,ngr], where ngr is the number of grains.
      double precision, dimension(:,:,:),allocatable,save :: TmatGr
      integer,save :: NGrElm = 0
      character(len=40), save :: TitMic = ''

      
contains
      
      ! Reading of "microstructure" (Euler angles defining 
      ! grain boundary segments) in SMT-format, allocation 
      ! and assignment of the module variables.
      subroutine GRFIL(fnam,F_mic,ierr)
      use altayMiscutils
      use altayIOConfig
      implicit none
      !
      integer,intent(out)         :: ierr
      character(len=*),intent(in) :: fnam
      !F_mic is a deformation gradient that conceptually
      ! 'deforms' a spherical grain into an ellipsoidal shape
      double precision, dimension(3,3), intent(in) :: F_mic
      !
      integer          :: IGrElm
      type(EulerAngles) :: EulGB
      double precision, dimension(3,3) :: T 
      ! 
      ierr = -1
      !
      ! output to NLIST 
      if(NLIST.eq.1) write (IMP,103) fnam
      103  format (' GRFIL - Input Texture File:' ,a)
      !
      open (unit=NDAT2,file=fnam,status='old',iostat=ierr) 
      if (ierr /= 0) return
      !
      read (NDAT2,94) NGrElm,TitMic
      94  format(I5,5x,A)
#ifndef NO_STDOUT
      write (*,93) NGrElm,TitMic
#endif
      ! output to NLIST 
      if(NLIST.eq.1) write (IMP,93) NGrElm,TitMic
      93  format (' Number of orientations in MICROSTRUCTURE file:' ,I5,/,' Titel on  file: ',A)
    
      !
      allocate(TmatGr(3,3,NGrElm),STAT=ierr)
      if (ierr.ne.0) then
          if(NLIST.eq.1) write(IMP,102)
          return
      end if 
      102  format (' GRFIL - Allocation of memory failed')
      !
      do IGrElm=1,NGrElm
            read (NDAT2,96) EulGB%fi2,EulGB%PHI,EulGB%fi1
            !Calc. the transformation matrix T
            T = rotmat(deg2rad(EulGB))
            !TmatGr(1:3,i,IGrElm) for i=1,2 holds two non-parallel vectors 
            !  within the initial GB (grain boundary) plane.      
            !TmatGr(1:3,i,IGrElm) for i=3 holds a vector out of the initial
            !  GB plane (not necessarily perpendicular to the GB plane).
            TmatGr(:,:,IGrElm)=matmul(F_mic,transpose(T))
      enddo
      96 format(3F10.0)
      !
      close(unit=NDAT2)
      ierr = 0
      return
      end subroutine GRFIL


      !> Finalizes the module. The subroutine puts the module variables 
      !> into initial state and deallocates the storage.
      subroutine MICROSTR_finalize(info)
      implicit none
      integer,intent(out)     :: info
      !
            NGrElm = 0
            TitMic = ''
            if (allocated(TmatGr)) deallocate(TmatGr,stat=info)
      !
      end subroutine

      
      !> Calculation of a 'weight' factor associated to the volume of the cluster
      subroutine mesostr_clusterweightfactor(NGR,IGrElm,MacroDefState,GEWF,info) 
      use altayIOConfig, only: IPR,NLIST,IMP
      use altayMacroKinematic
    
      implicit none
      integer,intent(in)                              :: NGR
      integer,intent(in)                              :: IGrElm
      type(DeformationState),intent(in)               :: MacroDefState
      double precision,intent(out)                    :: GEWF
      integer,intent(out)                             :: info
      !
      double precision :: GRPAR(3,3),x,vec1(3),AL(3),u,AA(3)
      integer :: i,j
      !
      !
      !
      !
      info = -1
      select case (NGR)
          case (1) !Taylor
            GEWF = 1.0D0
            info = 0
          case (2) !Alamel
            GRPAR = matmul(MacroDefState%TotalDefGrad,TmatGr(:,:,IGrElm))
            !     Calculation of volume affected by the surface
            do i=1,3,1
                  x=0.0
                  do j=1,3,1 
                        X=X+GRPAR(j,i)**2
                  enddo
                  AL(i)=sqrt(X)
            enddo 
            !     Box product
            vec1(1)=GRPAR(2,2)*GRPAR(3,3)-GRPAR(3,2)*GRPAR(2,3)
            vec1(2)=GRPAR(3,2)*GRPAR(1,3)-GRPAR(1,2)*GRPAR(3,3)            
            vec1(3)=GRPAR(1,2)*GRPAR(2,3)-GRPAR(2,2)*GRPAR(1,3)
            u=0.0D0
            do i=1,3
                  u=u+GRPAR(i,1)*vec1(i)
            enddo
            u=abs(u)*0.25D0/(AL(1)*AL(2)*AL(3))
            !     The factor 0.25 is there so that for equiaxed grains, GEWF below becomes 1/3;
            !      for very flattened grains, it should tend to 1.
            ! 
            !     re-order the basisvectors so that AA(1)>=AA(2)>=AA(3)
            !     find out which one of these corresponds to the original AL(3)
            !     Case 1: is AL(3) the longest? 
            if (AL(2).le.AL(3).and.AL(1).le.AL(3)) then
                  AA(1)=AL(3)
                  if(AL(2).ge.AL(1))then
                        AA(2)=AL(2)
                        AA(3)=AL(1)
                  else
                        AA(2)=AL(1)
                        AA(3)=AL(2)
                  endif
                  GEWF=u*(2.0D0*(AA(2)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.0D0)
            else 
                  !       Case 2: is AL(3) the shortest?
                  if (AL(3).le.AL(1).and.AL(3).le.AL(2)) then
                        AA(3)=AL(3)
                        if(AL(1).ge.AL(2))then
                              AA(1)=AL(1)
                              AA(2)=AL(2)
                        else
                              AA(1)=AL(2)
                              AA(2)=AL(1)
                        endif
                        GEWF=u*(4.D0*(AA(1)-AA(3))*(AA(2)-AA(3))*AA(3)  &
                              +2.0D0*(AA(2)-AA(3))*AA(3)**2+2.0D0*(AA(1)-AA(3))*AA(3)**2 &
                              +4.D0*AA(3)**3/3.D0)
                    else
                        !         Case 3: AL(3) is neither shortest nor longest      
                        AA(2)=AL(3)
                        if(AL(1).ge.AL(2))then
                              AA(1)=AL(1)
                              AA(3)=AL(2)
                        else
                              AA(1)=AL(2)
                              AA(3)=AL(1)
                        endif
                        GEWF=u*(2.D0*(AA(1)-AA(3))*AA(3)**2+4.D0*AA(3)**3/3.D0)
                    endif
            endif
            if ((IPR.gt.0) .and. (NLIST.eq.1)) then
                  write (IMP,103) GEWF
            end if 
103         format (/,' GEWF ',3d15.7,/) 
            info = 0
      end select
      !
      end subroutine               

      
            
      subroutine CLUSTER1(NGR,IGrElm,MacroDefRate,MacroDefState,Tprinc) 
      !   IF both relaxations are orthogonal:
      !      Cofcos=0 and Cofsin=0 is returned
      !   ELSE:
      !      Cofcos and Cofsin are the cosine and sine of the angle for relaxation-1
      !
      !   relaxation-2 is always the orthogonal one.
      !   TDC is the normalized von-Mise equivalent strain rate
      use altayIOConfig, only: IPR,NLIST,IMP
      use criMathUtils, only: unit_sr_Matrix, pi
      use altayMacroKinematic
    
      implicit none
      integer,intent(in)                              :: NGR
      integer,intent(in)                              :: IGrElm
      type(DeformationRate),intent(in)                :: MacroDefRate     
      type(DeformationState),intent(in)               :: MacroDefState
      double precision,dimension(3,3),intent(out)     :: Tprinc
 
      !
      double precision :: AXX(3,3), GRPAR(3,3), TDCGr(3,3), T_phi(3,3) 
      double precision :: x, Sphi, Cphi, phi
      integer :: i,j, IA, IB
      !
            !
            if (NGR.eq.1) then      ! let Tprinc be equal to the identity matrix.
                  Tprinc = unit_sr_Matrix
                  return
            end if
            !
            GRPAR = matmul(MacroDefState%TotalDefGrad,TmatGr(:,:,IGrElm))
            if ((IPR.gt.1) .and.(NLIST.eq.1)) then
                  write (IMP,409) IGrElm
                  409 format (' IGrElm = ',i5) 
                  do i=1,3 
                        write (IMP,407) (TmatGr(j,i,IGrElm),j=1,3)
                  enddo
                  407 format (' TmatGr ',3d15.7)
                  do i=1,3 
                        write (IMP,408) (GRPAR(j,i),j=1,3)
                  enddo
                  408 format (' GRPAR  ',3d15.7)
            endif 
            !
            if ((IPR.gt.0) .and. (NLIST.eq.1) )then
                  write (IMP,100)
                  100  format (//,' CLUSTER1')
            end if
 

            !     Construction of orientation matrices for frames associated to the
            !     interfaces
            IA=1
            IB=2
            do i=1,3
                  AXX(i,1)=GRPAR(i,IA)
            enddo
            !       Orientation of interfaces containing axes IA and IB
            !       Normal axis: (vector product)
            AXX(1,3)=GRPAR(2,IA)*GRPAR(3,IB)-GRPAR(3,IA)*GRPAR(2,IB)
            AXX(2,3)=GRPAR(3,IA)*GRPAR(1,IB)-GRPAR(1,IA)*GRPAR(3,IB)
            AXX(3,3)=GRPAR(1,IA)*GRPAR(2,IB)-GRPAR(2,IA)*GRPAR(1,IB)
            !       Orientation of 2nd axis:(vector product)
            AXX(1,2)=AXX(2,3)*AXX(3,1)-AXX(3,3)*AXX(2,1)
            AXX(2,2)=AXX(3,3)*AXX(1,1)-AXX(1,3)*AXX(3,1)
            AXX(3,2)=AXX(1,3)*AXX(2,1)-AXX(2,3)*AXX(1,1)
            !       Normalisation
            do j=1,3
                  x=0.0d0
                  do i=1,3
                        x=x+AXX(i,j)**2
                  enddo
                  x=sqrt(x)
                  do i=1,3
                        AXX(i,j)=AXX(i,j)/x
                  enddo
            enddo
            do i=1,3
                  do j=1,3
                        Tprinc(i,j)=AXX(j,i)
                  enddo           
                  if (IPR.gt.0) then
                        if(NLIST.eq.1) then 
                        write (IMP,102) (Tprinc(i,j),j=1,3)
                  end if
            end if
            102      format (' TGrb ',3d15.7)            
            enddo
            
          !Transform MacroDefRate%StrainModevM to the "Tprinc" reference frame
          TDCGr = rotateSRTensorFrom(MacroDefRate%StrainModevM,Tprinc)
          !
          Sphi=TDCGr(2,3)
          Cphi=TDCGr(1,3)
          !
          if (abs(Sphi).lt.1.0d-6.and.abs(Cphi).lt.1.0d-6) then
              phi=0.0
          else
              phi=ATAN2(Sphi,Cphi)
          endif  
          !
          !Additional transformation matrix to a GB reference frame for which 
          ! the 2nd relaxation is allways perpendicular to the imposed strain mode
          T_phi= 0.D0
          T_phi(1,1)= cos(phi)
          T_phi(2,2)= cos(phi)
          T_phi(3,3)= 1.D0
          T_phi(1,2)= sin(phi)
          T_phi(2,1)= -sin(phi)
          !Update Tprinc        
          Tprinc = matmul(T_phi,Tprinc)
      !
      end subroutine

      
end module
    
