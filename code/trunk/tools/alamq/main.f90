

program alamq
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
implicit none
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: Dmcoord, Dtcoord, Smcoord,Stcoord
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: tmplen, rho 
      double precision,dimension(5)             :: vA, vS,vSonA
      double precision                          :: fi1,phi,fi2
      double precision                          :: rvalue, qvalue
      integer     :: info
      integer     :: nslips = 16*6, nsteps = 1
      !
      info = 1
      !
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! INITIALIZATION OF ALAMEL: it should be done in different way !!!!
      call initConfig(acnf,nslips,nsteps,info)
      if (info /= 0) then
            write(*,*) 'Cannot initialize alamel config'
            stop
      endif
      call initResults(ares,nsteps,info)
      if (info /= 0) then
            write(*,*) 'Cannot initialize data structure for alamel results'
            stop
      endif           
      ! Apply modifications to acnf if needed.
      acnf%output_prefix = 'example'
      acnf%jobtitle = 'example job'
      acnf%micros_fname = 'micro1.smt'
      ! configure slipsystem data
      acnf%slipsystem%input_fname = 'fcc.pre'
      acnf%texture%input_fname='alum39.smt'
      ! >> typical dataset
      !acnf%texture%input_fname='micros.smt'
      call ALAMEL(1)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      fi1 = 0.0
      phi = 0.0
      fi2 = 0.0 
      rho = 0.0
       
      ! Calculate rotation matrix
      call KROTMAT(fi1,phi,fi2,Mrot)
      !
      Stcoord = 0.D0
      Stcoord(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(rho**2-rho+1)
      Stcoord(2,2) = rho*Stcoord(1,1)

      ! Rotate from "tensile" to material coordinate system
      Smcoord = matmul(transpose(Mrot),matmul(Stcoord,Mrot))
      
      call KMAT2VEC5D(Smcoord,vS) 
      
      call multilevelYLP(vS,vA,vSonA,info)
            !

            !call KNORML5D(df,AONSET,tmplen)
      
      ! Convert AONSET vector to tensor form
      call KVEC5D2MAT(vA,Dmcoord)
      ! Rotate back to the "tensile test" coordinate system   
      Dtcoord = matmul(matmul(Mrot,Dmcoord),transpose(Mrot))
      !            
      ! Calculate output variables
      if ( abs(Dtcoord(3,3)) >= epsilon(0.D0) ) then
            rvalue =  Dtcoord(2,2) / Dtcoord(3,3)
            !qv = -Dtcoord(2,2) / Dtcoord(1,1)
            qvalue = rvalue / (1.D0 + rvalue)
            ! borrowed from Facet 
            !rqs.svalue = dsqrt(3.D0/2.D0)*SCALS/(dsqrt(rho**2-rho+1))
      else
            info = 4
      endif



end program