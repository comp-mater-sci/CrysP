

program alamq
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
implicit none
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: Dmcoord, Dtcoord, Smcoord,Stcoord, SmIdent
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: tmplen, rho 
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: fi1,phi,fi2
      double precision                          :: rvalue, qvalue
      double precision                          :: R
      integer     :: info, iw
      integer     :: nslips = 16*6, nsteps = 1
      integer     :: i,j, nfis
      double precision,parameter :: fi2min = 0.D0, fi2max =  acos(-1.D0), rad2deg = (180.D0 / acos(-1.D0))
      double precision :: delta_fi2
      double precision,dimension(:),allocatable       :: qvalues, residuals
      !
      integer                 :: argc
      integer,parameter       :: argc_min = 1, argc_max=1
      character(len=128)      :: argv(0:argc_max)
      !
      info = 1
      iw = 3
      !
      !
      argc = command_argument_count()
      if (argc < argc_min) then
            write(*,*) 'one parameter is required: SMT-file'
            stop
      endif
      call get_command_argument(1,argv(1))
      ! Print banner
      write(*,'(/,A,1X,A,/)') 'Processing texture file', trim(argv(1))
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
      ! Texture data
      ! acnf%texture%input_fname='alum39.smt'  !! Test material
      ! acnf%texture%input_fname='alum926f.smt'  ! AA1100 
      acnf%texture%input_fname=trim(argv(1))
      ! >> typical dataset
      !acnf%texture%input_fname='micros.smt'
      call ALAMEL(1)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      fi1 = 0.0
      phi = 0.0
      fi2 = 0.0
      rho = 0.0

      nfis = 36
      !nfis = 2

      
      allocate(qvalues(nfis), residuals(nfis))
      
      delta_fi2 = (fi2max  - fi2min) / dble(nfis-1) 

      do i = 1,nfis
            write(*,'(/,A,1X,I4,1X,A,1X,F10.6,/)')'Point:',i,'fi2 =',fi2 
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Stcoord = 0.D0
            Stcoord(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(rho**2-rho+1)
            Stcoord(2,2) = rho*Stcoord(1,1)

            ! Rotate from "tensile" to material coordinate system
            Smcoord = matmul(transpose(Mrot),matmul(Stcoord,Mrot))
            !            
            call KMAT2VEC5D(Smcoord,vS) 
            !
            call multilevelYLP(vS,vA,vSonA,R,info)
            residuals(i) = R
            ! Calculate normalized stess
            vSonAn = vSonA / sqrt(dot_product(vSonA,vSonA)) 
            write(*,('(/)'))
            write(*,'(A,T30,5F10.6)') 'Requested stress:', vS
            write(*,'(A,T30,5F10.6)') 'Identified stress:',vSonAn       
            write(*,('(/)'))
            write(*,*) 'Identified stress (before normalization):'
            write(*,'(5F10.6)') vSonA
            
            ! Convert AONSET vector to tensor form
            call KVEC5D2MAT(vA,Dmcoord)
            
            if (iw >= 2) then
                  call KVEC5D2MAT(vSonAn,SmIdent)
                  write(*,400)
                  do j=1,3
                        write(*,401) Smcoord(j,:),SmIdent(j,:),Dmcoord(j,:)
                  enddo
            endif

            
            
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
            qvalues(i) = qvalue 
            write(*,'(A,T10,F10.6)') 'qvalue=', qvalue
            write(*,'(A,T10,F10.6)') 'rvalue=', rvalue
            !
            ! Next step
            fi2 = fi2 + delta_fi2
      enddo            

      fi2 = 0
      do i=1,nfis
            write(*,'(F10.6,1X,2F12.8)') rad2deg * fi2, qvalues(i), residuals(i)
            fi2 = fi2 + delta_fi2
      enddo
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))

end program