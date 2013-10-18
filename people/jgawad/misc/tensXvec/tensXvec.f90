program tensXvec
use fngMathUtils
use fngRuntime
use fngVec5D
implicit none
!
double precision,dimension(3,3)     :: tens
double precision                    :: v5D(5), v6D(6)
!
type(MapItem)     :: fake_map(0)
type(commandLine) :: cmdline
integer :: info, i
!
      call processCommandLine(cmdline,5,6,fake_map,0,info)
      !
      v6D = 0.D0
      v5D = 0.D0
      select case (cmdline%argc)
      case(5)     !
            do i=1,5
                  read(cmdline%argv(i),*) v5D(i)
            enddo
            tens = vec5D2tens(v5D)
            v6D = Mat33ToVec6(tens)
            write(*,'(6(E15.8,1X))') v6D
      !      
      case(6)     !
            do i=1,6
                  read(cmdline%argv(i),*) v6D(i)
            enddo 
            tens = Vec6ToMat33(v6D)
            v5D = tens2vec5D(tens)
            write(*,'(5(E15.8,1X))') v5D
      case default
            9999 format('The program expects either 5 or 6 real values.',/, &
                        ' - If 5 values are provided, they are interpreted as components of the 5D vector representation of a deviatoric tensor.',/, &
                        ' - If 6 values are provided, they are interpreted as a sequence of components in a symmetric tensor X:',/, &
                        '   X11, X22, X33, X12, X23, X13')
            write(*,9999) 
      end select 
!
end program
