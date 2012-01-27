!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-14
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> Useful data types for calculation of anisotropic characteristics
module qrsTypes

      type qrsData
            double precision :: qvalue = 0.D0
            double precision :: rvalue = 0.D0
            double precision :: svalue = 0.D0
      end type    

contains

      type(qrsData) pure function avgQRS(qrsvalues)
      implicit none
      type(qrsData),dimension(:),intent(in)     :: qrsvalues
      double precision :: frc
      integer :: i,n
      !
            avgQRS = qrsData(0.D0, 0.D0, 0.D0)
            n = size(qrsvalues) 
            if (n > 0) then
                  do i=1,n
                       avgQRS%qvalue= avgQRS%qvalue +  qrsvalues(i)%qvalue
                       avgQRS%rvalue= avgQRS%rvalue +  qrsvalues(i)%rvalue
                       avgQRS%svalue= avgQRS%svalue +  qrsvalues(i)%svalue
                  enddo
                  frc = 1.D0 / dble(n)
                  avgQRS%qvalue = avgQRS%qvalue * frc
                  avgQRS%rvalue = avgQRS%rvalue * frc
                  avgQRS%svalue = avgQRS%svalue * frc
            endif
      
      end function


      type(qrsData) pure function calculateQRS(Dt,s) result(qrsvalue)
      implicit none
      double precision,dimension(3,3),intent(in)      :: Dt
      double precision,intent(in)                     :: s
            if ( abs(Dt(3,3)) >= epsilon(0.D0) ) then
                  qrsvalue%rvalue = Dt(2,2) / Dt(3,3)
                  qrsvalue%qvalue = qrsvalue%rvalue / (1.D0 + qrsvalue%rvalue)
                  qrsvalue%svalue = s
            else
                  qrsvalue = qrsData(0.D0, 0.D0, 0.D0)
            endif
      end function      

      
end module