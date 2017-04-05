      implicit double precision (a-h,o-z) 
      dimension TRAT(10),JTRAT(10)
      do J=1,10
        JTRAT(J)=-J
      enddo
      do j=1,10
        TRAT(j)=10*(11-j)
        write (*,100) JTRAT(j),TRAT(j)
 100  format (I5,F20.5)
      enddo
      call SORTT(10,TRAT,JTRAT)
      do j=1,10
        write (*,100) JTRAT(j),TRAT(j)
      enddo  
      stop
      end      
                  
      SUBROUTINE SORTT(M,TRAT,JTRAT)
C     To sort TRAT in descending order and modify the order
C     of JTRAT accordingly
      implicit double precision (a-h,o-z) 
      DIMENSION TRAT(M),JTRAT(M) 
      do JJ=1,M-1
        TMAX=TRAT(JJ)
        jmax=JJ
        do j=JJ+1,M
          if (TRAT(j).gt.TMAX) then
                                 jmax=j
                                 TMAX=TRAT(j)
                               endif
        enddo
        X=TRAT(JJ)
        TRAT(JJ)=TMAX
        TRAT(jmax)=X
        j=JTRAT(JJ)
        JTRAT(JJ)=JTRAT(JMAX)
        JTRAT(JMAX)=j
      enddo
      return
      end