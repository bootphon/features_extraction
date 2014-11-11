SUBROUTINE SolveTridiag(aa,bb,cc,vv,matrow,Qsol)
      USE Declare
      IMPLICIT none
!      aa - sub-diagonal (means it is the diagonal below the main diagonal)
!      bb - the main diagonal
!      cc - sup-diagonal (means it is the diagonal above the main diagonal)
!      vv - right part
!      Qsol - the answer
!      matrow - number of equations (ME+n)

        INTEGER, INTENT(IN) :: matrow
        REAL(8), INTENT(OUT):: Qsol (0:matrow-1)
        REAL(8), INTENT(IN) :: aa(0:matrow-1) !bottom row ZAL
        REAL(8), INTENT(IN) :: bb(0:matrow-1) !diagonal ZASC
        REAL(8), INTENT(IN) :: cc(0:matrow-1) !top row ZAH
        REAL(8), INTENT(IN) :: vv(0:matrow-1) !right side
        REAL(8)  gam(0:matrow-1)
        REAL(8) bet
        INTEGER r
        !All the vectors go from 0 (ME) to n(sections), 
        !In matlab you cannot start at 0 value
        !everything here starts from 0 iso 1(matlab)
 
        bet=bb(0)  !bb(0) should not be 0 is ok
        Qsol(0)=vv(0)/bet

        !First pass
        DO r = 1,matrow-1
           gam(r)=cc(r-1)/bet
           bet=bb(r)-aa(r)*gam(r) !%bet should not be 0
           Qsol(r)=(vv(r)-aa(r)*Qsol(r-1))/bet
        END DO

        !Second pass
        DO r = matrow,0,-1
           Qsol(r)=Qsol(r)-gam(r+1)*Qsol(r+1)
        END DO
       
END SUBROUTINE SolveTridiag
