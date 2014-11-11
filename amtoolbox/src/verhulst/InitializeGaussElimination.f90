	! Calculation of diagonal elements of tridiagonal matrix A.
	! This has to be done only once because A is time invariant.
SUBROUTINE InitializeGaussElimination

	USE Declare
	IMPLICIT NONE
	REAL(8) Scala_Area
	! Tridiagonal matrix has in the upper left corner the value AME,
	! in the lower right corner AHT and the diagonal values in between are ASC
        REAL(8) AME
        REAL(8) ASC
        REAL(8) AHT
        REAL(8) ZAHT
        REAL(8) ZAME    
            
            ! ZASC and ZASQ depend on i, so calculate Ms(i) and Mp(i) first
            ! all have size 0:n where 0 is ME and 1:n the BM sections
             DO i = 0, n, 1
                ZweigMs(i) = (ZweigMso * ZweigOmega_co) / omega(i) !kg/m5
                ZweigMp(i) = Ko / (ZweigOmega_co * omega(i))       !kg/m3
             ENDDO 
             !Fill in the constants in the tridiagonal matrix MAT*q=g*ZASQ
             
             !FOR ME
             ZASQ(0)=1d0 ![]
             ZASC(0)=1d0 + (ZweigMso*dx)/Mme !diagonal line ![]
             ZAL(0)=0d0 !bottom diagonal
             ZAH(0)=-1d0 !top diagonal
             ZAH(n)=-0d0
             !ZAL and ZAH have one value less than ZASC 
             !because they are matrix diagonals
             !ZAL has to start with zero
             !ZAH has to end with zero
                          
             !FOR THE LOWER DIAGONAL
             DO i = 1, n, 1
                ZAL(i)=-ZweigMs(i) ![kg/m5]
                !ZAL(i)=-1d0 ![kg/m5] with no crosscoupling
             ENDDO
             
             !FOR THE TOP DIAGONAL
             DO i = 1, n-1,1
                ZAH(i)=-ZweigMs(i-1) ![kg/m5]
                !ZAH(i)=-1d0 ![kg/m5] with no cross coupling
             ENDDO

             !FOR THE MAIN DIAGONAL
             DO i = 1, n, 1
                !ZASQ(i) = ((omega(i) * ZweigMs(i) * (dx ** 2.d0)) / (ZweigOmega_co * ZweigMpo)) ![kg/m5] no crosscoupling
                ZASQ(i) = ((omega(i) * ZweigMs(i) * ZweigMs(i-1) * (dx ** 2.d0)) / (ZweigOmega_co * ZweigMpo)) ![kg/m5]
                ZASC(i) = ZASQ(i) +  ZweigMs(i) + ZweigMs(i-1) ![kg/m5]
                !ZASC(i)=ZASQ(i)+2d0
             ENDDO 

END SUBROUTINE
