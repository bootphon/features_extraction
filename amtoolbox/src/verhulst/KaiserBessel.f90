! Computes the Kaiser-Bessel function with the variable alphaKB
! (see, e.g., introductory Digital Signal Processing; Paul A. Lynn & Wolfgang Fuerst, p. 154)
! Answer is an array: -M ... 0 ... M, containing 2*M + 1 points
! Fortran automatically adjusts the indices for the subroutine call
! to an array that runs from 1 .. 2*M+1.

! below an alternative code for the ISML Bessel function routine

SUBROUTINE KaiserBessel(answer, M, alphaKB)
  !use BSI0_INT
  !include 'link_fnl_static.h'
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: M
  REAL, INTENT(OUT) :: answer(-M:M)
  REAL, INTENT(IN) :: alphaKB
  REAL arg
  INTEGER n
  
  DO n = 1, M
     arg = 1e0 - (REAL(n)/REAL(M)) ** 2e0
     answer(n) = BESI0(alphaKB * SQRT(arg))/BESI0(alphaKB)
     ! Kaiser Bessel is symmetric, hence:
     answer(-n) = answer(n)
  ENDDO
  answer(0) = 1e0
  
  
Contains        
  REAL FUNCTION BESI0 (x) !!USE ISML-routine
    IMPLICIT NONE
    real, intent(in) :: x
    real :: tloc, yloc, eloc, deloc, sde
    integer :: jcnt
    
    yloc=x/2.
    tloc=1.e-8
    eloc=1.
    deloc=1.
    
    do jcnt=1,25
       deloc=deloc*yloc/float(jcnt)
       sde=deloc*deloc
       eloc=eloc+sde
       if (eloc*tloc > sde ) exit
    enddo
    BESI0 = eloc
    return
  END FUNCTION BESI0  

END SUBROUTINE KaiserBessel
  


    
