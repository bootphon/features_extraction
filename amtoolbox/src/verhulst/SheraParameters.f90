!this subroutine calculates SheraRho(1:n), SheraMu(1:n) and Sherad(1:n) 
!for a given pole distribtion SheraP(1:n) along the length of the cochlea
!for calculation details see footnote 8 of Shera 2001

SUBROUTINE SheraParameters
     USE Declare
     IMPLICIT NONE     
     REAL(8), PARAMETER ::  c = 120.8998691636393d0
     REAL(8) a(1:n)
     REAL(8) pimu(1:n)
        
     a(1:n) = ( SheraP(1:n) + DSQRT( (SheraP(1:n) ** 2d0) + c * (1d0 - (SheraP(1:n) ** 2d0))) ) / c 
     Sherad(1:n) = 2d0 * (SheraP(1:n) - (a(1:n)))
     !SheraMu(1:n) = 1.7435d0 !to be idem to ZweigFactor !1.7441d0
     SheraMu(1:n)=1d0 / (2d0 * pi * a(1:n)) !replaced from the tlm2012 implementation. Is now made variable
    ! pimu = 1 / (2 * a(1:n))
    ! SheraRho = (DSQRT(1.d0 - (Sherad(1:n) ** 2d0)/4)) / (pimu(1:n) * DEXP(1.d0 + pimu * Sherad))
     SheraRho(1:n) = 2d0 * a(1:n) * DSQRT(1d0 - (Sherad(1:n)/2d0)**2d0) * DEXP(-SheraP(1:n)/a(1:n)) !replaced from the tlm2012 implementation

END SUBROUTINE SheraParameters
