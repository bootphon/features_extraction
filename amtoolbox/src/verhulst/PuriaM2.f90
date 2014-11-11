! Filters the stimulus with an approximation of the Puria2003 M2 filter
! Filter was designed as a fit to M2 in  Fig.2 of Puria2003
! 2nd order butterworth filter with fs=400000, fc1=600Hz and fc2=3000Hz,
! this makes that fc=1400Hz as in M2
! The parameters of the filter were found in Matlab (FDAtool), and translated into B and A filter coefficients with sos2tf

FUNCTION PuriaM2 (pressureStapes)
        USE Declare
	IMPLICIT NONE

        REAL(8), INTENT(IN) :: pressureStapes
        REAL(8) PuriaM2        
        REAL(8) M2gain       !filtergain from Puria M2
        REAL(8) Fin
        REAL(8), PARAMETER :: a2=-1.9626d0
        REAL(8), PARAMETER :: a3=0.9630d0
        REAL(8), PARAMETER :: b1=0.0185d0
        REAL(8), PARAMETER :: b2=0d0
        REAL(8), PARAMETER :: b3=-0.0185d0
        !Filter parameters
        
        M2gain = 1d0 / (10d0 ** (30d0/20d0)) !-30dB gain 
        Fin = M2gain * pressureStapes !gain applied
        PuriaM2=( b1 * Fin + b2 * PuriaOutxmin1 + b3 * PuriaOutxmin2 - a2 * PuriaOutymin1 - a3 * PuriaOutymin2 ) !filtering as in Matlab filter function
                        
        PuriaOutxmin2=PuriaOutxmin1 
        PuriaOutxmin1=Fin
        PuriaOutymin2=PuriaOutymin1
        PuriaOutymin1=PuriaM2
        !passing on the variables for next timestep dt

	!PuriaM2=Fin !pressureStapes !for testing purposes, or to cancel the filtering

END FUNCTION
