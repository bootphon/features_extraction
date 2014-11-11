! Filters the stimulus with an approximation of the Puria2003 M1 filter
! Calculations done for different offsets because x-1,x-2 need to be stored corresponding to a specific offset in memory
! Filter was designed as a fit to M1 in  Fig.2 of Puria2003
! 2nd order butterworth filter with fs=400000, fc1=100Hz and fc2=3000Hz,
! this makes that fc=+/-900Hz as in M1
! The parameters of the filter were found in Matlab (FDAtool), and translated into B and A filter coefficients with sos2tf

FUNCTION PuriaM1 (offset,Stim)
        USE Declare
	IMPLICIT NONE

        INTEGER, INTENT(IN) :: offset
        REAL(8), INTENT(IN) :: Stim
        REAL(8) PuriaM1        
        REAL(8) pt
        REAL(8) gain       !filtergain from Puria M1
        REAL(8) Stim1
        REAL(8) VoltageDivisionGain
        !VoltageDivisionGain is because Rme +/-= Zcoch in this model. 
        !Stim thus goes through 2* Zcoch, and thus stim needs to be multiplied
        !by 2 to have Stim left after Rme is passed, and before Zcoch is entered. 
        REAL(8), PARAMETER :: a2=-1.9554d0
        REAL(8), PARAMETER :: a3=0.9555d0
        REAL(8), PARAMETER :: b1=0.0223d0
        REAL(8), PARAMETER :: b2=0d0
        REAL(8), PARAMETER :: b3=-0.0223d0
        !Filter parameters
        
        VoltageDivisionGain=2d0 !2 resistor matching network(Zme+Zoch)
        pt=offset+1d0
        gain = 10d0 ** (18d0/20d0) !18dB gain 
        Stim1 = VoltageDivisionGain * gain * Stim
        PuriaM1=( b1 * Stim1 + b2 * PuriaInxmin1(pt) + b3 * PuriaInxmin2(pt) - a2 * PuriaInymin1(pt) - a3 * PuriaInymin2(pt) )
                        
        PuriaInxmin2(pt)=PuriaInxmin1(pt)
        PuriaInxmin1(pt)=Stim1
        PuriaInymin2(pt)=PuriaInymin1(pt)
        PuriaInymin1(pt)=PuriaM1

	!PuriaM1=Stim1 !VoltageDivisionGain * Stim !for testing purposes, or to cancel the filtering

END FUNCTION
