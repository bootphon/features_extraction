! Resamples waveData and stores resampled signal in ResampledWaveData
! to match the frequency of the signal with toFrequency (with an accuracy of 1%)
! Afterwards toFrequency will be changed to match the frequency of the resampled signal
! exactly

SUBROUTINE Resample(toFrequency)
	USE Declare
	USE WaveReadModule
	IMPLICIT NONE

	REAL(8), INTENT (INOUT) :: toFrequency
!	INTEGER GreatestCommonDenuminator

	REAL, PARAMETER :: precision = .01e0
	INTEGER up
	INTEGER down
	INTEGER lcd

	up      = INT(.5e0 + toFrequency / (REAL(SamplesPerSec) * precision))
	down    = INT(1e0/precision)
	lcd     = GreatestCommonDenuminator (up, down)

	up      = up /lcd
	down    = down / lcd

	ResampledWaveDataPoints = SamplesPerChannel * up / down
	ALLOCATE (ResampledWaveData (0:ResampledWaveDataPoints-1), STAT = err)
	IF (err /= 0) CALL AllocationError
	
        CALL SampleUpAndDown  (waveData, SamplesPerChannel, up, down, ResampledWaveData)
	!IF (ALLOCATED(waveData)) DEALLOCATE(waveData)
	toFrequency = DBLE(SamplesPerSec * up / down)

CONTAINS

INTEGER FUNCTION GreatestCommonDenuminator (a, b)
	IMPLICIT NONE
	INTEGER, INTENT (IN) :: a, b
	INTEGER :: u, v, q, t
	
	u = a
	v = b
	DO WHILE (v/=0)
		q = u/v
		t = u - v*q
		u = v
		v = t
	ENDDO
	GreatestCommonDenuminator = ABS(u)

END FUNCTION GreatestCommonDenuminator	
	

END SUBROUTINE Resample
