! returns the level in pressure, corresponding to the <dB_value> relative to the <reference> - pressure.

FUNCTION DecibelToLevel (dB_value, reference)
	IMPLICIT NONE
	REAL(8) DecibelToLevel
	REAL(8), INTENT(IN) :: dB_value
	REAL(8), INTENT(IN) :: reference

	DecibelToLevel = reference * 10 ** (dB_value / 20)

END FUNCTION