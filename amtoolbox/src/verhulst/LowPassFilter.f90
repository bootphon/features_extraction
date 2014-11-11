! Calculates the Inverse Fourier Transform of a low pass filter
! with cut off frequency <f_cutoff>.
! answer(n) = 2 * f * sinc (2 * pi * f * n) = sin (2 * pi * f * n)/(pi * n)
! Answer is array [-M ... 0 ... M] containing 2*M + 1 values

SUBROUTINE LowPassFilter (answer, M, f_cutoff)
	IMPLICIT NONE

	INTEGER, INTENT(IN) :: M
	REAL, INTENT(OUT) :: answer(-M:M)
	REAL, INTENT(IN) :: f_cutoff
	REAL sinc

	REAL, PARAMETER :: pi = 3.14159265
	REAL two_pi_f
	INTEGER n

	two_pi_f = 2 * pi * f_cutoff

	DO n = 1, M
		answer(n) = SIN (two_pi_f * n) / (pi * n)
		! sinc is symmetrisch, dus:
		answer(-n) = answer(n)
	ENDDO
	answer(0) = 2 * f_cutoff

END SUBROUTINE LowPassFilter