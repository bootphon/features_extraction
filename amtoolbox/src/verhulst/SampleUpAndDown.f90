! Resamples array <input> containing <input_size> points.
! The routine samples up the original signal by (virtually) inserting
! (up-1) zero's between each two points and low-passfiltering in the
! time domain afterwards. Selecting only every <down>th point,
! downsampling of the upsampled signal is established.
! This results in a netto upsampling of up/down (rounded off to zero)
! output array must contain exactly input_size * up / down points (rounded off to zero)
! Note: in Fortran lower and upperbounds of calling array's do not have to be the
! same as those of the dummy argument array's as long as their size is equal.

SUBROUTINE SampleUpAndDown  (input, input_size, up, down, output)
	!USE IFQWIN, ONLY: SETTEXTPOSITION, rccoord, SETACTIVEQQ
	!USE WindowsModule, ONLY: LEGEND_UNIT
	IMPLICIT NONE

	INTEGER, INTENT(IN)	:: input_size
	INTEGER, INTENT(IN)	:: up
	INTEGER, INTENT(IN)	:: down
	REAL, INTENT(IN) :: input (0 : input_size -1)
	REAL, INTENT(OUT) :: output (0: input_size * up / down -1)

	REAL, PARAMETER :: alphaKB=11 ! ( = 3.5 * pi )
	INTEGER, PARAMETER :: relative_window_size = 20  ! (20)
	INTEGER output_size

	INTEGER M	! window = array van -M ..0 .. +M
	REAL, ALLOCATABLE :: KB(:)
	REAL, ALLOCATABLE :: window(:)
	REAL :: relative_f_cutoff
	INTEGER largest_factor
	INTEGER n, i, i_begin, i_end, output_i
	INTEGER err

	!neglecting 1st and 2nd half of convolving window
	output_size =  input_size * up /down

	largest_factor = MAX(up, down)
	M = relative_window_size * largest_factor
	ALLOCATE (KB(-M:M), window(-M:M), STAT = err)
	IF (err /= 0) CALL AllocationError

	! f_cutoff is relative to the sample frequency of the upsampled signal
	! f_cutoff moet kleiner zijn van 1/(2*largest_factor)
	relative_f_cutoff = .4d0/REAL(largest_factor)
	CALL LowPassFilter (window, M, relative_f_cutoff)
	CALL KaiserBessel (KB, M, alphaKB)
	window = window * KB * REAL(up)

	! (1) add (up-1) zero's
	! (2) low pass filter
	! (3) skip (down-1) points

	! low pass filter is implemented as convolution in time-domain
	! output[n] = SOMi [input_with_zero's_added(i) * window(n-i)]
	! output[n] = SOMi [input(i) * window(n-i*up)]
	! input is array [0 .. input_size-1], so:
	!		(a)		i >= 0
	!		(b)		i <= input_size-1
	! window is array [-M .. +M], so:
	!		(c)		n-i*up <= +M	<-->	i >= (n-M)/up
	!		(d)		n-i*up >= -M	<-->	i <= (n+M)/up

	n = 0
	!CALL InitializeLegend(.TRUE.)
	DO output_i = 0, output_size-1
		!CALL updateLegend(100 * (output_i+1) /output_size)
		i_begin = MAX ((n-M-1)/up+1, 0)				! (a) and (c)
		i_end = MIN ((n+M)/up, input_size -1)	! (b) and (d)

		! convolution: output[n] = SOMi [input(i) * window(n-i*up)]
		output(output_i) = 0
		DO i = i_begin, i_end
			output(output_i) = output(output_i) + input(i) * window(n-i*up)
		END DO

		! downsampling, skip down-1 points:
		n = n + down
	ENDDO

	DEALLOCATE (KB)
	DEALLOCATE (window)

END SUBROUTINE SampleUpAndDown
