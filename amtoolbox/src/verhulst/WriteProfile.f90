! Opens <profileFileName> in <OutputDirectory>
! Writes section number, resonance frequency, maximum position and maximum velocity
! of each plotted section to the file and closes it.


SUBROUTINE WriteProfile
	USE Declare
	USE GraphsModule
	USE FilesModule
	IMPLICIT NONE
	REAL f_resonance(n)
        REAL plotd(n) !!sv
        REAL plots(n) !!sv
        REAL plotdebug(n)!!sv 
        REAL QualityFactor(n)
	INTEGER ii, j, ios
	   
	!sv plotd and plots write the stifness and damping factors in the profile.dat file in 3rd and 4th column
        IF (storeProfile) THEN

		OPEN (FH_profile, FILE = TRIM(OutputDirectory) // TRIM(profileFileName), FORM = writeFormat, IOSTAT = ios)
		IF (ios /= 0) THEN
			CALL WriteError(profileFileName)
		ENDIF

		f_resonance = omega(1:n) / (2.d0 * pi) 
                plotd=dtot(1:n) !!sv
                plots=stot(1:n) !!sv
                plotdebug=debug(1:n)    !!sv
                
                DO i = 1, n, plotEvery_n_points
			ii = (i-1) / plotEvery_n_points
			WRITE (FH_profile, '(I7, ES23.7, ES23.7, ES23.7, ES23.7, ES23.7, ES23.7)', IOSTAT = ios) i, f_resonance(i), MAXVAL(positionMatrix(ii, :)), MAXVAL(velocityMatrix(ii, :)), plotd(i), plots(i), plotdebug(i)

		END DO

		CLOSE (FH_profile)
	
	ENDIF

END SUBROUTINE WriteProfile
