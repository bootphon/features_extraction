! Main program

! Reads parameters from parameter file <parameter.dat>
! *Reads nonLinearity file <gamma.fat> in case of active damping
! *Reads membrane status from <final_y.bin> and <final_v.bin>
! *Reads and resamples wavefile
! Performs the GaussElimination/RK4-loop:
!		calculating the position and velocity of each section of the membrane for each time step
!		*writing the pressure at the ear canal to file
!		*writing the velocity and position of the user set probe points to file
!		*plotting the graphs (legend, membrane, stimulus, cochleagram)
!		*determining the phase of each section relative to the signal
! *Plots profile (maximum position or velocity of each section) and phase of each section
! Displays rescale dialog box and plots the rescaled graphs until finish button is pressed 
! *Writes final membrane status to <final_y.bin> and <final_v.bin>
! *Writes profile to user set file

! * Actions starting with an *, will only be performed if specified in <parameter.dat>

PROGRAM cochlea
        USE WaveReadModule
	USE Declare
	USE FilesModule
	USE MessageModule
	USE GraphsModule

	IMPLICIT NONE

	integer(2) :: irow
	integer(4) :: kk100,kmd100, kkl, nkmod, kmod
	REAL :: time_begin, time_end

	CALL ReadParameterFile (FN_parameters)
	IF (.NOT. ParameterFileSucceeded) &
		WRITE (*,*) 'Incorrect parameter file !'
	CALL Initialize
	CALL ReadMembraneStatus 
	CALL OpenWrite
	CALL InitializeGraphs
	IF (UseAudioFile) THEN
           CALL WaveRead (audiofilename, useLeftChannel, .TRUE.)
           CALL Resample (2d0 * ComputationalFrequency)
	ENDIF


	CALL CPU_TIME(time_begin)
!000 use 'double'counting to minimize # UpdateLegends calls
	nkmod=kmax/100
	kmod =mod(kmax,100)
	DO kk100 = 0, nkmod-1
	  do kmd100=0,99
	    kk = kk100*100 + kmd100
		t = dt * kk
		CALL WriteData
		IF (MOD(kk, plotEvery_t_points) == 0) THEN
			t_plotCounter = t_plotCounter +1
			! store current velocities and position of sections 1:n with an interval of plotEvery_n_points in velocityMatrix and positionMatrix
			velocityMatrix(section_axis, t_plotCounter) = REAL(V(1:n:plotEvery_n_points))
			positionMatrix(section_axis, t_plotCounter) = REAL(Y(1:n:plotEvery_n_points))
		ENDIF

		CALL RK4
	  enddo

	ENDDO
!!111 last block may contain different number of elements (kmod < 100)
!! hence: separate treatment
	kkl=kk+1
	DO kk=kkl,kkl+kmod
		t = dt * kk
		CALL WriteData
		IF (MOD(kk, plotEvery_t_points) == 0) THEN
			t_plotCounter = t_plotCounter +1
			! store current velocities and position of sections 1:n with an interval of plotEvery_n_points in velocityMatrix and positionMatrix
			velocityMatrix(section_axis, t_plotCounter) = REAL(V(1:n:plotEvery_n_points))
			positionMatrix(section_axis, t_plotCounter) = REAL(Y(1:n:plotEvery_n_points))
		ENDIF
		CALL RK4
	ENDDO
!!now write requested final points
!    CALL UpdateLegend(100 * kk / kmax)
	t = t + dt
	CALL WriteData
	
	CALL CPU_TIME(time_end)
	WRITE (*,*) "Time: ", time_end - time_begin
	CALL DeInitialize (.TRUE.)
	CALL CloseWrite	
	!CALL ClosePressureEarCanal
	!IF(storeProbing) CALL CloseProbing

	CALL WriteMembraneStatus
	IF(storeProfile) CALL WriteProfile

	CALL DeInitialize (.FALSE.)

 END PROGRAM cochlea
