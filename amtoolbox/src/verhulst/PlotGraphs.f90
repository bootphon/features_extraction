SUBROUTINE PlotGraphs

	USE Declare
	USE GraphsModule
	IMPLICIT NONE
	INTEGER ii

!		IF (MOD(kk, plotEvery_t_points) == 0) THEN
!			t_plotCounter = t_plotCounter +1
			! store current velocities and position of sections 1:n with an interval of plotEvery_n_points in velocityMatrix and positionMatrix
!			velocityMatrix(section_axis, t_plotCounter) = REAL(V(1:n:plotEvery_n_points))!
!			positionMatrix(section_axis, t_plotCounter) = REAL(Y(1:n:plotEvery_n_points))
!		ENDIF

END SUBROUTINE PlotGraphs
