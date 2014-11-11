SUBROUTINE InitializeGraphs
	USE Declare
	USE GraphsModule
	IMPLICIT NONE

	n_plotpoints = (n-1)/plotEvery_n_points +1
	t_plotpoints = kmax/plotEvery_t_points +1
	ALLOCATE (section_axis(0: n_plotpoints-1), &

	velocityMatrix (0:n_plotpoints-1, 0:t_plotpoints-1), &
	positionMatrix (0:n_plotpoints-1, 0:t_plotpoints-1), &
	PositionPhase(0:n_plotpoints - 1), &
	VelocityPhase(0:n_plotpoints - 1), &
	previousPosition(0:n_plotpoints - 1), &
	previousVelocity(0:n_plotpoints - 1), &
	STAT = err)

	IF (err /= 0) CALL AllocationError
	PositionPhase = -1
	VelocityPhase = -1
	previousPosition = 0
	previousVelocity = 0

	section_axis = [0 : n_plotpoints - 1]
	membrane_y_pixel = 0
	t_plotCounter = -1

END SUBROUTINE InitializeGraphs
