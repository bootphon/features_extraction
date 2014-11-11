MODULE GraphsModule
	IMPLICIT NONE

	INTEGER(2), ALLOCATABLE :: membrane_y_pixel (:)
	INTEGER(2), ALLOCATABLE :: section_axis(:)
	INTEGER, ALLOCATABLE :: white(:)
	INTEGER, ALLOCATABLE :: black(:)
	REAL, ALLOCATABLE :: velocityMatrix ( : , : )
	REAL, ALLOCATABLE :: positionMatrix ( : , : )
	INTEGER(2) t_plotCounter
	REAL(8), ALLOCATABLE :: PositionPhase(:)
	REAL(8), ALLOCATABLE :: VelocityPhase(:)
	REAL(8), ALLOCATABLE :: previousPosition(:)
	REAL(8), ALLOCATABLE :: previousVelocity(:)
	REAL, ALLOCATABLE :: LastPositionPoints(:, :)
	REAL, ALLOCATABLE :: LastVelocityPoints(:, :)

	INTEGER LastIndex
	REAL maximumVelocity
	REAL minimumVelocity
	REAL maximumPosition
	REAL minimumPosition
	INTEGER t_plotpoints
	INTEGER n_plotpoints

END MODULE GraphsModule
