! sets initial BM-displacement <Y> and BM-velocity <V>

! If <retrieveMembraneStatus> then <Y> and <V> are read
! from the binary files <FN_FINALPOSITIONS> and
! <FN_FINALVELOCITIES> respectively, otherwise
! <Y> and <V> are set to zero

! If a read error occurres, ReadError is called and
! program continues with <Y> and <V> set to zero

SUBROUTINE ReadMembraneStatus
	USE Declare
	USE FilesModule
	IMPLICIT NONE

	INTEGER irl, ios

	Y = 0
	V = 0

	IF (retrieveMembraneStatus) THEN
		irl=(n+1)*8

		! read initial mebrane state (positions)
		OPEN (FH_FINALPOSITIONS, FILE = FN_FINALPOSITIONS, ACCESS='direct', RECL = irl, IOSTAT =ios)
		READ (FH_FINALPOSITIONS, REC=1, IOSTAT =ios) Y
		CLOSE (FH_FINALPOSITIONS)
		IF (ios /= 0) THEN
			CALL ReadError(FN_FINALPOSITIONS)
			Y = 0
			V = 0
		ENDIF

		! read initial mebrane state (velocities)
		OPEN (FH_FINALVELOCITIES, FILE = FN_FINALVELOCITIES, ACCESS='direct', RECL = irl, IOSTAT =ios)
		READ (FH_FINALVELOCITIES, REC=1, IOSTAT =ios) V
		CLOSE (FH_FINALVELOCITIES)
		IF (ios /= 0) THEN
			CALL ReadError(FN_FINALVELOCITIES)
			Y = 0
			V = 0
		ENDIF
	END IF

END SUBROUTINE ReadMembraneStatus