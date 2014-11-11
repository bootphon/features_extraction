! If <storeMembraneStatus>, stores final BM-displacement
! <Y> and BM-velocity <V> in the binary files
! <FN_FINALPOSITIONS> and <FN_FINALVELOCITIES> respectively.

! If a write error occurres, WriteError is called
SUBROUTINE WriteMembraneStatus
	USE Declare
	USE FilesModule
	IMPLICIT NONE

	INTEGER irl, ios

	IF (storeMembraneStatus) THEN
		irl = (n+1)*8

		! write final mebrane state (positions)
		OPEN (FH_FINALPOSITIONS, FILE = FN_FINALPOSITIONS, ACCESS='direct', RECL=irl, IOSTAT = ios)
		WRITE (FH_FINALPOSITIONS, REC = 1, IOSTAT = ios) Y !(Y(i), i=0,n)
		CLOSE (FH_FINALPOSITIONS)
		IF (ios /= 0) CALL WriteError(FN_FINALPOSITIONS)

		! write final mebrane state (velocities)
		OPEN (FH_FINALVELOCITIES, FILE = FN_FINALVELOCITIES, ACCESS='direct', RECL=irl, IOSTAT = ios)
		WRITE (FH_FINALVELOCITIES, REC=1, IOSTAT = ios) V !(v(i), i=0,n)  
		CLOSE (FH_FINALVELOCITIES)
		IF (ios /= 0) CALL WriteError(FN_FINALVELOCITIES)

	ENDIF

END SUBROUTINE WriteMembraneStatus
