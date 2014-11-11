! Releases all allocated memory,
! displays message and
! terminates program

SUBROUTINE AllocationError
	USE MessageModule
	IMPLICIT NONE

	CALL DeInitialize (.TRUE.)
	CALL DeInitialize (.FALSE.)
	CALL Message (MessageText(ALLOCATIONFAILURE), MessageTitle(ALLOCATIONFAILURE))
	STOP

END SUBROUTINE