!This function is used by "cochlea" and "cochleaDialog"

!Returns a left-aligned string of 12 characters representing the integer <int>

FUNCTION IntToString (int)
	IMPLICIT NONE
	INTEGER int
	CHARACTER(12) IntToString, temp

	WRITE (temp, *) int
	IntToString = ADJUSTL (temp)

END FUNCTION IntToString
