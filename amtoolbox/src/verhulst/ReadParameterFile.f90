! This routine is shared by "Cochlea" and CochleaDialog"

! reads parameters from <FileName> using a call to Parameters

SUBROUTINE ReadParameterFile (FileName)
	USE FilesModule
	IMPLICIT NONE
	CHARACTER(512), INTENT (IN) :: FileName
	INTEGER ios

	OPEN (FH_parameters, FILE = FileName, IOSTAT = ios)
	CALL Parameters (READS, FH_parameters)
	CLOSE (FH_parameters)

END SUBROUTINE ReadParameterFile