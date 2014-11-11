SUBROUTINE InitializeFiles
	USE Declare
	USE FilesModule
	USE MessageModule
!	USE DFLIB
	IMPLICIT NONE
	INTEGER length, startIndex
	LOGICAL ok, driveLetterPresent

	IF (storePressureEarCanal .OR. storeProfile .OR. storeProbing) THEN
		OutputDirectory = ADJUSTL(OutputDirectory)
		driveLetterPresent = INDEX(OutputDirectory, ":") == 2
		length = LEN_TRIM(outputDirectory)

		length = LEN_TRIM(outputDirectory)

		startIndex = 1
		IF (driveLetterPresent) startIndex = 3
		
		WriteFormat = 'FORMATTED'

	ENDIF

END SUBROUTINE  InitializeFiles
