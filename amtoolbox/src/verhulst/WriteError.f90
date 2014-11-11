!displays a message box informing the user that a write error occurred
! while writing to <fileName> 

SUBROUTINE WriteError(message)
!	USE DFLIB
	IMPLICIT NONE
	CHARACTER (*), INTENT(IN) :: message
!        integer(4) :: MESSAGEBOXQQresult
!	MESSAGEBOXQQresult = MESSAGEBOXQQ ("Could not write to " // TRIM(fileName) // ".\nMake sure file is not write protected or in use by\nanother program and that there is enough disk space."C, &
!					"Error writing "//TRIM(FileName) // ""C, MB$OK .OR. MB$ICONINFORMATION) 
			WRITE (*,*) 'Could not write to file: ' // TRIM(message)
END SUBROUTINE
