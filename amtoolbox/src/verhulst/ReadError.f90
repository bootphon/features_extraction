!displays a message box informing the user that a read error occurred
! while reading from <fileName> 

SUBROUTINE ReadError(fileName)
!	USE DFLIB
	IMPLICIT NONE
	CHARACTER (*), INTENT(IN) :: fileName
!	integer(4) :: messageBoxQQresult

!	messageBoxQQresult = MESSAGEBOXQQ ("Could not read from " // TRIM(fileName) // & 
!	                &".\nMake sure the file is in the right directory, it's a correct &
!	                &\nfile and the file is not in use by another program"C, &
!					&"Error reading "//TRIM(FileName) // ""C, MB$OK .OR. MB$ICONINFORMATION) 
			WRITE (*,*) 'could not read data file: ' // TRIM(fileName)

END SUBROUTINE
