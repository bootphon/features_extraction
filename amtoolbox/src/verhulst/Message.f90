! displays message box with <text> and <title>

SUBROUTINE Message (text, title)
!	USE IFQWIN
	IMPLICIT NONE
	CHARACTER(*), INTENT(IN) :: text
	CHARACTER(*), INTENT(IN) :: title
!	integer(4) :: messageResult

!	messageResult = MessageBoxQQ (TRIM(text), TRIM(title), MB$OK  .OR. MB$ICONEXCLAMATION)
	WRITE (*,*) text

END SUBROUTINE
