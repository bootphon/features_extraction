! Opens <PressureEarCanalFileName> in <outputDirectory>
! If <storeExtraInfo>, a header row will be written, indicating
! what values will be written in each colum, that is: "time (s)"
! and "pressure Ear Canal (Pa)"

SUBROUTINE CloseWrite
	USE Declare
	USE FilesModule
	IMPLICIT NONE

	IF (storePressureEarCanal) THEN
		CLOSE (FH_pEarcan)
	ENDIF
	
	IF (storeProbing) THEN
		CLOSE (FH_probing)
	ENDIF

	!IF (storeCochlea) THEN
	!	CLOSE (FH_cochlea) 
	!ENDIF
END SUBROUTINE CloseWrite
