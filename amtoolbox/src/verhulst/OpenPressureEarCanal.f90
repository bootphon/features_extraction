! Opens <PressureEarCanalFileName> in <outputDirectory>
! If <storeExtraInfo>, a header row will be written, indicating
! what values will be written in each colum, that is: "time (s)"
! and "pressure Ear Canal (Pa)"

SUBROUTINE OpenPressureEarCanal
	USE Declare
	USE FilesModule
	IMPLICIT NONE
	INTEGER ios

	IF (storePressureEarCanal) THEN
		OPEN (FH_pEarcan, FILE = TRIM(OutputDirectory) // TRIM(PressureEarCanalFileName), FORM = writeFormat, IOSTAT = ios)

	ENDIF
END SUBROUTINE OpenPressureEarCanal
