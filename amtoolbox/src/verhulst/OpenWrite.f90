! Opens <PressureEarCanalFileName> in <outputDirectory>
! If <storeExtraInfo>, a header row will be written, indicating
! what values will be written in each colum, that is: "time (s)"
! and "pressure Ear Canal (Pa)"

SUBROUTINE OpenWrite
	USE Declare
	USE FilesModule
	IMPLICIT NONE
	INTEGER ios
	INTEGER iosec

	IF (storePressureEarCanal) THEN
		OPEN (FH_pEarcan, FILE = TRIM(OutputDirectory) // TRIM(PressureEarCanalFileName), FORM = writeFormat, IOSTAT = iosec)
	ENDIF
	
	IF (storeProbing) THEN
		OPEN (FH_probing, FILE = TRIM(outputDirectory) // TRIM(probingFileName), FORM = writeFormat, IOSTAT = ios)
	ENDIF

	!IF (storeCochlea) THEN
	!	OPEN (FH_cochlea, FILE = TRIM(outputDirectory) // TRIM(CochleaFileName), FORM = writeFormat, IOSTAT = ios)

	!ENDIF
END SUBROUTINE OpenWrite
