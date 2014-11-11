! Opens <probingFileName> in <outputDirectory>
! If <storeExtraInfo>, header rows will be written, indicating what values will be written in each colum

SUBROUTINE OpenProbing
	USE Declare
	USE FilesModule
	IMPLICIT NONE
	CHARACTER(12) IntToString
	INTEGER ios

	IF (storeProbing) THEN
		OPEN (FH_probing, FILE = TRIM(outputDirectory) // TRIM(probingFileName), FORM = writeFormat, IOSTAT = ios)

!		IF (storeExtraInfo) THEN!
			!Header row 1 (legend)
!			WRITE (FH_probing, *, IOSTAT = ios) "Y = position (m), V = Velocity (m/s); number indicates probe number"

			!Header row 2 (Y1, Y2, Y3, ... V1, V2, V3, ...)
!			WRITE (FH_probing, '($A15)', IOSTAT = ios) ""
!			DO i = 1, 10
!				IF (probes(i) > 0 .AND. probes(i) <=n) WRITE (FH_probing, '($A13)', IOSTAT = ios) "Y_" // TRIM(IntToString(i))
!			ENDDO
!			DO i = 1, 10
!				IF (probes(i) > 0 .AND. probes(i) <=n) WRITE (FH_probing, '($A13)', IOSTAT = ios) "V_" // TRIM(IntToString(i))
!			ENDDO
!			WRITE (FH_probing, *, IOSTAT = ios)

			!Header row 3 ("time (s)", section numbers)
!			WRITE (FH_probing, '($A15)', IOSTAT = ios) "Time (s)"
!			DO i = 1, 10
!				IF (probes(i) > 0 .AND. probes(i) <=n) WRITE (FH_probing, '($A13)', IOSTAT = ios) "Section:" // TRIM(IntToString(probes(i)))
!			ENDDO
!			DO i = 1, 10
!				IF (probes(i) > 0 .AND. probes(i) <=n) WRITE (FH_probing, '($A13)', IOSTAT = ios) "Section:" // TRIM(IntToString(probes(i)))
!			ENDDO
!			WRITE (FH_probing, *, IOSTAT = ios)
!		ENDIF

	ENDIF
END SUBROUTINE OpenProbing
