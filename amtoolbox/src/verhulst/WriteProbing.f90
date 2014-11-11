! Writes current time <t>, BM-displacement <Y> and BM-velocity <V>
! of each probe-section to probe file (each call writes one row of data)
! If <storeBinary>, the values will be written as real(4)'s in a binary file.
! To process the binary data in Matlab use:
! fid = fopen ( '    probing file name    ' ) 
! x = fread(fid, [2n+1, inf], 'real*4')		%n indicating the number of probes
! t = x(:, 1)
! Y1 = x(:, 2)
! Y2 = x(:, 3)
! ....
! V1 = x(:, n+2)
! ....
! Vn = x(:, 2*n+1)

SUBROUTINE WriteProbing
	USE Declare
	USE GraphsModule
	USE FilesModule
	IMPLICIT NONE
	INTEGER ios

	IF (storeProbing) THEN
		
		DO i = 1, 20
			IF (probes(i) > 0 .AND. probes(i) <=n) THEN
				
!					
				WRITE (FH_probing, '($ES13.3)', IOSTAT = ios) Y(probes(i))
			
			ENDIF
		ENDDO

		DO i = 1, 20
			IF (probes(i) > 0 .AND. probes(i) <=n) THEN
				
				WRITE (FH_probing, '($ES13.3)', IOSTAT = ios) V(probes(i))

			ENDIF
		ENDDO

!new part sv: to write the poles for a certain section
                DO i = 1, 20
                   IF (probes(i) > 0 .AND. probes(i) <=n) THEN
				
			WRITE (FH_probing, '($ES13.3)', IOSTAT = ios) SheraP(probes(i))

			ENDIF
		ENDDO

!new part sv: to write the pressure for a certain section
!                DO i = 1, 20
!                   IF (probes(i) > 0 .AND. probes(i) <=n) THEN
!				IF (storeBinary) THEN
!					WRITE (FH_probing, IOSTAT = ios) (((bm_width * ZweigOmega_co * ZweigMpo) / omega(i) )  * q(probes(i)))
!				ELSE
!					WRITE (FH_probing, '($ES13.3)', IOSTAT = ios)(((bm_width * ZweigOmega_co * ZweigMpo) / omega(i) )  * q(probes(i)))
!				ENDIF
!			ENDIF
!		ENDDO

!!new part sv: to write the pressure for a certain section
!                DO i = 1, 20
!                   IF (probes(i) > 0 .AND. probes(i) <=n) THEN
!				IF (storeBinary) THEN
!					WRITE (FH_probing, IOSTAT = ios) (stot(probes(i)))
!				ELSE
!					WRITE (FH_probing, '($ES13.3)', IOSTAT = ios)(stot(probes(i)))
!				ENDIF
!			ENDIF
!		ENDDO
		
!                IF (.NOT. storeBinary) THEN 
!                   WRITE (FH_probing, *, IOSTAT = ios
!		ENDIF
             ENDIF
END SUBROUTINE WriteProbing
