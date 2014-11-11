! Writes current time <t>, BM-displacement <Y> and BM-velocity <V>
! of each probe-section to probe file (each call writes one row of data)
! t = x(:, 1)
! Y1 = x(:, 2)
! Y2 = x(:, 3)
! ....
! V1 = x(:, n+2)
! ....
! Vn = x(:, 2*n+1)

SUBROUTINE WriteData
	USE Declare
	USE GraphsModule
	USE FilesModule
	IMPLICIT NONE
	INTEGER ios
	INTEGER iosec
        REAL(8) pressureStapes
	REAL(8) pressureEarCanal
        REAL(8) PuriaM2
	REAL(8) stapes_area
	INTEGER o,p

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


                DO i = 1, 20
                   IF (probes(i) > 0 .AND. probes(i) <=n) THEN
				
			WRITE (FH_probing, '($ES13.3)', IOSTAT = ios) SheraP(probes(i))

			ENDIF
		ENDDO

		WRITE (FH_probing, *, IOSTAT = ios)

         ENDIF

	IF (storePressureEarCanal) THEN
 		stapes_area = stapesArea(parameterSet)

		! Calculation of the pressure at the ear drum (ear canal)
           	pressureStapes =  Qbase * q0_factor
           	pressureEarCanal = PuriaM2(pressureStapes)
	
		WRITE (FH_pEarcan, '(ES15.7, ES26.7, ES26.7)', IOSTAT = iosec) t, pressureEarCanal, stimulus0 
	ENDIF

!	IF(storeCochlea) THEN
!	DO o=0, 1249
!!		WRITE (*,*) o
!		DO p=0,399
!			WRITE (FH_cochlea,'(1X, 1ES12.4)') positionMatrix(p,o)
!!		WRITE (*,*) positionMatrix(u,t_plotCounter)
!		END DO
!		WRITE (FH_cochlea,'(/)')
!	END DO
!	ENDIF
END SUBROUTINE WriteData
