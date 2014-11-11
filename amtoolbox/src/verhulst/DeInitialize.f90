SUBROUTINE DeInitialize (DeAllocateFirstPart)
!	USE SampleDownModule
	USE Declare
!	USE WindowsModule
	USE WaveReadModule
!	USE GraphsModule
	IMPLICIT NONE
	LOGICAL, INTENT(IN) :: DeAllocateFirstPart

	IF (DeAllocateFirstPart) THEN

		DEALLOCATE (gamma, STAT = err)
	!	DEALLOCATE (d, STAT = err)
		DEALLOCATE (x, STAT = err)
		DEALLOCATE (ZweigRhoAdapted, STAT = err)
		DEALLOCATE (ZweigDampingAdaptation, STAT = err)
		DEALLOCATE (delay, STAT = err)
		DEALLOCATE (delay_deviation, STAT = err)
		DEALLOCATE (Yzweig, STAT = err)
		DEALLOCATE (ZweigSample1, STAT = err)
		DEALLOCATE (ZweigSample2, STAT = err)
                DEALLOCATE (Zwp, STAT = err)
		DEALLOCATE (Ybuffer, STAT = err)
                DEALLOCATE (ZASQ, STAT = err) !sv
                DEALLOCATE (ZASC, STAT = err) !sv
                DEALLOCATE (ZAH, STAT = err) !sv
                DEALLOCATE (ZAL, STAT = err) !sv
                DEALLOCATE (Zweigd, STAT = err) !sv
                DEALLOCATE (ZweigMs, STAT = err) !!sv
                DEALLOCATE (ZweigMp, STAT = err) !!sv 
                !DEALLOCATE (Sherad, STAT = err) !sv
                DEALLOCATE (Sherad_factor, STAT = err) !sv
                !DEALLOCATE (SheraP, STAT = err) !sv
                !DEALLOCATE (SheraRho, STAT = err) !sv
                !DEALLOCATE (SheraMu, STAT = err) !sv
                DEALLOCATE (delay, STAT = err) !sv
                DEALLOCATE (delay_deviation, STAT = err) !sv
                DEALLOCATE (Yzweig, STAT = err) !sv
                DEALLOCATE (ZweigSample1, STAT = err) !sv
                DEALLOCATE (ZweigSample2, STAT = err) !sv
                DEALLOCATE (PuriaInxmin1, STAT = err) !sv
                DEALLOCATE (PuriaInxmin2, STAT = err) !sv
                DEALLOCATE (PuriaInymin1, STAT = err) !sv
                DEALLOCATE (PuriaInymin2, STAT = err) !sv
!		DEALLOCATE (Ytmp, STAT = err)
!		DEALLOCATE (Vtmp, STAT = err)
!		DEALLOCATE (M1234, STAT = err)
!		DEALLOCATE (M2, STAT = err)
!		DEALLOCATE (M3, STAT = err)
!		DEALLOCATE (M4, STAT = err)
!		DEALLOCATE (g, STAT = err)
		DEALLOCATE (q, STAT = err)
		DEALLOCATE (b, STAT = err)
		DEALLOCATE (k, STAT = err)
!		DEALLOCATE (previousPosition, STAT = err)
!		DEALLOCATE (previousVelocity, STAT = err)
!		DEALLOCATE (LastPositionPoints, STAT = err)
!		DEALLOCATE (LastVelocityPoints, STAT = err)
!		DEALLOCATE (window, STAT = err)

	ELSE
                DEALLOCATE (d, STAT = err)
		DEALLOCATE (s, STAT = err)
		DEALLOCATE (Y, STAT = err)
		DEALLOCATE (V, STAT = err)
                DEALLOCATE (g, STAT = err)
                DEALLOCATE (stot, STAT = err) !!sv
                DEALLOCATE (dtot, STAT = err) !!sv
                DEALLOCATE (omega, STAT = err) !sv
                DEALLOCATE (debug, STAT = err) !sv
               	DEALLOCATE (Ytmp, STAT = err)
		DEALLOCATE (Vtmp, STAT = err)
		DEALLOCATE (ResampledWaveData, STAT = err)
                DEALLOCATE (SheraP, STAT = err) !sv
                DEALLOCATE (SheraRho, STAT = err) !sv
                DEALLOCATE (SheraMu, STAT = err) !sv
                DEALLOCATE (Sherad, STAT = err) !sv
!		DEALLOCATE (section_axis, STAT = err)
!		DEALLOCATE (membrane_y_pixel, STAT = err)
!		DEALLOCATE (white, STAT = err)
!		DEALLOCATE (black, STAT = err)
!		DEALLOCATE (PositionPhase, STAT = err)
!		DEALLOCATE (VelocityPhase, STAT = err)
!		DEALLOCATE (velocityMatrix, STAT = err)
!		DEALLOCATE (positionMatrix, STAT = err)
	
	ENDIF

END SUBROUTINE DeInitialize
