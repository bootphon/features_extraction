SUBROUTINE Initialize
	USE Declare
	IMPLICIT NONE

	ALLOCATE (	s(0:n), &
			d(0:n), &
			x(0:n), &
			Y(0:n), &
			V(0:n), &
			Ytmp(0:n), &
			Vtmp(0:n), &
!			M1234(4,0:n), &
!			M2(0:n), &
!			M3(0:n), &
!			M4(0:n), &
			g(0:n), &
                        q(0:n), &
                        b(0:n), &
			k(0:n), &
                        debug(0:n), & !sv
                        dtot(0:n), & !sv
                        stot(0:n), & !sv
                        Zweigd(0:n), &!sv 
                        ZweigMs(0:n), &!sv
                        ZweigMp(0:n), &!sv
                        omega(0:n), & !sv
                        ZASQ(0:n), & !sv
                        ZASC(0:n), & !sv
                        ZAH(0:n), & !sv
                        ZAL(0:n), & !sv
                        Sherad(1:n), & !sv
                        Sherad_factor(0:n), & !sv 
                        SheraRho(1:n), & !sv
                        SheraMu(1:n), & !sv
                        SheraP(1:n), & !sv
                        delay(1:n), & !sv
                        delay_deviation(1:n), & !sv
                        Yzweig(1:n), & !sv
                        ZweigSample1(1:n), & !sv
                        ZweigSample2(1:n), & !sv
                        Zwp(1:n), & !sv
                        PuriaInxmin1(1:3), & !sv
                        PuriaInxmin2(1:3), & !sv
                        PuriaInymin1(1:3), & !sv
                        PuriaInymin2(1:3), & !sv

			STAT = err)

	IF (err /= 0) CALL AllocationError

	dt = 1 / ComputationalFrequency
	half_dt = dt / 2
	kmax = INT(t_max / dt)
	t_LastPeriod = t_max - 1d0/signal1Frequency

	CALL InitializeFiles
	CALL InitializeStimulus
	CALL InitializeCochlea
	CALL InitializeMiddleEar
	CALL SetDampingAndStiffness 
	CALL InitializeZweig
	CALL InitializeGaussElimination

END SUBROUTINE Initialize
