
subroutine RK4
	use Declare
	IMPLICIT NONE

	REAL(8), PARAMETER	:: Kgs=9.6d-6
	REAL(8), PARAMETER	:: kT = 300 * 1.3807d-23
	REAL(8), PARAMETER	:: Yc(0:2) = (/-45d-9, 15d-9, 30d-9/)
	REAL(8)			:: U_helicotrema	!Volume velocity	[m3/s]
	INTEGER(4)		   :: RK4step,iRK4
	REAL(8), DIMENSION (0:n,4) :: M1234
       ! REAL(8), DIMENSION(0:4) :: aa
       ! REAL(8), DIMENSION(0:4) :: bb
       ! REAL(8), DIMENSION(0:4) :: cc
        REAL(8), DIMENSION(0:n) :: right
        INTEGER  matsize
        REAL(8), DIMENSION(0:n) :: Qsol
        REAL(8)  Rh
        REAL(8)  Uh
        REAL(8)  ApexFactor
        INTEGER, dimension(2) :: seed
        REAL(8)  Pvar(1:n)
   !     REAL(8)  Rand(1:n)


  !  seed(1)=Subjectnr * 29
  !  seed(2)=Subjectnr + 2010 * 08                
  !  CALL RANDOM_SEED(PUT=seed) 
  !  CALL RANDOM_NUMBER(Pvar(1:n))
  !  DO i=0,n/2
  !       Rand(2*i)= 0.05d0 * (Pvar(2*i) - 0.5d0)
  !       Rand(2*i-1)= 0.05d0 * (Pvar(2*i) - 0.5d0)
  !  ENDDO 


   IF (t == 0) THEN 
     SheraP(1:n)= SheraPo
     CALL SheraParameters     
     CALL ZweigImpedance
   ELSE
     !the poles that go into this fuction were determined 
     !at the end of the previous time step (depend on V and Y)
     !comment CALL poleset  back in/out when doing polesetting 
     CALL SheraParameters 
     CALL ZweigImpedance     
   ENDIF 
 
  do RK4step=1,4

	select case (RK4step)
	case (1)
  		F0=Stimulus(0)
		Ytmp = Y
		Vtmp = V
	case (2)
		F0=Stimulus(1)
		Ytmp = Y + Vtmp * half_dt
		Vtmp = V + M1234(0:n,1) / 2.d0
	case (3)
		Ytmp = Y + Vtmp * half_dt
		Vtmp = V + M1234(0:n,2) / 2.d0
	case (4)
		F0=Stimulus(2)
		Ytmp = Y + Vtmp *dt
		Vtmp = V + M1234(0:n,3)
	end select

        
	CALL Calculate_g
         
         !test version for Gaussian elimination
         !aa(0:4)=(/0d0, 1d0, 2d0, 3d0, 4d0/) !bottom diagonal (should start w 0)
         !bb(0:4)=(/3d0, 5d0, 6d0, 7d0, 7d0/) !middle diagonal
         !cc(0:4)=(/9d0, 8d0, 3d0, 8d0, 0d0/) !top diagonal (should end w 0)
         !vv(0:4)=(/2d0, 2d0, 2d0, 2d0, 2d0/) !right side
         !matrow=5d0
         !Qsol=-3.57 1.38 -0.17 0.10 0.22
         
         right(0)  =  g(0) + p0x * F0
         right(1:n)=  ZASQ(1:n)*g(1:n)
        ! IF(useApexShortcut) THEN
            right(n)=ZASQ(n)*g(n)
         ! ELSE !with matching impedance at helicotrema
         !   Rh=DSQRT(ZweigMs(n)*ZweigMp(n)*(omega(n)**2d0)) !sqrt(Msn*Kn) kg/m4
         !   Uh=(stapesArea(parameterset)*Vtmp(0))-SUM(Vtmp(1:n)*bm_width*dx)!m3/s
         !   ApexFactor=(omega(n)*ZweigMs(n-1))/(bm_width*ZweigOmega_co*ZweigMpo)![1/m3]
         !   right(n)=ZASQ(n)*g(n)!+ApexFactor*Rh*Uh ![kg/m4s2]
         !ENDIF    
         
         matsize=n+1d0 !n cochlear sections + 1 ME equation
         CALL SolveTridiag(ZAL,ZASC,ZAH,right,matsize,Qsol)
         !this funtion solves the tridiagonal matrix system
         !gives the solution q vector
         !debug(1:n)=Qsol(0:n-1)
         !q(0:n)=Qsol(0:n) ! PASS ON VARIABLE TO WRITE OUT
       
         ! calculate over vertical sections p=M Abm + R Vbm + K Ybm for 
         !Abm= A* abm and M1234 = abm * dt
         M1234(0,RK4step) = (RK4_0 * Qsol(0) + RK4G_0 * (g(0) + p0x * F0)) * dt !works 
         M1234(1:n,RK4step) = (Qsol(1:n)-g(1:n)) * dt

!	if(RK4step==1) CALL WritePressureEarCanal

  enddo !RK4step
               
        !Result of the Runge-Kutta: as in the original implementation, was checked and works !sv
	Y = Y + (V + (M1234(0:n,1)+ M1234(0:n,2)+M1234(0:n,3)) / 6.d0) * dt
        V = V + (M1234(0:n,1) + 2.d0 * (M1234(0:n,2)+M1234(0:n,3)) + M1234(0:n,4)) / 6.d0

        CALL PoleCalculation 
        !function that calculates the poles for the next iteration

        !this is to store the stimulus 
        stimulus0=F0 !writes it with offset = 2, thus one sample delay
        Qbase=Qsol(0)!this gives the Q at the base forward to output stage
 
!!END MAIN RK4 ROUTINE
CONTAINS
!

  REAL(8) FUNCTION Stimulus (offset)
	! offset is numer of half_dt's from current time t, e.g. offset = 1 means: time = t + 1 * half_dt
	! if useAudioFile, function returns sample (2*kk+offset * half_dt) from resampled audiofile
	! audiofile must be resampled to 2 * ComputationalFrequency, so the sample
	! corresponds with t = t + offset * half_dt
	! if not useAudioFile function composes stimulus at t = t + offset * half_dt

	USE WaveReadModule

	IMPLICIT NONE 

	INTEGER, INTENT(IN) :: offset

	INTEGER index
        REAL (8) RMScomp
        REAL (8) Stim !is stimulus before filtering with M1
        REAL (8) PuriaM1

	IF (useAudioFile) THEN
                RMScomp = audioFileRMS !RMSref / audioFileRMS is out for now
		index = 2 * kk + offset
                !index (2*kk+offset) and (t+offset*half_dt) are equivalent
                !because t=dt*kk
                !offset represents the step in the RK4 algorithm
		IF (index < ResampledWaveDataPoints) THEN
			Stim = RMScomp * audioFilePressure * DBLE(ResampledWaveData(index))
                        
                        !resampledwavedata is the original stimulus w  fs, 
                        !resampled to 2fs,thus double amount of samples than original
                        !offset determines with sample is picked dt(0), dt+dt/2(1) or 2dt(2)
                        !Stim has in the end an fs of the original 
                        !as only 1/2 samples are picked from ResampledWaveData        
                     
                        Stimulus=PuriaM1(offset,Stim)
                        !This is the Puria2003 M1 filter, 
                        !and voltagedivision compensation
                        !is implemented for every offset value
                        !use (1) for offset 0, (2) for offset 1, 3 for offset (3)


                ELSE
			Stimulus = 0d0

		ENDIF
	ELSE
		Stim = ComposeStimulus(t + offset * half_dt)
                Stimulus=PuriaM1(offset,Stim)
                !This is the Puria2003 M1 filter, 
                !and voltagedivision compensation
                !is implemented for every offset value
                !use (1) for offset 0, (2) for offset 1, 3 for offset (3)

	ENDIF
 END FUNCTION Stimulus

!!!!!!!!!!!!!!!!

  REAL(8) FUNCTION ComposeStimulus (time)
	! compose stimulus at t=time
    USE  Declare
	IMPLICIT NONE 
	REAL(8), INTENT(IN) :: time !contents of the 
	REAL(8) signal, signal1, signal2, signal3
	REAL(8) onsetWindow
	INTEGER pulseCounter /0/
	REAL(8) t_pulse

	signal = 0.d0
	IF (useSignal1) signal = signal1Pressure * COS(signal1AngularFrequency * time + phi1)
	IF (useSignal2) signal = signal + signal2Pressure * COS(signal2AngularFrequency * time + phi2)
	IF (useSignal3) THEN
		t_pulse = time - DBLE(pulseCounter) * signal3Period
		IF (t_pulse < signal3Duration) THEN
			signal = signal + signal3Pressure
		ELSEIF (t_pulse >= signal3Period) THEN
			pulseCounter = pulseCounter +1
		ENDIF
	ENDIF

	IF (time < onsetDuration) THEN
		onsetWindow = (1.d0 - COS(pi * time / onsetDuration)) / 2.d0
		! Peter van Hengel:
		! onsetWindow = DEXP(-((onsetDuration - time)*1d3) ** 2)
		ComposeStimulus = signal * onsetWindow
	ELSE
		ComposeStimulus = signal
	END IF  
        
  END FUNCTION ComposeStimulus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! In linear case, g = d * V +s * y
! In nonlinear cases, the damping and/or the stiffness depend on velocity and position respectively.
! In the case of Zweig Impedance, the negative damping will be stabilized by a delayed feedback force
  	SUBROUTINE Calculate_g
	IMPLICIT NONE
	REAL(8) tmp (n)
	REAL(8) absV (n) 
        REAL(8) Rand(n)
        
        g(0) = d_m_factor * Vtmp(0)
        !debug(1:n)=Vtmp(0)
        !the g(0) will depend on ME properties, see InitializeMiddleEar

	dtot(1:n) = Sherad_factor(1:n) * Sherad(1:n) 
        stot(1:n) = (omega(1:n) ** 2.d0) * (Ytmp(1:n) +( SheraRho(1:n) * Yzweig(1:n) ))
        g(1:n) = (dtot(1:n) * Vtmp(1:n)) + stot(1:n)
 
	RETURN
	END SUBROUTINE Calculate_g

END SUBROUTINE RK4
