!This module is shared by "Cochlea" and "CochleaDialog"

! Module contains variables and constants for all values concerning the
! real cochlea and middle ear, including parameters for Zweig-impedance

MODULE CochleaParameters
	IMPLICIT NONE

	! parameter(1) is human value; parameter(2) = guinea pig value
	INTEGER, PARAMETER :: parameterSet= 1 !model is now set to always human
	! cochlea parameters
	REAL(8), PARAMETER :: cochleaLength (2) = [35d-3, 18.5d-3]   !m
	REAL(8), PARAMETER :: bmMass(2) = [0.5d0, 0.25d0]	! kg/m2
	REAL(8), PARAMETER :: bmImpedanceFactor(2)	= [1d0, 0.75d0]
	REAL(8), PARAMETER :: scalaWidth(2) = [1d-3, 0.5d-3]	!m
	REAL(8), PARAMETER :: scalaHeight(2) = [1d-3, 0.5d-3]	!m
	REAL(8), PARAMETER :: helicotremaWidth(2) = [1d-3, 0.5d-3]	!m
	REAL(8), PARAMETER :: rho = 1d3		!kg/m3
	REAL(8), PARAMETER :: Normal_Q = 20d0
	REAL(8) bm_mass	! kg/m2
	REAL(8) bm_width	! m
	REAL(8) bm_length	! m
        REAL(8) ZweigMso        ! kg/m5 sv
        REAL(8) ZweigMpo        ! kg/m3 sv
        REAL(8) ZweigL          ! [] exp. cochlear map base ln sv
        REAL(8) ZweigOmega_co   ! rad : char. angular freq. at base sv
        REAL(8) Ko              ! kg/s2m3 sv
        ! The previous parameters are calculated in InitializeCochlea.f90
        ! If not already given a value above

	! Frequency map parameters, x measured from stapes
	! Exponential:  f(x) = A * 10 ^ (-alpha x)
	!REAL(8), PARAMETER :: Exponential_A (2) = [22507d0, 38346d0]	!Hz
	!REAL(8), PARAMETER :: Exponential_alpha (2) = [65.1d0, 113.5d0]	!1/m

	! Greenwood:  f(x) = A * 10 ^ (-alpha x) - B
	REAL(8), PARAMETER :: Greenwood_A (2) = [20682d0, 43765d0]	!Hz
	REAL(8), PARAMETER :: Greenwood_alpha (2) = [61.765d0, 113.5d0]	!1/m (=2.1/bmlength)
	REAL(8), PARAMETER :: Greenwood_B (2) = [140.6d0, 297.5d0]	!Hz

	! Middle Ear parameters
	REAL(8), PARAMETER :: stapesArea(2) = [3d-6, 0.81d-6]    !m2
	REAL(8), PARAMETER :: EardrumArea(2) = [60d-6, 23.9d-6]    !m2
	REAL(8), PARAMETER :: MiddleEarResonanceFrequency(2)= [2d3, 4d3] !Hz
	REAL(8), PARAMETER :: MiddleEarQualityFactor = 0.4d0
	REAL(8), PARAMETER :: SpecificAcousticImpedanceOfAir = 415d0     !N s/m3
	REAL(8), PARAMETER :: middleEarTransformer(2) = [30d0, 39.3d0]
	REAL(8), PARAMETER :: damping_coupler(2) = [140d5, 280d5]	!Ns/m5 =kg/sm4= Pa/(m/s)
	REAL(8), PARAMETER :: mass_coupler(2) = [43.4d2, 21.7d2]	!kg/m4
	REAL(8), PARAMETER :: stiffness_coupler= 1d0/2.28d-11  !kg/s2m4

	REAL(8), PARAMETER :: p0    = 2d-5  !Pa

        !Zweig parameters linear
	REAL(8), PARAMETER :: ZweigQ = 1d0/-0.0606d0 !pole at 0.061d0  !1.0d0 /-0.0802d0 !for passive:1/0.3197
	REAL(8), PARAMETER :: ZweigRho = 0.0935d0 ! 0.1041d0 !for passive: 0.0111
	REAL(8), PARAMETER :: ZweigFactor = 1.7435d0!  !1.7441d0 !for passive: 1.7654
	REAL(8), PARAMETER :: ZweigQAtBoundaries = 20d0
	REAL(8), PARAMETER :: ZweigBeta = 10000d0
	REAL(8), PARAMETER :: ZweigGamma = 6200.d0
        REAL(8), PARAMETER :: ZweigN = 1.5 !sv
        
        !Shera parameters nonlinear
        REAL(8), PARAMETER :: SheraMuMax = 4.3d0!!corresponds to max possible delay, corresponding to max passive pole (highest pole value 0.95 here, this makes that the starting pole max in parameter file should not be set to more than 0.3 when choosing a nonlinear model). This value sets the length of the memory storage when using variable Mu in the nonlinearity

        !RMS compensation reference: is the rms of the stimulus 
        !that was used to design the nonlinearity thresholds.
        !Was a 20 ms cosine windowed 1 kHz internal sinusoid in our case
        !rms calculated on the -1 to +1 stimulus range (MatLab) !sv
        REAL(8), PARAMETER :: RMSref = 0.6124d0 !is not used for now
	INTEGER, PARAMETER :: plotEvery_n_points=1
	INTEGER, PARAMETER :: plotEvery_t_points=16
END MODULE CochleaParameters
