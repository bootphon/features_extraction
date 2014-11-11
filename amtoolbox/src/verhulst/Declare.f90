!This module is shared by "Cochlea" and "CochleaDialog"

MODULE Declare
	USE ParametersModule
	USE CochleaParameters

	REAL(8) t		! simulated time
	REAL(8) dt		! computational time step
	REAL(8) t_LastPeriod	! Start time of last period of signal 1, used in FindPhase
	REAL(8) phi1, phi2

	!variabelen voor RK4 en Initialize
	REAL(8) Asq, Asq0
        REAL(8), ALLOCATABLE :: ZASQ(:) !!sv
        REAL(8), ALLOCATABLE :: ZASC(:) !!sv
        REAL(8), ALLOCATABLE :: ZAH(:)  !!sv
        REAL(8), ALLOCATABLE :: ZAL(:)  !!sv
        REAL(8), ALLOCATABLE :: debug(:) !!sv a variable that can be written out
	REAL(8), ALLOCATABLE :: x(:)	! position of section in meters from base
        !Middle ear variables
	REAL(8) gam0
	REAL(8) d_m_factor
	REAL(8) s_m_factor
        !REAL(8) Rme
        REAL(8) Mme
        REAL(8) RK4_0
        REAL(8) RK4G_0

        !Puria IN/OUT ME M1 and M2 filtering
        REAL(8), ALLOCATABLE :: PuriaInxmin1(:) !one for every offset RK4 value
        REAL(8), ALLOCATABLE :: PuriaInxmin2(:)
        REAL(8), ALLOCATABLE :: PuriaInymin1(:)
        REAL(8), ALLOCATABLE :: PuriaInymin2(:)
        REAL(8) PuriaOutxmin1 !M2
        REAL(8) PuriaOutxmin2
        REAL(8) PuriaOutymin1
        REAL(8) PuriaOutymin2

	!Zweig 
	REAL(8), ALLOCATABLE :: ZweigRhoAdapted(:)
	REAL(8), ALLOCATABLE :: ZweigDampingAdaptation(:)
	REAL(8), ALLOCATABLE :: Ybuffer(:)
	REAL(8), ALLOCATABLE :: Yzweig(:)
	INTEGER, ALLOCATABLE :: delay(:)				! delay in integer number of samples
        REAL(8), ALLOCATABLE :: ZweigMs(:), ZweigMp(:) !!sv
        REAL(8), ALLOCATABLE :: delay_deviation(:)		! deviation from exact delay, nessecary for interpolation
	INTEGER, ALLOCATABLE :: ZweigSample1(:), ZweigSample2(:)	!indices of last and last but one value in Ybuffer for every section
	INTEGER, ALLOCATABLE :: Zwp(:) !pointer for dynamic zweigbuffer for nonlinear model with varying mu delay
        REAL(8) zweig_Relative_Q_minus_1

        !Shera parameters for nonlinearity
        REAL(8), ALLOCATABLE :: Sherad(:) !sv
        REAL(8), ALLOCATABLE :: Sherad_factor(:) !sv
        REAL(8), ALLOCATABLE :: SheraP(:) !sv
        REAL(8), ALLOCATABLE :: SheraRho(:) !sv 
        REAL(8), ALLOCATABLE :: SheraMu(:) !sv
    
	!variabele voor WriteMembraneStatus, InitializeGraph
	LOGICAL l_nostim, l_noprobe

	REAL(8), PARAMETER :: pi =   3.1415926535897932384626433832795028841971693993751d0

	INTEGER :: kmax, kk, i

	REAL(8) half_dt
	REAL(8) dx
	REAL(8) ms_ME
	REAL(8) ds_ME
	REAL(8) ss_ME
	REAL(8) r_Xtr0
	REAL(8) m0_RK4
	REAL(8) F0		!in RK4 en WriteValuesPEarcan
	REAL(8) phi		! used to be KHTfactor
	REAL(8) q0_factor
	REAL(8) g0_factor			
	REAL(8) Y0_factor
	REAL(8) stimulus0
        REAL(8) Qbase

	REAL(8), ALLOCATABLE :: xplot(:), yplot(:), yplot0(:)
	REAL(8), ALLOCATABLE :: s(:), d(:)
        REAL(8), ALLOCATABLE :: stot(:)  !!sv
        REAL(8), ALLOCATABLE :: dtot(:)  !!sv
        REAL(8), ALLOCATABLE :: Zweigd(:)!!sv 
        REAL(8), ALLOCATABLE :: omega(:) !!sv 
	!Y(0) and V(0) are the displacement and the velocity of the stapes
	!Y(1:n) and V(1:n) are the displacements and the velocities of the sections 1..n
	REAL(8), ALLOCATABLE :: Y(:),V(:)
	REAL(8), ALLOCATABLE :: Ytmp(:),Vtmp(:) 

!	REAL(8), ALLOCATABLE :: M1234(:,:) !, M2(:), M3(:), M4(:) these are non-global, only in RK4, redefined there
	REAL(8), ALLOCATABLE :: g(:)	
	REAL(8), ALLOCATABLE :: q(:)       
	REAL(8), ALLOCATABLE :: b(:)
	REAL(8), ALLOCATABLE :: k(:)
	REAL(8), ALLOCATABLE :: gamma(:)

	INTEGER err

END MODULE Declare
