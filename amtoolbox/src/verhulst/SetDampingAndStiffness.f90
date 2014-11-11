! m = bm_mass / area
! d = damping / bm_mass
! s = stiffness / bm_mass

SUBROUTINE SetDampingAndStiffness

	USE Declare
	IMPLICIT NONE
	REAL(8) f_resonance(0:n)

        !s for Zweigimplementation(linear or nonlinear) is not defined here(sv)
        !but is written directly in g(i) in RK4 as stot(sv) 
        !d for Zweiglinear is implemented here as Zweigd, 
        !with or without random cochlear irregularities
        !d for Zweignonlinear is determined in RK4 as dtot(sv)
        !because the damping term is made dynamic
        !the factor by which it is multiplied (Sherad_factor) in RK4  
        !is calculated here with or without nonlinearities
        
        ! Determine frequencies for all sections using the Greenwood map
	f_resonance = Greenwood_A(parameterSet) * 10 ** (-Greenwood_alpha(parameterSet) * x) - Greenwood_B(parameterSet)
	omega = 2.d0 * pi * f_resonance
            
          !damping multiplication factor for nonlinear Zweig implemenation
          !The Q or SheraDamping is calculated in RK4
          !because it is made dynamic, so here is only determined
          !a factor that is multiplied with Sherad (i.e. with 1/SheraQ) in RK4
          Sherad_factor(1:n) = omega(1:n)
   
END SUBROUTINE SetDampingAndStiffness
