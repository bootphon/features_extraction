SUBROUTINE InitializeMiddleEar
	USE Declare
	IMPLICIT NONE

	REAL(8) stapes_area
	stapes_area = stapesArea(parameterSet)

	   !the ME represents a resistance Rme
           !that equals the cochlear input impedance at low frequencies. 
           !By doing this, there are little reflections of cochlear energy
           !back into the model, as it absorbs the energy at the stapes.
                 
                  Mme=1d0 !dummy parameter
	        q0_factor = ZweigMpo * bm_width
                p0x = (ZweigMso * dx) / (Mme * ZweigMpo * bm_width )
                d_m_factor = - p0x * stapes_area * Rme
                RK4_0 = -(bm_width * ZweigMpo)/(Mme * stapes_area) !for RK4 method 
                RK4G_0= (ZweigMpo * bm_width)/(ZweigMso * stapes_area * dx)
END SUBROUTINE InitializeMiddleEar
