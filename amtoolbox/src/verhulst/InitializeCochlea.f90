SUBROUTINE InitializeCochlea
	USE Declare
	IMPLICIT NONE
 
	bm_length = cochleaLength(parameterSet) - helicotremaWidth(parameterSet)
	bm_width = scalaWidth(parameterSet)
	bm_mass = bmMass(parameterSet) * bmImpedanceFactor(parameterSet)
        !extra parameters are defined here for Zweigimplementation {sv}
        !Ms and Mp depend on the location in the Zweigimplementation (tapering)
        !Mpo is related to Mso by (4N)^2=(ZweigL*Mso) /(Mpo)
        !reference to Zweig(1990)
        ZweigMso = 2.d0 * rho / ( bm_width * scalaHeight(parameterSet) )!kg/m5 
        ZweigL = 1.d0 / (2.303d0 * Greenwood_alpha(parameterSet))
        ZweigOmega_co = 2.d0 * pi * Greenwood_A(parameterSet)
      
        ZweigMpo = (ZweigMso * (ZweigL ** 2.d0)) / ((4.d0 * ZweigN) ** 2.d0)!kg/m3
        Ko = ZweigMpo * (ZweigOmega_co ** 2.d0) !kg/m3s2
        !ZweigMs(i) and ZweigMp(i) are calculated in initializeGausselimination
        !because they depend on omega and the cochlear map

        !calculate the distances from the stapes in m
	dx = bm_length / n
	DO i = 0, n
		x(i) = dx * i !is in m
        ENDDO
               
END SUBROUTINE InitializeCochlea
