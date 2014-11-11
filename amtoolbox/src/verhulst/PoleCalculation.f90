!this subroutine will calculate the polepositions of the next iteration
!depending on the current V and/or Y or envelope (depends on the choice)
SUBROUTINE PoleCalculation
  USE Declare
  IMPLICIT NONE
  
  REAL(8), PARAMETER :: Yknee1=6.9183d-10 !(Ybm at 30dB)
!  REAL(8), PARAMETER :: Yknee2=1.5488d-8 !(Ybm at 97.4dB) 
  REAL(8), PARAMETER :: Vknee1=4.3652d-6 !(Vbm at 30dB)
!  REAL(8), PARAMETER :: Vknee2=9.7836d-5 !(Vbm at 97.4dB)
 
  REAL(8) Vknee2
  REAL(8) Yknee2
 ! REAL(8), PARAMETER :: Va=-3.112d18
 ! REAL(8), PARAMETER :: Vb=1.828d14
 ! REAL(8), PARAMETER :: Vc=-2.466d9 
 ! REAL(8), PARAMETER :: Vd=4.318d4
 ! REAL(8), PARAMETER :: Ve=0.02669d0
  REAL(8) Yknee1CST(1:n)
  REAL(8) Yknee2CST(1:n)
  REAL(8) Vvect(1:n)
  REAL(8) Yvect(1:n)
  REAL(8) Yknee1F(1:n)
  REAL(8) Yknee2F(1:n)
  REAL(8) Rth(1:n)
  REAL(8) NDY1(1:n)
  REAL(8) NDY2(1:n)
  REAL(8) NDV1(1:n)
  REAL(8) NDV2(1:n)
  REAL(8) RthY1(1:n)
  REAL(8) RthY2(1:n)
  REAL(8) RthV1(1:n)
  REAL(8) RthV2(1:n)
  REAL(8) dY1
  REAL(8) dY2
  REAL(8) dV1
  REAL(8) dV2
  REAL(8),PARAMETER ::  factor=100d0
  REAL(8) PoleE
  REAL(8) Ax
  REAL(8) Bx
  REAL(8) PoleS(1:n)
  REAL(8) Theta0(1:n)
  REAL(8) Theta(1:n)
  REAL(8) Sfoc(1:n)
  REAL(8) Sa(1:n)
  REAL(8) Sb(1:n)
  REAL(8) Se(1:n)
  REAL(8) Sxp(1:n)
  REAL(8) Syp(1:n)
  REAL(8) Sy(1:n)
  REAL(8) Rand(1:n)
  INTEGER ctr
  INTEGER fhtel
  INTEGER onektel
  INTEGER, dimension(2) :: seed

!calculate the PoleE depending on the starting pole
!and the compression slope. The values come from the pole calculation plot, obtained by running linear models at 0 and 100 dB SPL for different starting poles.
IF (compressionslope .EQ. 0.4d0) THEN
  Yknee2=1.5488d-8 !(Ybm at 97.4dB) 
  Vknee2=9.7836d-5 !(Vbm at 97.4dB)
  Ax=(0.7d0-0.06d0)/(97.4d0-30d0)
  Bx=SheraPo-Ax*30d0
  PoleE=Ax*97.4d0+Bx
ENDIF
IF (compressionslope .EQ. 0.5d0) THEN
  Yknee2=1.766d-8 !(Ybm at 97.82dB) 
  Vknee2=1.114d-4 !(Vbm at 97.82dB)
  Ax=(0.4d0-0.06d0)/(97.82d0-30d0)
  Bx=SheraPo-Ax*30d0
  PoleE=Ax*97.82d0+Bx
ENDIF
IF (compressionslope .EQ. 0.3d0) THEN
  Yknee2=7.015d-9 !(Ybm at 87.77dB) 
  Vknee2=4.426d-5 !(Vbm at 87.77dB)
  Ax=(0.7d0-0.06d0)/(87.77d0-30d0)
  Bx=SheraPo-Ax*30d0
  PoleE=Ax*87.77d0+Bx
ENDIF
IF (compressionslope .EQ. 0.2d0) THEN
  Yknee2=3.228d-9 !(Ybm at 80.59dB) 
  Vknee2=2.037d-5 !(Vbm at 80.59dB)
  Ax=(0.7d0-0.06d0)/(80.59d0-30d0)
  Bx=SheraPo-Ax*30d0
  PoleE=Ax*80.59d0+Bx	
ENDIF
IF (compressionslope .NE. 0.2d0 .AND. compressionslope .NE. 0.3d0 .AND. compressionslope .NE. 0.4d0 .AND. compressionslope .NE. 0.5d0) THEN 
!if other value is given present the default 0.4 compression slope
  Yknee2=1.5488d-8 !(Ybm at 97.4dB) 
  Vknee2=9.7836d-5 !(Vbm at 97.4dB)
  Ax=(0.7d0-0.06d0)/(97.4d0-30d0)
  Bx=SheraPo-Ax*30d0
  PoleE=Ax*97.4d0+Bx
ENDIF
 
!Introduce variability in the Vknee thresholds, kneepoint varies slightly from section
!to section
IF (useZweigIrregularity) THEN
   seed(1)=Subjectnr * 29
   seed(2)=Subjectnr + 2010 * 08                
   CALL RANDOM_SEED(PUT=seed) 
   CALL RANDOM_NUMBER(Rth(1:n))
   
   !section to section variability in the kneepoint threshold in dB
   IF (useKneeVar) THEN
      dY1=20d0*LOG10(Yknee1)
      dY2=20d0*LOG10(Yknee2)
      dV1=20d0*LOG10(Vknee1)
      dV2=20d0*LOG10(Vknee2)
      NDY1(1:n)= dY1 + ((Rth(1:n) - 0.5d0)* KneeVar/0.5d0) !normal dist in [dB]
      NDY2(1:n)= dY2 + ((Rth(1:n) - 0.5d0)* KneeVar/0.5d0) !normal dist in [dB]
      NDV1(1:n)= dV1 + ((Rth(1:n) - 0.5d0)* KneeVar/0.5d0) !normal dist in [dB]
      NDV2(1:n)= dV2 + ((Rth(1:n) - 0.5d0)* KneeVar/0.5d0) !normal dist in [dB]
      RthY1(1:n)= 10d0 ** (NDY1/20d0) !log dist around kneepoint in disp [m]
      RthY2(1:n)= 10d0 ** (NDY2/20d0) !log dist around kneepoint in disp [m]
      RthV1(1:n)= 10d0 ** (NDV1/20d0) !log dist around kneepoint in vel [m]
      RthV2(1:n)= 10d0 ** (NDV2/20d0) !log dist around kneepoint in vel [m]
   ELSE
      RthY1(1:n)= Yknee1
      RthY2(1:n)= Yknee2
      RthV1(1:n)= Vknee1
      RthV2(1:n)= Vknee2
   ENDIF
   !the main variability sits in the starting pole variation  
    DO i=0,n
         Rand(i)= IrrPct * (Rth(i) - 0.5d0)
    ENDDO 
    PoleS(1:n)=(1d0+Rand) * SheraPo

ELSE !no irregularities
   PoleS(1:n)= SheraPo
   RthY1(1:n)= Yknee1
   RthY2(1:n)= Yknee2
   RthV1(1:n)= Vknee1
   RthV2(1:n)= Vknee2
ENDIF

!!Don't put irregularities in the low frequencies
!!If subject dependent irregularity patterns are needed, they need to go here
    ctr=0
    onektel=0
    DO i=1,n       
      IF (omega(i)/(2d0 *pi) .GE. 100) THEN
         ctr=ctr+1;
      ENDIF
      IF (omega(i)/(2d0 *pi) .GE. 1000) THEN
         onektel=onektel+1;   
         !find the section freq
      ENDIF
   ENDDO
    
   IF(.NOT.useLFirregularity) THEN
      PoleS(ctr:n)= SheraPo
      RthY1(ctr:n)= Yknee1
      RthY2(ctr:n)= Yknee2
      RthV1(ctr:n)= Vknee1
      RthV2(ctr:n)= Vknee2
   END IF

!! Here the poles are calculated for the nonlinear case
IF (Nonlinear) THEN
  IF (SheraNonlinearityType == DISP) THEN    
     !nonlinearity threshold for the displacement is proportional to 1/omega,
       Yknee1CST= RthY1 * omega(onektel) !normalized to 1 kHz location 
       Yknee2CST= RthY2 * omega(onektel)
       Yknee1F(1:n)= Yknee1CST(1:n) / omega(1:n) !kneepoint function ifo omega
       Yknee2F(1:n)= Yknee2CST(1:n) / omega(1:n) !so that at 1kHz, YkneeF = Yknee
       
       Yvect(1:n)=ABS(Y(1:n))/Yknee1F(1:n)
       Theta0(1:n)=ATAN(((PoleE-PoleS)*factor)/((Yknee2F(1:n)/Yknee1F(1:n))-1d0)) 
       Theta(1:n)=Theta0/2d0
       Sfoc(1:n)=(PoleS*factor)/(Yknee2F(1:n)/Yknee1F(1:n))
       Se(1:n)=1d0/COS((pi-Theta0(1:n))/2d0)
       Sb(1:n)=Sfoc(1:n)/Se(1:n)
       Sa(1:n)=Sfoc(1:n)*DSQRT(1d0-(1d0/(Se(1:n)**2d0)))
       Sxp(1:n)=(Yvect(1:n)-1d0)*COS(Theta(1:n))/COS(2d0*Theta(1:n))
       Syp(1:n)=Sb(1:n)*DSQRT(1d0+(Sxp(1:n)/Sa(1:n))**2d0)
       Sy(1:n)=Sxp(1:n)*SIN(Theta(1:n))+Syp*COS(Theta(1:n))
       SheraP(1:n)=PoleS(1:n)+Sy(1:n)/factor
  ENDIF !end for the displacement nonlinearity
  
  IF (SheraNonlinearityType == VEL) THEN 

     Vvect(1:n)=ABS(V(1:n))/RthV1(1:n)
     Theta0(1:n)=ATAN(((PoleE-PoleS)*factor)/((RthV2(1:n)/RthV1(1:n))-1d0)) 
     Theta(1:n)=Theta0/2d0
     Sfoc(1:n)=(PoleS*factor)/(RthV2(1:n)/RthV1(1:n))
     Se(1:n)=1d0/COS((pi-Theta0(1:n))/2d0)
     Sb(1:n)=Sfoc(1:n)/Se(1:n)
     Sa(1:n)=Sfoc(1:n)*DSQRT(1d0-(1d0/(Se(1:n)**2d0)))
     Sxp(1:n)=(Vvect(1:n)-1d0)*COS(Theta(1:n))/COS(2d0*Theta(1:n))
     Syp(1:n)=Sb(1:n)*DSQRT(1d0+(Sxp(1:n)/Sa(1:n))**2d0)
     Sy(1:n)=Sxp(1:n)*SIN(Theta(1:n))+Syp*COS(Theta(1:n))
     SheraP(1:n)=PoleS(1:n)+Sy(1:n)/factor

  ENDIF
     
  IF (SheraNonlinearityType == ENV) THEN
     SheraP(1:n)=0.061d0 !is not implemented yet, so is set to active,linear behaviour for now
  ENDIF

!to stop the poles from increasing after a certain value lead to linear behavior at high levels
  DO i=1,n
     IF(SheraP(i).GT.PoleE) THEN
        SheraP(i)=PoleE
     ENDIF
  ENDDO   

ELSE !The poles for the linear implementation
   SheraP(1:n)=PoleS(1:n)
ENDIF

debug(1:n)=SheraP(1:n)
END SUBROUTINE PoleCalculation
