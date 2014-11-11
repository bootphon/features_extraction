! Stores the Y-values of the last <delay> samples for each section in a
! circular buffer (so the last Y-value overwrites the Y-value of <delay> samples ago).
! The circular buffers for each section are part of one large array, where each circular
! buffer is stored consecutively. (so it's an array of circular buffers, where each circular
! buffer stores the displacements of the membrane of the last samples of its section)
! Determines <Yzweig> for every section by interpolating between ZweigSample1 and
! ZweigSample2. ZweigSample1 is the first sample before and
! ZweigSample2 is the first sample after the desired value of Y.
! The desired value, Yzweig,  is the value that Y had at exactly <ZweigFactor> times
! the resonantial period of that section ago.

SUBROUTINE ZweigImpedance
	USE Declare
	IMPLICIT NONE

	INTEGER YbufferStart	! YbufferStart is first index in Ybuffer of current section
	INTEGER YbufferEnd	! YbufferEnd is last index in Ybuffer of current section
        INTEGER(8) Zrp(1:n)
        INTEGER(8) Mudelay(1:n)  !in samples (floored)
        REAL(8) MudelayExact(1:n)!in samples with rounded
        REAL(8) Dev(1:n) !deviation between the two
	! Ybuffer = |_____________|____________________|________________________|____....
	!                          ^          ^^      ^				  
	!                        Start    Sample1,2  End
	!                          <------delay(i)---->
           !delay is max size of each buffer (fixed)
           !Mu delay is the delay corresponding to Mu (variable)
           MudelayExact(1:n)=(SheraMu(1:n)*2d0*pi)/(omega(1:n)*dt)
           Mudelay(1:n)=INT(MudelayExact(1:n)) + 1
           Dev(1:n)= DFLOAT(Mudelay(1:n)) - MudelayExact(1:n)
           DO i =1, n
              Zrp(i)=Zwp(i)-Mudelay(i) !readp is writep - delay in samp
              IF (Zrp(i).LT.ZweigSample1(i)) THEN
                 Zrp(i)=Zrp(i)+delay(i) !wrap the pointer if it reaches end
              ENDIF
              !read out the value and interpolate in each buffer
              IF (Zrp(i).EQ.ZweigSample1(i)+delay(i)-1) THEN
                 YZweig(i)=(1d0-Dev(i))*Ybuffer(Zrp(i)) + Dev(i)*Ybuffer(Zrp(i)-delay(i)+1)
              ELSE
                 YZweig(i)=(1d0-Dev(i))*Ybuffer(Zrp(i)) + Dev(i)*Ybuffer(Zrp(i)+1)
              ENDIF
              !write in the new value in each buffer
              Ybuffer(Zwp(i)) = Y(i)
              !shift the write pointer by one for next iteration
              Zwp(i)=Zwp(i)+1
              IF (Zwp(i).GT.ZweigSample1(i)+delay(i)-1) THEN
                 Zwp(i)=Zwp(i)-delay(i)
              ENDIF
              !debug(1:n)=MudelayExact
           ENDDO

END SUBROUTINE ZweigImpedance
