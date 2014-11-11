! Open and read <audiofile> and store data in WaveData
! If <useLeftChannel> data from the left channel is stored, else data from the right channel
! Set Audio Format data
! Set WaveFileErrorMessage and InfoMessage

SUBROUTINE WaveRead (audiofile, useLeftChannel, keepData)
	USE WaveReadModule

	IMPLICIT NONE

	CHARACTER(127), INTENT(IN) :: audiofile
	LOGICAL, INTENT(IN) :: useLeftChannel
	LOGICAL, INTENT(IN) :: keepData	!if true written data will be kept in wavedata, else memory will be freed

        WRITE (*,*) 'sox ' // TRIM(audiofile) // ' /tmp/cochlear.dat'

        CALL SYSTEM('sox ' // TRIM(audiofile) // ' /tmp/cochlear.dat')
        CALL SYSTEM('python preformat.py')
        
        OPEN(1,file='/tmp/cochlear_processed.dat')
        
        READ(1,*) SamplesPerSec, SamplesPerChannel
        
        !WRITE (*,*) "The stuff is working!"
        !WRITE (*,*) SamplesPerSec, SamplesPerChannel
        
        ALLOCATE(WaveData(SamplesPerChannel))
        
        ! Read line 2, may contain the number of channels if more than 2
        READ(1,*) WaveData
        
        CLOSE(1)

        CALL SYSTEM("rm /tmp/cochlear.dat")
        CALL SYSTEM("rm /tmp/cochlear_processed.dat")
        
        !CALL OpenWaveFile (audiofile, handler)
	!CALL FindWaveChunk (handler)
	!CALL ReadFmtChunk (handler)
	!CALL CheckFMTvalues
	!CALL ReadDataChunk (handler)
	!CALL ReadData (handler, useLeftChannel)
	
        !IF (ALLOCATED(WaveData) .AND. .NOT. keepData) DEALLOCATE (WaveData)
	!CALL CloseWaveFile (handler)

END SUBROUTINE WaveRead
