!This module is shared by "Cochlea" and CochleaDialog"

!If <WriteRead> is READS then reads otherwise writes parameters to <FileHandler>
!<ParameterFileSucceeded> indicates whether  I/O operation succeeded or not
! All the parameters read or written are in the namelists below
! Combining the READ and WRITE routines in one file, as done here, 
! makes it possible to use one NAMELIST.

SUBROUTINE Parameters (WriteRead, FileHandler)
	USE Declare
	USE FilesModule
	IMPLICIT NONE
	INTEGER, INTENT (IN) :: FileHandler
	INTEGER, INTENT (IN) :: WriteRead
	INTEGER ios
	CHARACTER(127) tmpAudioFileName, tmpSettingsDirectory, tmpOutputDirectory, tmpPressureEarCanalFileName, tmpProfileFileName, tmpProbingFileName, tmpCochleaFileName !SV

	NAMELIST /GENERAL/ n, ComputationalFrequency, t_max, retrieveMembraneStatus, storeMembraneStatus, settingsDirectory
	NAMELIST /STIMULUS/ OnsetDuration
	NAMELIST /SIGNAL1/ useSignal1, signal1Frequency, signal1Phase, signal1Level
	NAMELIST /SIGNAL2/ useSignal2, signal2Frequency, signal2Phase, signal2Level
	NAMELIST /SIGNAL3/ useSignal3, signal3Period, signal3Duration, signal3Level
	NAMELIST /AUDIOFILE/ useAudiofile, useLeftChannel, audioFileName, audioFileLevel, audioFileRMS
	NAMELIST /COCHLEA/ Rme, useZweigIrregularity, IrrPct, useLFirregularity, useKneeVar , KneeVar , Nonlinear, SheraNonlinearityType, SheraPo, compressionslope, Subjectnr			 
	NAMELIST /OUTPUT/ OutputDirectory, storePressureEarCanal, PressureEarCanalFileName, storeProfile, ProfileFileName, &
													storeProbing, ProbingFileName, probes !!SV

	IF (WriteRead==READS) THEN
		READ (FileHandler, GENERAL, IOSTAT = ios)
		ParameterFileSucceeded = (ios ==0)
		READ (FileHandler, STIMULUS, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, SIGNAL1, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, SIGNAL2, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, SIGNAL3, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, AUDIOFILE, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, COCHLEA, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		READ (FileHandler, OUTPUT, IOSTAT = ios)
!		IF(ios /= 0)	WRITE (*,*) 'OUTPUT NAMELIST: '
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
	ELSE
		! Add quotation marks around FileName / directory in case they contain spaces
		tmpSettingsDirectory = SettingsDirectory
		tmpAudioFileName = AudioFileName
		tmpOutputDirectory = OutputDirectory
		tmpPressureEarCanalFileName = PressureEarCanalFileName
		tmpProfileFileName = ProfileFileName
		tmpProbingFileName = ProbingFileName
		tmpCochleaFileName = CochleaFileName
		SettingsDirectory = '"' // TRIM(SettingsDirectory) // '"'
		AudioFileName = '"' // TRIM(AudioFileName) // '"'
		OutputDirectory = '"' // TRIM(OutputDirectory) // '"'
		PressureEarCanalFileName = '"' // TRIM(PressureEarCanalFileName) // '"'
		ProfileFileName = '"' // TRIM(ProfileFileName) // '"'
		ProbingFileName = '"' // TRIM(ProbingFileName) // '"'
		CochleaFileName = '"' // TRIM(ProbingFileName) // '"'
		WRITE (FileHandler, GENERAL, IOSTAT = ios)
		ParameterFileSucceeded = (ios ==0)
		WRITE (FileHandler, STIMULUS, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, SIGNAL1, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, SIGNAL2, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, SIGNAL3, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, AUDIOFILE, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, COCHLEA, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		WRITE (FileHandler, OUTPUT, IOSTAT = ios)
		ParameterFileSucceeded  = (ParameterFileSucceeded .AND. ios ==0)
		SettingsDirectory = tmpSettingsDirectory
		AudioFileName = tmpAudioFileName
		OutputDirectory = tmpOutputDirectory
		PressureEarCanalFileName = tmpPressureEarCanalFileName
		ProfileFileName = tmpProfileFileName
		ProbingFileName = tmpProbingFileName
	        CochleaFileName = tmpCochleaFileName
	ENDIF

END SUBROUTINE Parameters
