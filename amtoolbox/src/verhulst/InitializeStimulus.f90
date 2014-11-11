! Calculation of values related to 'loudness' and frequency of an external stimulus.

SUBROUTINE InitializeStimulus
	USE Declare
	IMPLICIT NONE
	REAL(8) DecibelToLevel

	signal1Pressure = DecibelToLevel (signal1Level, p0)
	signal2Pressure = DecibelToLevel (signal2Level, p0)
	signal3Pressure = DecibelToLevel (signal3Level, p0)
	audioFilePressure = DecibelToLevel (audioFileLevel, p0)
	signal1AngularFrequency =2 * pi * signal1Frequency
	signal2AngularFrequency = 2 * pi * signal2Frequency
	IF (useAudioFile) THEN
		maximumSignalPressure = audioFilePressure
	ELSE
		maximumSignalPressure = 0
		IF (useSignal1)	maximumSignalPressure = signal1Pressure
		IF (useSignal2)	maximumSignalPressure = maximumSignalPressure + signal2Pressure
		IF (useSignal3)	maximumSignalPressure = maximumSignalPressure + signal3Pressure
	ENDIF

END SUBROUTINE InitializeStimulus