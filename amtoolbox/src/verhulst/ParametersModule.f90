!This module is shared by "Cochlea" and "CochleaDialog"

! module contains variable names whose values are read from and written to parameters.dat

MODULE ParametersModule

! General parameters
INTEGER n		!Number of BM-sections
REAL(8) ComputationalFrequency
REAL(8) t_max
LOGICAL retrieveMembraneStatus							!read mebrane status or not
LOGICAL storeMembraneStatus						!write membrane status or not
CHARACTER(127) settingsDirectory

! Stimulus parameters
!REAL(8), PARAMETER :: PI=3.1415926535897932384626433832795028841971693993751
REAL(8) signal1Period
REAL(8) signal1Frequency
REAL(8) signal1Level
REAL(8) signal1Phase
LOGICAL useSignal1

REAL(8) signal2Period
REAL(8) signal2Frequency
REAL(8) signal2Level
REAl(8) signal2Phase
LOGICAL useSignal2

REAL(8) signal3Period
REAL(8) signal3Duration
REAL(8) signal3Level
LOGICAL useSignal3

REAL(8) signal1AngularFrequency
REAL(8) signal2AngularFrequency
REAL(8) signal1Pressure
REAL(8) signal2Pressure
REAL(8) signal3Pressure
REAL(8) p0x	!druk op x=0

REAL(8) OnsetDuration

LOGICAL useAudioFile
LOGICAL useLeftChannel		!use left or right channel from audio file
REAL(8) audioFileLevel
REAL(8) audioFileRMS
REAL(8) audioFilePressure
CHARACTER(127) audioFileName
REAL(8) maximumSignalPressure

! Cochlea parameters
REAL(8) Rme
LOGICAL Nonlinear
LOGICAL useActiveDamping			! defines whether damping is active or not 
LOGICAL useZweigIrregularity
REAL(8) IrrPct
LOGICAL useLFirregularity
LOGICAL useKneeVar
REAL(8) KneeVar
INTEGER Subjectnr
INTEGER SheraNonlinearityType
INTEGER, PARAMETER :: DISP = 1
INTEGER, PARAMETER :: VEL = 2
INTEGER, PARAMETER :: ENV = 3
REAL(8) SheraPo
REAL(8) compressionslope


! Output parameters
CHARACTER(127) OutputDirectory
LOGICAL storePressureEarCanal
LOGICAL storeProfile
LOGICAL storeProbing
LOGICAL storeCochlea
INTEGER probes(20)
CHARACTER(127) PressureEarCanalFileName
CHARACTER(127) ProfileFileName
CHARACTER(127) ProbingFileName
CHARACTER(127) CochleaFileName

END MODULE ParametersModule
