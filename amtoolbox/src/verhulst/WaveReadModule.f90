MODULE WaveReadModule
	!USE WINMM
	IMPLICIT NONE
	SAVE
	
	REAL, ALLOCATABLE :: WaveData( : )
	REAL, ALLOCATABLE :: ResampledWaveData( : )
	INTEGER ResampledWaveDataPoints

	!CHARACTER(80) WaveFileErrorMessage
	!CHARACTER(80), PARAMETER :: FAILEDTOOPEN = "Failed to open file"C
	!CHARACTER(80), PARAMETER :: CORRUPTWAVEFILE = "Corrupt wave file"C
	!CHARACTER(80), PARAMETER :: NOTAWAVEFILE = "File is not a wave file"C
	!CHARACTER(80), PARAMETER :: COMPRESSEDWAVEFILE = "Compressed wave files are not supported"C
	!CHARACTER(80), PARAMETER :: TOOMANYSAMPLESPERSEC = "Wave file contains too many samples per second"C
	!CHARACTER(80), PARAMETER :: TOOMANYAVGBYTESPERSEC = "Wave file contains too many bytes per second"C
	!CHARACTER(80), PARAMETER :: NODATA = "Wave file contains no data"C
	!CHARACTER(80), PARAMETER :: WRONGNUMBEROFBITSPERSAMPLE = "Number of bits per sample in wave file is not supported"C
	!CHARACTER(80), PARAMETER :: TOOMANYCHANNELS = "Wave file contains too many channels"C
	!CHARACTER(80), PARAMETER :: FILENAMETOOLONG = "File name too long"C
	!CHARACTER(80) InfoMessage
	!CHARACTER(80), PARAMETER :: CONTAINS3CHANNELS = "Wave file contains three channels\nOnly left and right channel will be used"C

	!TYPE (T_MMCKINFO) WaveChunk
	!TYPE (T_MMCKINFO) SubChunk

	!INTEGER FormatTag
	!INTEGER Channels
	INTEGER SamplesPerSec
	!INTEGER AvgBytesPerSec
	!INTEGER BlockAlign
	!INTEGER BytesPerSample
	!INTEGER DataSize
	INTEGER SamplesPerChannel
	!REAL(8) AudioFileTotalTime !(in seconds)

	!REAL, PARAMETER :: MaxSigned_n_Bytes (4) = (/128.0, 32768.0, 8388608.0, 2.147483648e9/)
	!MaxSigned_n_Bytes gives the maximum value that can be stored in a n-byte signed, that is: 2^(8*n) / 2

END MODULE	WaveReadModule
