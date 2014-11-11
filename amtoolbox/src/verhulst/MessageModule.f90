MODULE MessageModule
	USE FilesModule
	IMPLICIT NONE

	INTEGER, PARAMETER :: INCORRECTPARAMETERFILE = 1
	INTEGER, PARAMETER :: WINDOWINITIALIZATIONERROR = 2
	INTEGER, PARAMETER :: GAMMAFILEOPENERROR = 3
	INTEGER, PARAMETER :: ALLOCATIONFAILURE = 4
	INTEGER, PARAMETER :: CREATEDIRECTORYERROR = 5

	CHARACTER (*), PARAMETER :: MessageText(5) = &
		["Parameter file could not be read correctly. Make sure it is a correct parameter file and its not read only or in use by another program. Program will run using the values that are read correctly."C, &
		"-window cant be initialized correctly. Try and decrease the total time or skip more points to plot"C, &
		"Gamma file "//FN_gamma//" for active damping could not be opened. Make sure the file exists and is in the current directory. Program will run without active damping (gamma is set to zero).", &
		"Memory allocation has failed. Enlarge your (virtual) memory on your computer, or use less points in the calculation by decreasing e.g. the number of segments and/or the total time."C, &
		"Creation of output directory failed. Make sure the output directory is a valid directory name and the location is writable. Output files will not be written."C]

	CHARACTER (*), PARAMETER :: MessageTitle(5) = &
		["Error reading "//FN_parameters // ""C, &
		"Initialization error"C, &
		"Error reading "//FN_gamma // ""C, &
		"Memory allocation error"C, &
		"Directory creation error"C]

END MODULE
