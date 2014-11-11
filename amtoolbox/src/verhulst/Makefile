#Makefile for cochlea model
OBJS =  AllocationError.o CloseWrite.o cochlea.o \
 CochleaParameters.o DecibelToLevel.o Declare.o DeInitialize.o \
 FilesModule.o GraphsModule.o InitializeCochlea.o InitializeGaussElimination.o \
 InitializeMiddleEar.o Initialize.o InitializeStimulus.o InitializeZweig.o \
 IntToString.o MessageModule.o Message.o OpenWrite.o \
 ParametersModule.o Parameters.o ReadError.o ReadMembraneStatus.o InitializeFiles.o \
 ReadParameterFile.o RK4.o SetDampingAndStiffness.o WriteError.o WriteMembraneStatus.o \
 WriteData.o WriteProfile.o ZweigImpedance.o InitializeGraphs.o \
 WaveReadModule.o WaveRead.o Resample.o SampleUpAndDown.o KaiserBessel.o LowPassFilter.o SheraParameters.o PoleCalculation.o PuriaM1.o PuriaM2.o SolveTridiag.o \

#.o:
#	$(CC) $(OFLAGS)  

EFLAGS = -o

# gfortran setting
#OFLAGS = -c -ffixed-line-length-none -Wline-truncation
#CC = gfortran

# Intel Fotran setting
CC = ifort
OFLAGS = -c

verhulst2012: $(OBJS)
	$(CC) $(EFLAGS) verhulst2012 $(OBJS)
	cp verhulst2012 ../../verhulst/

cochlea.o: Declare.f90 filesmodule.mod cochlea.f90 wavereadmodule.mod messagemodule.mod graphsmodule.mod
	$(CC) $(OFLAGS) cochlea.f90

Declare.o: parametersmodule.mod cochleaparameters.mod Declare.f90
	$(CC) $(OFLAGS) Declare.f90

declare.mod: Declare.o Declare.f90
	$(CC) $(OFLAGS) Declare.f90

MessageModule.o: filesmodule.mod MessageModule.f90
	$(CC) $(OFLAGS) MessageModule.f90

messagemodule.mod: MessageModule.o MessageModule.f90
	$(CC) $(OFLAGS) MessageModule.f90  

GraphsModule.o: GraphsModule.f90
	$(CC) $(OFLAGS) GraphsModule.f90

graphsmodule.mod: GraphsModule.o GraphsModule.f90
	$(CC) $(OFLAGS) GraphsModule.f90

FilesModule.o: FilesModule.f90
	$(CC) $(OFLAGS) FilesModule.f90

filesmodule.mod: FilesModule.o FilesModule.f90
	$(CC) $(OFLAGS) FilesModule.f90

ParametersModule.o: ParametersModule.f90
	$(CC) $(OFLAGS) ParametersModule.f90

parametersmodule.mod: ParametersModule.f90 ParametersModule.o
	$(CC) $(OFLAGS) ParametersModule.f90

ReadParameterFile.o: filesmodule.mod ReadParameterFile.f90
	$(CC) $(OFLAGS) ReadParameterFile.f90

cochleaparameters.mod: CochleaParameters.f90 CochleaParameters.o
	$(CC) $(OFLAGS) CochleaParameters.f90

Initialize.o: declare.mod InitializeStimulus.f90 InitializeCochlea.f90 InitializeMiddleEar.f90\
		SetDampingAndStiffness.f90 InitializeZweig.f90 InitializeGaussElimination.f90\
		AllocationError.f90 Initialize.f90
	$(CC) $(OFLAGS) Initialize.f90

InitializeCochlea.o: declare.mod InitializeCochlea.f90
	$(CC) $(OFLAGS) InitializeCochlea.f90

InitializeMiddleEar.o: declare.mod InitializeMiddleEar.f90
	$(CC) $(OFLAGS) InitializeMiddleEar.f90

InitializeGaussElimination.o: declare.mod InitializeGaussElimination.f90
	$(CC) $(OFLAGS) InitializeGaussElimination.f90

InitializeStimulus.o: declare.mod InitializeStimulus.f90
	$(CC) $(OFLAGS) InitializeStimulus.f90

InitializeZweig.o: declare.mod InitializeZweig.f90
	$(CC) $(OFLAGS) InitializeZweig.f90

ReadMembraneStatus.o: declare.mod filesmodule.mod DecibelToLevel.f90 ReadError.f90 ReadMembraneStatus.f90
	$(CC) $(OFLAGS) ReadMembraneStatus.f90

OpenWrite.o: declare.mod filesmodule.mod OpenWrite.f90
	$(CC) $(OFLAGS) OpenWrite.f90

RK4.o: declare.mod RK4.f90
	$(CC) $(OFLAGS) RK4.f90

WriteData.o: declare.mod graphsmodule.mod filesmodule.mod WriteData.f90
	$(CC) $(OFLAGS) WriteData.f90

CloseWrite.o: declare.mod filesmodule.mod CloseWrite.f90
	$(CC) $(OFLAGS) CloseWrite.f90

DeInitialize.o: declare.mod DeInitialize.f90
	$(CC) $(OFLAGS) DeInitialize.f90

WriteMembraneStatus.o: declare.mod filesmodule.mod graphsmodule.mod WriteMembraneStatus.f90
	$(CC) $(OFLAGS) WriteMembraneStatus.f90

WriteProfile.o: graphsmodule.mod declare.mod filesmodule.mod WriteProfile.f90
	$(CC) $(OFLAGS) WriteProfile.f90

AllocationError.o: messagemodule.mod Message.f90 AllocationError.f90
	$(CC) $(OFLAGS) AllocationError.f90




DecibelToLevel.o: DecibelToLevel.f90	
	$(CC) $(OFLAGS) DecibelToLevel.f90

PuriaM1.o: declare.mod  PuriaM1.f90	
	$(CC) $(OFLAGS) PuriaM1.f90

PuriaM2.o: declare.mod  PuriaM2.f90	
	$(CC) $(OFLAGS) PuriaM2.f90

SolveTridiag.o: declare.mod  SolveTridiag.f90	
	$(CC) $(OFLAGS) SolveTridiag.f90

IntToString.o: IntToString.f90
	$(CC) $(OFLAGS) IntToString.f90 	

Message.o: Message.f90
	$(CC) $(OFLAGS) Message.f90	

Parameters.o: declare.mod filesmodule.mod Parameters.f90
	$(CC) $(OFLAGS) Parameters.f90

ReadError.o: ReadError.f90
	$(CC) $(OFLAGS) ReadError.f90

SetDampingAndStiffness.o: declare.mod SetDampingAndStiffness.f90
	$(CC) $(OFLAGS)  SetDampingAndStiffness.f90

ZweigImpedance.o: declare.mod ZweigImpedance.f90
	$(CC) $(OFLAGS)  ZweigImpedance.f90

SheraParameters.o: declare.mod SheraParameters.f90
	$(CC) $(OFLAGS) SheraParameters.f90

PoleCalculation.o: declare.mod PoleCalculation.f90
	$(CC) $(OFLAGS) PoleCalculation.f90

InitializeFiles.o: declare.mod filesmodule.mod messagemodule.mod InitializeFiles.f90
	$(CC) $(OFLAGS) InitializeFiles.f90	

#PlotGraphs.o: declare.mod graphsmodule.mod PlotGraphs.f90
#	$(CC) $(OFLAGS) PlotGraphs.f90

InitializeGraphs.o: declare.mod graphsmodule.mod InitializeGraphs.f90
	$(CC) $(OFLAGS) InitializeGraphs.f90

wavereadmodule.mod: WaveReadModule.o WaveReadModule.f90
	$(CC) $(OFLAGS) WaveReadModule.f90

%.o: %.f90
	$(CC) $(OFLAGS) $<

#clean rule. Remove all object files, backup files AND the executable
clean:
	rm *.o *.mod *~ verhulst2012
