pushd D:\UnrealProjects\AR_Facades\Data

:: MUST SET
set participantID=Test
set fitFileLocation=D:\UnrealProjects\AR_Facades\Data\Subjects\%participantID%\
:: MUST SET
set fitFile=%fitFileLocation%132749086795_WELLNESS

:: HOLOLENS LOG SCRIPT ::
set fileName=Subjects\%participantID%\%participantID%
python FREDLogParser.py %fileName%

:: WATCH SCRIPT ::
set javaToolLocation=D:\UnrealProjects\AR_Facades\Data\Watch\FitSDKRelease_21.60.00\java\FitCSVTool.jar
java -jar %javaToolLocation% %fitFile%.fit
python FitReader.py %fitFile%.csv %fitFileLocation%%participantID%_WatchEvents.csv
