pushd D:\UnrealProjects\AR_Facades\Data
:: env\Scripts\activate.bat

:: MUST SET
set participantID=5
set fitFileLocation=D:\UnrealProjects\AR_Facades\Data\Subjects\%participantID%\
:: MUST SET
set fitFile=%fitFileLocation%109398184347_WELLNESS
set javaToolLocation=D:\UnrealProjects\AR_Facades\Data\Watch\FitSDKRelease_21.60.00\java\FitCSVTool.jar
java -jar %javaToolLocation% %fitFile%.fit
python FitReader.py %fitFile%.csv %fitFileLocation%%participantID%_WatchEvents.csv

:: deactivate