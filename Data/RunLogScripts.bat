pushd D:\UnrealProjects\AR_Facades\Data

:: MUST SET
set participantID=75
set fileName=Subjects\%participantID%\%participantID%
python FREDLogParser.py %fileName%
python FredPlotter.py %fileName%
