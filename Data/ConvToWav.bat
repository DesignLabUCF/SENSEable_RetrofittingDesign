pushd D:\UnrealProjects\AR_Facades\Data

:: MUST SET
set participantID=6

cd Subjects\%participantID%

ffmpeg -i %participantID%_Audio.m4a %participantID%_Audio.wav