
REM to change dir to the location of this file
REM start time used to average humidity and temperature values: start_time_avg
REM end time used to average humidity and temperature values: stop_time_avg
REM weights (in 15min intervals ie 0.25, 0.5, 0.75 or 1, round down for start and up for end): start w: start_weight, mid w: mid_weight (might not have one), end w: end_weight

cd /D "%~dp0"

mkdir DJI_SDK_raw

for %%i in (*.JPG) do (
echo Working on %%i...
C:\dji_thermal_sdk_v1.3_20220517\utility\bin\windows\release_x64\dji_irp.exe -s %%i -a measure -o DJI_SDK_raw/%%~ni.raw --humidity hum_change --distance dist_change --emissivity emis_change --reflection reflec_change
)
pause