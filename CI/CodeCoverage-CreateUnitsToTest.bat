@echo off

for /F "delims= eol=" %%A IN ('dir /B /A-D /S %1\*.pas') do echo %%~nA