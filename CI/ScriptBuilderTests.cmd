echo @ECHO OFF > GeneratedTestsBuildScript.cmd
echo call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat" >> GeneratedTestsBuildScript.cmd
echo SET DUNITX=$(BDS)\source\dunitx >> GeneratedTestsBuildScript.cmd
echo SET config="Release" >> GeneratedTestsBuildScript.cmd
echo SET platform="Win32" >> GeneratedTestsBuildScript.cmd
for /f tokens^=* %%i in ('where /r .\..\tests *.dproj')do (echo/msbuild /m %%~dpi%%~nxi /t:build /p:Config=%%config%% /p:platform=%%platform%% /v:diag /fl &echo if NOT %%ERRORLEVEL%%==0 EXIT %%ERRORLEVEL%%) >> GeneratedTestsBuildScript.cmd