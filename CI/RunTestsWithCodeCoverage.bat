@ECHO OFF
DEL allunitstotest.txt
DEL allsourcedirs.txt
RMDIR /Q /S TestResults
RMDIR /Q /S ../built/CodeCoverageResults

call CodeCoverage-CreateUnitsToTest.bat ..\source >> allunitstotest.txt
call CodeCoverage-MakeSourceDirs.bat ..\source >> allsourcedirs.txt

SET Params=-e ../built/Win32/Debug/Tests/FidoLibTests.exe -m ../built/Win32/Debug/Tests/FidoLibTests.map -ife -spf allsourcedirs.txt -uf allunitstotest.txt -od ..\built\CodeCoverageResults\ -html -xml -xmllines -lt
CodeCoverage.exe  %Params%

DEL allunitstotest.txt
DEL allsourcedirs.txt
