@echo off
echo Starting...

set ver=3.0.4

if /i "%processor_architecture%"=="AMD64" GOTO AMD64
if /i "%PROCESSOR_ARCHITEW6432%"=="AMD64" GOTO AMD64
if /i "%processor_architecture%"=="x86" GOTO x86

:x86
	set arch=i386-win32
	
:AMD64
	set arch=x86_64-win64

:compile
	ren rpn.lpr rpn.pas

	echo Attempting to install from Lazarus FPC executable...
	call C:\lazarus\fpc\%ver%\bin\%arch%\fpc.exe rpn.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Attempting to install from a non-Lazarus FPC executable...
	call C:\fpc\%ver%\bin\%arch%\fpc.exe rpn.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Maybe there is FPC in Windows PATH...
	fpc.exe rpn.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Could not find a FPC in the %ver% version.
	goto :end

:next
	echo.
	echo.
	echo ================================================================ 
	echo Done.
	echo ================================================================

:end
	del *.o
	del *.or
	del *.ppu
    del *.obj
	ren rpn.pas rpn.lpr 
	pause