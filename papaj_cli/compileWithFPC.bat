@echo off
echo Starting...

:: set ver=3.0.4
:: set ver=3.2.0
set yourversion=3.3.1

if /i "%processor_architecture%"=="AMD64" GOTO AMD64
if /i "%PROCESSOR_ARCHITEW6432%"=="AMD64" GOTO AMD64
if /i "%processor_architecture%"=="x86" GOTO x86

:x86
	set arch=i386-win32
	goto :compile1
	
:AMD64
	set arch=x86_64-win64
	goto :compile1

:compile1
    set ver=%yourversion%
	ren papaj.lpr papaj.pas

:compile2
    echo Looking for fpc %ver%...
	echo Attempting to install from Lazarus FPC executable...
	echo C:\lazarus\fpc\%ver%\bin\%arch%\fpc.exe
	call C:\lazarus\fpc\%ver%\bin\%arch%\fpc.exe papaj.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Attempting to install from a non-Lazarus FPC executable...
	call C:\fpc\%ver%\bin\%arch%\fpc.exe papaj.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Maybe there is FPC in Windows PATH...
	fpc.exe rpn.pas
	if %ERRORLEVEL% == 0 goto :next

	echo Failed. Could not find a FPC in the %ver% version.
	goto :changever

:changever
    if (%ver% == %yourversion%) (
        set ver=3.3.1
        goto :compile2
    )
    if %ver% == 3.3.1 (
        set ver=3.2.4
        goto :compile2
    )
    if %ver% == 3.2.4 (
        set ver=3.2.2
        goto :compile2
    )
    if %ver% == 3.2.2 (
        set ver=3.2.0
        goto :compile2
    )
    if %ver% == 3.2.0 (
        set ver=3.0.4
        goto :compile2
    )
    if %ver% == 3.0.4 (
        set ver=3.0.2
        goto :compile2
    )
    if %ver% == 3.0.2 (
        set ver=3.0.0
        goto :compile2
    )
    if %ver% == 3.0.0 (
        set ver=2.6.4
        goto :compile2
    )
    if %ver% == 2.6.4 (
        set ver=2.4.2
        goto :compile2
    )
    if %ver% == 3.3.1 (
        set ver=3.2.0
        goto :compile2
    )
    goto :fail

:fail 
    echo Could not find any FPC instance. Program has not been compiled.
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
	ren papaj.pas papaj.lpr 
	pause