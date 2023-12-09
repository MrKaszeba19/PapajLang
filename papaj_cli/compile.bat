@echo off
echo Starting...
"C:\lazarus\lazbuild.exe" rpn.lpi
if %ERRORLEVEL% == 0 goto :next
goto :end

:next
    echo.
    echo.
    echo ================================================================ 
    echo Done.
    echo Run installREPL.bat to create a shortcut to RPN REPL on Desktop. 
    echo ================================================================

:end
    pause