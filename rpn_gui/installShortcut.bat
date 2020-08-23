@echo off
set /A directory=value
echo Set oWS = WScript.CreateObject("WScript.Shell") > CreateShortcut.vbs
echo sLinkFile = "%userprofile%\Desktop\RPN Calculator GUI.lnk" >> CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> CreateShortcut.vbs
echo oLink.TargetPath = "%cd%\rpncalculator.exe" >> CreateShortcut.vbs
echo oLink.WorkingDirectory = "%cd%" >> CreateShortcut.vbs
echo oLink.Description = "Run a GUI app of RPN Calculator – PapajScript interpreter." >> CreateShortcut.vbs
echo oLink.IconLocation = "%cd%\rpncalculator.ico" >> CreateShortcut.vbs
echo oLink.Save >> CreateShortcut.vbs
cscript CreateShortcut.vbs
del CreateShortcut.vbs
echo The shortcut of RPN Calculator GUI app has been created on Desktop.
pause
