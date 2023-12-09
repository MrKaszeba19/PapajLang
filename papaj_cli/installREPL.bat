@echo off
set /A directory=value
echo Set oWS = WScript.CreateObject("WScript.Shell") > CreateShortcut.vbs
echo sLinkFile = "%userprofile%\Desktop\Papaj REPL.lnk" >> CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> CreateShortcut.vbs
echo oLink.TargetPath = "%cd%\papaj.exe" >> CreateShortcut.vbs
echo oLink.Arguments = "repl" >> CreateShortcut.vbs 
echo oLink.WorkingDirectory = "%cd%" >> CreateShortcut.vbs
echo oLink.Description = "Run Papaj REPL" >> CreateShortcut.vbs
echo oLink.IconLocation = "%cd%\papaj.ico" >> CreateShortcut.vbs
echo oLink.Save >> CreateShortcut.vbs
cscript CreateShortcut.vbs
del CreateShortcut.vbs
echo The shortcut of RPN REPL has been created on Desktop.
pause
