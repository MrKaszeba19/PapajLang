echo Starting...
path=`xdg-user-dir DESKTOP`
echo "#!/usr/bin/env xdg-open" > $path/RPN.desktop
echo "[Desktop Entry]" >> $path/RPN.desktop
echo "Version=0.5" >> $path/RPN.desktop
echo "Type=Application" >> $path/RPN.desktop
echo "Terminal=true" >> $path/RPN.desktop
echo "Exec=$(pwd)/rpn repl" >> $path/RPN.desktop
echo "Name=RPN REPL 0.5.0" >> $path/RPN.desktop
echo "Comment=REPL of RPN Calculator – PapajScript interpreter." >> $path/RPN.desktop
echo "Icon=$(pwd)/rpn.ico" >> $path/RPN.desktop
if [ -f $path/RPN.desktop ] ; then
	chmod +x ~/Desktop/RPN.desktop
	echo The shortcut of RPN REPL has been created on Desktop.
fi