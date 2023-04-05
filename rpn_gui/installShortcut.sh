#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installShortcut.sh
#   or ./installShortcut.sh [flags]
#
# flags:
#    - (none)  : Just install RPN GUI App to your Desktop
#    - v       : Do it verbosely (print every step)
#    - m       : Install REPL to Apllications Menu
#
# ======================================================
# =====================|SCRIPT|=========================

verbose=0
menustart=0
if [[ ${BASH_ARGV[0]} =~ v ]] ; 
then
	verbose=1
fi
if [[ ${BASH_ARGV[0]} =~ m ]] ; 
then
	menustart=1
fi

if [ $verbose -eq 1 ] ;
then
    echo Starting...
fi

if ! command -v xdg-user-dir &> /dev/null then
then
    echo "Warning: xdg-user-dir not installed on this computer."
    echo "Using explicit ~/Desktop as the Desktop path. This path might not exist on non-English OSes."
    path=~/Desktop
else
    path=`xdg-user-dir DESKTOP`
fi
echo "#!/usr/bin/env xdg-open" > "$path/RPNG.desktop"
echo "[Desktop Entry]" >> "$path/RPNG.desktop"
echo "Version=0.5" >> "$path/RPNG.desktop"
echo "Type=Application" >> "$path/RPNG.desktop"
echo -e "Exec=\"$(pwd)/rpncalculator\"" >> "$path/RPNG.desktop"
echo "Name=RPN Calculator GUI" >> "$path/RPNG.desktop"
echo "Categories=Development;" >> "$path/RPNG.desktop"
echo "Comment=A GUI app of RPN Calculator – PapajScript interpreter." >> "$path/RPNG.desktop"
echo "Icon=$(pwd)/rpncalculator.ico" >> "$path/RPNG.desktop"

if [ -f "$path/RPNG.desktop" ] ; then
	chmod +x "$path/RPNG.desktop"
	if [ $verbose -eq 1 ] ;
    then
        echo "The shortcut of RPN Calculator GUI has been created at $path."
    fi
else
    echo "Error: cannot create a shortcut at $path."
fi

if [ $menustart -eq 1 ] ;
then
    if [ $verbose -eq 1 ] ;
    then
        echo Copying GUI App to your Applications Menu. 
        #echo It is a sudo comand. You might be asked for your password.
    fi
    cp "$path/RPNG.desktop" "$HOME/.local/share/applications/RPNG.desktop"
    if [[ $? -eq 0 && $verbose -eq 1 ]] ;
    then
	    echo "The shortcut of RPN GUI App has been created in your Applications Menu."
    fi
fi
