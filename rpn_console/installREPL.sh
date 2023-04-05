#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installREPL.sh
#   or ./installREPL.sh [flags]
#
# flags:
#    - (none)  : Just install RPN REPL to your Desktop
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

echo "#!/usr/bin/env xdg-open" > "$path/RPN.desktop"
echo "[Desktop Entry]" >> "$path/RPN.desktop"
echo "Version=0.5" >> "$path/RPN.desktop"
echo "Type=Application" >> "$path/RPN.desktop"
echo "Terminal=true" >> "$path/RPN.desktop"
echo "Categories=Development;" >> "$path/RPN.desktop"
echo -e "Exec=\"$(pwd)/rpn\" repl" >> "$path/RPN.desktop"
echo "Name=RPN REPL" >> "$path/RPN.desktop"
echo "Comment=REPL of RPN Calculator – PapajScript interpreter." >> "$path/RPN.desktop"
echo "Icon=$(pwd)/rpn.ico" >> "$path/RPN.desktop"

if [ -f "$path/RPN.desktop" ] ; then
	chmod +x "$path/RPN.desktop"
    if [ $verbose -eq 1 ] ;
    then
	    echo "The shortcut of RPN REPL has been created at $path."
    fi
else
    echo "Error: cannot create a shortcut at $path."
fi

if [ $menustart -eq 1 ] ;
then
    if [ $verbose -eq 1 ] ;
    then
        echo Copying REPL to your Applications Menu. 
        #echo It is a sudo comand. You might be asked for your password.
    fi
    cp "$path/RPN.desktop" "$HOME/.local/share/applications/RPN.desktop"
    if [[ $? -eq 0 && $verbose -eq 1 ]] ;
    then
	    echo "The shortcut of RPN REPL has been created in your Applications Menu."
    fi
fi
