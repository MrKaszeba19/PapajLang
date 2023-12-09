#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installREPL.sh
#   or ./installREPL.sh [flags]
#
# flags:
#    - (none)  : Just install Papaj REPL to your Desktop
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

#if ! command -v xdg-user-dir &> /dev/null then
if ! type xdg-user-dir &> /dev/null then
then
    echo "Warning: xdg-user-dir not installed on this computer."
    echo "Using explicit ~/Desktop as the Desktop path. This path might not exist on non-English OSes."
    path=~/Desktop
else
    path=`xdg-user-dir DESKTOP`
fi

echo "#!/usr/bin/env xdg-open" > "$path/Papaj.desktop"
echo "[Desktop Entry]" >> "$path/Papaj.desktop"
echo "Version=0.5" >> "$path/Papaj.desktop"
echo "Type=Application" >> "$path/Papaj.desktop"
echo "Terminal=true" >> "$path/Papaj.desktop"
echo "Categories=Development;" >> "$path/Papaj.desktop"
echo -e "Exec=\"$(pwd)/papaj\" repl" >> "$path/Papaj.desktop"
echo "Name=Papaj REPL" >> "$path/Papaj.desktop"
echo "Comment=REPL of PapajScript." >> "$path/Papaj.desktop"
echo "Icon=$(pwd)/papaj.ico" >> "$path/Papaj.desktop"

if [ -f "$path/Papaj.desktop" ] ; then
	chmod +x "$path/Papaj.desktop"
    if [ $verbose -eq 1 ] ;
    then
	    echo "The shortcut of Papaj REPL has been created at $path."
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
    cp "$path/Papaj.desktop" "$HOME/.local/share/applications/Papaj.desktop"
    if [[ $? -eq 0 && $verbose -eq 1 ]] ;
    then
	    echo "The shortcut of RPN REPL has been created in your Applications Menu."
    fi
fi
