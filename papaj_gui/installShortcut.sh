#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installShortcut.sh
#   or ./installShortcut.sh [flags]
#
# flags:
#    - (none)  : Just install Papaj GUI App to your Desktop
#    - v       : Do it verbosely (print every step)
#    - m       : Install GUI App to Apllications Menu
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
echo "#!/usr/bin/env xdg-open" > "$path/PapajGui.desktop"
echo "[Desktop Entry]" >> "$path/PapajGui.desktop"
echo "Version=0.5" >> "$path/PapajGui.desktop"
echo "Type=Application" >> "$path/PapajGui.desktop"
echo -e "Exec=\"$(pwd)/papaj_gui\"" >> "$path/PapajGui.desktop"
echo "Name=Papaj GUI" >> "$path/PapajGui.desktop"
echo "Categories=Development;" >> "$path/PapajGui.desktop"
echo "Comment=PapajScript interpreter GUI app." >> "$path/PapajGui.desktop"
echo "Icon=$(pwd)/papaj_gui.ico" >> "$path/PapajGui.desktop"

if [ -f "$path/PapajGui.desktop" ] ; then
	chmod +x "$path/PapajGui.desktop"
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
    fi
    cp "$path/PapajGui.desktop" "$HOME/.local/share/applications/PapajGui.desktop"
    if [[ $? -eq 0 && $verbose -eq 1 ]] ;
    then
	    echo "The shortcut of RPN GUI App has been created in your Applications Menu."
    fi
fi
