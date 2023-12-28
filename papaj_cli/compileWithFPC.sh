#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./compileWithFPC.sh
#   or ./compileWithFPC.sh [flags]
#
# flags:
#    - (none)  : Just compile the project
#    - s       : Compile and save the compilation files
#    - S       : Compile, save the compilation files, along with assembler code
#    - a       : Save assembler code
#    - O       : Use optimization level 3
#
# ======================================================
# =====================|SCRIPT|=========================

echo "Starting..."

# ====== ARGS
savetemps=0
assembler=0
optimize=""
if [[ ${BASH_ARGV[0]} =~ s ]] ; 
then
	echo "Note: Save-temps level 1 activated";
	savetemps=1;
	assembler=0;
elif [[ ${BASH_ARGV[0]} =~ S ]] ; 
then
	echo "Note: Save-temps level 2 activated";
	savetemps=2;
	assembler=1;
fi

if [[ ${BASH_ARGV[0]} =~ a ]] ; 
then
	echo "Note: Assembler file is preserved";
	assembler=1;
fi

if [[ ${BASH_ARGV[0]} =~ O ]] ; 
then
	echo "Note: Optimized level 3 compilation activated";
	optimize="-O3";
fi

# ====== COMPILATION
# ------ Preparation

# ------ Running

echo "Compiling..."
mv papaj.lpr papaj.pas

if [[ $assembler -lt 1 ]] ; then
	fpc papaj.pas -o"papaj" $opt
else
	fpc papaj.pas -o"papaj" -a $opt
fi

# ------ If compiled

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"bash ./installREPL.sh\" to create a shortcut to Papaj REPL on Desktop."
	echo "Run \"bash ./installLocal.sh\" to install compiled app to your local \$PATH"
	echo "Run \"sudo bash ./installGlobal.sh\" to install compiled app to the global path"
	echo "==================================================================="
fi

# ====== FINAL FIXING

if [[ $savetemps -lt 1 ]] ; then
	rm *.o
	rm *.or
	rm *.ppu
fi
mv papaj.pas papaj.lpr
