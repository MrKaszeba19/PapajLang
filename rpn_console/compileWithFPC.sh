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
#
# ======================================================
# =====================|SCRIPT|=========================

echo "Starting..."

# ====== ARGS
savetemps=0
assembler=0
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

# ====== COMPILATION
# ------ Preparation
units=(
    MathUtils.pas
    ConsoleUtils.pas
    DTUtils.pas
    UnitEntity.pas
    UnitStack.pas
    UnitVariables.pas
    UnitFunctions.pas
    UnitEnvironment.pas
    UnitREPL.pas
    unit2.pas
    unit5.pas
)

# ------ Running

echo "Compiling..."
mv rpn.lpr rpn.pas

if [[ $assembler -lt 1 ]] ; then
	fpc ${units[*]} rpn.pas -o"rpn" 
else
	fpc ${units[*]} rpn.pas -o"rpn" -a
fi

# ------ If compiled

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"./installREPL.sh\" to create a shortcut to RPN REPL on Desktop."
	echo "Run \"./installBash.sh\" to install RPN to your \$PATH"
	echo "==================================================================="
fi

# ====== FINAL FIXING

if [[ $savetemps -lt 1 ]] ; then
	rm *.o
	rm *.or
	rm *.ppu
fi
mv rpn.pas rpn.lpr
