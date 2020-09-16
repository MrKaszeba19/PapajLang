#!/bin/bash

# ====== ARGS
units=(
    ConsoleUtils.pas
    UnitEntity.pas
    UnitStack.pas
    UnitVariables.pas
    UnitFunctions.pas
    UnitEnvironment.pas
    UnitREPL.pas
    unit2.pas
    unit5.pas
)

# ====== RUNNING
# ------ Compiling

echo "Starting..."
mv rpn.lpr rpn.pas

fpc ${units[*]} rpn.pas -o"rpn" 

# ------ If compiled

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"./installREPL.sh\" to create a shortcut to RPN REPL on Desktop."
	echo "==================================================================="
fi

# ====== FINAL FIXING

mv rpn.pas rpn.lpr
