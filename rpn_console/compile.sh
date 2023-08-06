#!/bin/sh

echo "Starting..."

lazbuild rpn.lpi

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"./installREPL.sh\" to create a shortcut to RPN REPL on Desktop (if using Linux)."
	echo "Run \"./installLocal.sh\" to install compiled RPN console app to your local BASH \$PATH"
	echo "Run \"sudo bash ./installGlobal.sh\" to install compiled RPN console app to the global path"
	echo "==================================================================="
fi
