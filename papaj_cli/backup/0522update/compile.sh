#!/bin/bash

echo "Starting..."

lazbuild rpn.lpi

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"./installREPL.sh\" to create a shortcut to RPN REPL on Desktop."
	echo "Run \"./installLocal.sh\" to install compiled RPN console app to your local \$PATH"
	echo "Run \"./installGlobal.sh\" to install compiled RPN console app to the global path"
	echo "==================================================================="
fi
