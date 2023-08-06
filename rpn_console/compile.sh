#!/bin/sh

echo "Starting..."

lazbuild rpn.lpi

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"bash ./installREPL.sh\" to create a shortcut to RPN REPL on Desktop."
	echo "Run \"bash ./installLocal.sh\" to install compiled app to your local \$PATH"
	echo "Run \"sudo bash ./installGlobal.sh\" to install compiled app to the global path"
	echo "==================================================================="
fi
