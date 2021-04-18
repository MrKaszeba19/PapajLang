#!/bin/bash

echo "Starting..."

lazbuild rpn.lpi

if [ $? -eq 0 ] ; then
	echo
	echo
	echo "==================================================================="
	echo "Successfully compiled!"
	echo "Run \"./installREPL.sh\" to create a shortcut to RPN REPL on Desktop."
	echo "Run \"./installBash.sh\" to install RPN to your \$PATH"
	echo "==================================================================="
fi
