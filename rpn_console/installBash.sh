#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installBash.sh
#   or ./installBash.sh [flags]
#
# flags:
#    - (none)  : Just install RPN to your bash $PATH
#    - v       : Do it verbosely (print every step)
#
# ======================================================
# =====================|SCRIPT|=========================

appname="RPN Calculator"
appexec="rpn"
verbose=0
if [[ ${BASH_ARGV[0]} =~ v ]] ; 
then
	verbose=1
fi

if [ $verbose -eq 1 ] ;
then
	echo "Starting..."
	echo "Creating directory $HOME/bin... (unless exists)"
fi

mkdir -p $HOME/bin
if [ $? -ne 0 ] ; then
	echo "Error when creating or finding directory $HOME/bin."
else
	if [ $verbose -eq 1 ] ; then 
		echo "Done!"; 
		echo "Copying $appname to $HOME/bin..."
	fi
	cp ./$appexec $HOME/bin/$appexec
	if [ $? -ne 0 ] ; then
		echo "Error when copying $appname to $HOME/bin."
	else
		if [ $verbose -eq 1 ] ; then 
			echo "Done!"; 
			echo "Setting up \$PATH..."
		fi
		if grep -Fxq "export PATH=$HOME/bin:\$PATH" $HOME/.bashrc ;
		then
			if [ $verbose -eq 1 ] ; then 
				echo "It looks like you've already installed \$PATH on $HOME/bin."
				echo "I've just updated your $appname executable."
			fi
			echo "Installation of $appname to your bash \$PATH has been finished with success!"
			echo "Type '$appexec' in your command prompt to check out the program then."
		else
			echo "# Set up a $appname path" >> $HOME/.bashrc
			echo "export PATH=$HOME/bin:\$PATH" >> $HOME/.bashrc
			source $HOME/.bashrc 
			if [ $? -eq 0 ] ; then
				echo "Installation of $appname to your bash \$PATH has been finished with success!"
				echo "Type '$appexec' in your command prompt to check out the program."
			else
				echo "Error when setting up \$PATH."
			fi
		fi	
	
	fi
fi

