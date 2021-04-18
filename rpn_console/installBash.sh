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
		echo "Copying RPN Calculator to $HOME/bin..."
	fi
	cp ./rpn $HOME/bin/rpn
	if [ $? -ne 0 ] ; then
		echo "Error when copying RPN Calculator to $HOME/bin."
	else
		if [ $verbose -eq 1 ] ; then 
			echo "Done!"; 
			echo "Setting up \$PATH..."
		fi
		if grep -Fxq "export PATH=$HOME/bin/rpn:\$PATH" $HOME/.bashrc ;
		then
			if [ $verbose -eq 1 ] ; then 
				echo "It looks like you've already installed RPN Calculator on $HOME/bin."
				echo "I'll just update your rpn executable."
			fi
			echo "Installation RPN to your bash \$PATH has been finished with success!"
			echo "Type 'rpn' in your command prompt to check out the program."
		else
			echo "# Set up a RPN Calculator path" >> $HOME/.bashrc
			echo "export PATH=$HOME/bin/rpn:\$PATH" >> $HOME/.bashrc
			source $HOME/.bashrc 
			if [ $? -eq 0 ] ; then
				echo "Installation RPN to your bash \$PATH has been finished with success!"
				echo "Type 'rpn' in your command prompt to check out the program."
			else
				echo "Error when setting up \$PATH."
			fi
		fi	
	
	fi
fi

