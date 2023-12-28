#!/bin/bash

# ======================================================
# ======================|INFO|==========================
# Use: ./installGlobal.sh
#   or ./installGlobal.sh [flags]
#
# flags:
#    - (none)  : Just install to your global system path (/bin)
#    - v       : Do it verbosely (print every step)
#
# ======================================================
# =====================|SCRIPT|=========================

appname="Papaj CLI"
appexec="papaj"
verbose=0
if [[ ${BASH_ARGV[0]} =~ v ]] ; 
then
	verbose=1
fi

if [ $verbose -eq 1 ] ; then 
	echo "Starting..."
	echo "Checking if you are running this script as root."
fi

if [[ $(/usr/bin/id -u) -ne 0 ]]; then
    echo "Error: Not running as root."
    exit
fi

if [ $verbose -eq 1 ] ; then 
	echo "You do. I can proceed."
	echo "Copying $appname to /bin."
fi

cp ./$appexec /bin/$appexec

if [ $? -ne 0 ] ; then
	echo "Error when copying $appname to /bin."
else
	if [ $verbose -eq 1 ] ; then 
		echo "$appname copied to /bin successfully."
	fi
fi
