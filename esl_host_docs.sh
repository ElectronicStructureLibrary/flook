#!/bin/bash

GIT=git
DOXYGEN=doxygen

# fetch the master
$GIT clone git@github.com:ElectronicStructureLibrary/flook.git $TMPDIR
# Go into temporary directory
cd $TMPDIR
# touch the arch.make file (not needed for documentation creation anyway)
touch arch.make
# Create documentation
$DOXYGEN doc/Doxyfile
