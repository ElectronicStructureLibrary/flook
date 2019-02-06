#!/bin/bash

# Create link from .setup.make to
# setup.make

_old_arch=
if [ -L setup.make ]; then
    # We assume the setup.make is 
    # a link to .setup.make
    # Simply delete it, we will re-instantiate it
    rm setup.make
elif [ -e setup.make ]; then
    _old_arch=.temporary_setup.make
    mv setup.make $_old_arch
fi

# Create link to setup.make
ln -s .setup.make setup.make

# call make...
make clean
make liball
FLOOK_LIB="flookall -ldl -L." make check

if [ -z "$_old_arch" ]; then
    # the link should sustain,
    # it still links to .setup.make
    echo "do nothing" > /dev/null
else
    rm setup.make
    mv $_old_arch setup.make
fi
