# @lib

Library for defining input/output data through Lua interface.

This allows abstraction of input files to be pure Lua files to enable
calculations in the in-out put of the input.

It also enables running Lua at intermediate levels in any code.
For instance for retrieval of input etc.

## Usage

Imagine you have a program which has 3 distinct places where interaction
might occur:

      program main
      call initialize()
      call calculate()
      call finalize()
      end program

At each intermediate point one wishes to communicate with a scripting language.  
@lib lets you communicate @f and @lua.


## Dowloading and installation

Installing @lib requires you to first fetch the library which is currently hosted at Github.

To fetch all required files do this

    git clone git@github.com:ElectronicStructureLibrary/flook.git
    cd flook
    git submodule init
    git submodule update
    ls -l

At this point you should see the following directories and files:

    drwxr-xr-x 7 USER USER 4.0K Jun  5 17:57 aotus
    -rw-r--r-- 1 USER USER 7.5K Jun  5 17:56 LICENSE
    -rw-r--r-- 1 USER USER  667 Jun  5 17:56 Makefile
    -rw-r--r-- 1 USER USER  298 Jun  5 17:56 README.md
    drwxr-xr-x 2 USER USER 4.0K Jun  5 17:56 src
    drwxr-xr-x 2 USER USER 4.0K Jun  5 17:56 test

To compile @lib you need a minimal `arch.make` file.  
The content of `arch.make` which should be located in the top directory can be this
(please correct format to conform to `Makefile` standards):

    CC = gcc
    FC = gfortran
    CFLAGS = -g
    FFLAGS = -g
    .f90.o:
        $(FC) -c $(FFLAGS) $(INC) $<
    .F90.o:
        $(FC) -c $(FFLAGS) $(INC) $<
    .c.o:
        $(CC) -c $(CFLAGS) $(INC) $<

The `$(INC)` are needed for internal reasons, (sorry about the quick mock-up)...

Type `make` and possibly `make check` to run the tests in the [test](test/) directory.

### Linking

Now comes the worst part. 

@lib consists intrinsically of 4 libraries, 
1. Lua library (`-llua`),
2. fortran to Lua interface (`-lflu`), 
3. basic fortran Lua interaction layer (`-laotus`),  
4. @lib (`-lflook`).

I thank [Harald Klimach](https://bitbucket.org/haraldkl) which hosts the required lower 
level libraries for direct Lua interaction.

Currently I do not have time to reduce these to fewer libraries but PR are always appreciated!

In order to link properly to @lib you can use this template (`Makefile`) for for include statements and library linking 
(note that you should _not_ switch the order of these statements):

    FLOOK_PATH  = /path/to/flook/parent
    FLOOK_LIBS += -L$(FLOOK_PATH)/src -lflook
    FLOOK_LIBS += -L$(FLOOK_PATH)/aotus/source -laotus
    FLOOK_LIBS += -L$(FLOOK_PATH)/aotus/LuaFortran -lflu
    FLOOK_LIBS += -L/opt/generic/lua/5.3.0/lib -llua -ldl
    FLOOK_INC   = -I$(FLOOK_PATH)/src

For the sources that you compile you need to add `$(FLOOK_INC)` to the command line, whilst 
for linking the program you need `$(FLOOK_LIBS)` on the command line.

## Example

Several examples exists in the [test](test/) directory where one of the cases
are shown in the following example:
@include exp_flook.f90
The above program is a @f program which communicates with an embedded @lua
@env. It communicates with @lua 6 times and allows retrieval of elements
and changing the elements.
The communicating @lua code looks like this:
@include exp_flook.lua


### Thanks

First, I owe [Harald Klimach](https://bitbucket.org/haraldkl) a big thanks 
for the creation of [aotus](https://bitbucket.org/haraldkl/aotus) for the Lua-Fortran embedment.

Second, I thank James Spencer for help regarding
the [aotus](https://bitbucket.org/haraldkl/aotus) API.

Third, I thank [ESL-CECAM](http://esl.cecam.org/) for hosting a workshop for me to participate 
and create the initial library.
