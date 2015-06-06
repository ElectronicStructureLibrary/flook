# flook #

[![Build Status](https://travis-ci.org/ElectronicStructureLibrary/flook.svg?branch=master)](https://travis-ci.org/ElectronicStructureLibrary/flook)

The fortran-Lua-hook library.

It allows abstraction of input files to be pure Lua files to enable
configuration of internal variables through an embedded Lua interpreter.

Any valid Lua script can be executed from within any fortran application
at points where the application creates Lua _channels_.

Its main usage is the ability to change run-time variables at run-time
in order to optimize, or even change, the execution path of the parent
program.

## Usage ##

Imagine you have a program which has 3 distinct places where interaction
might occur:

	program main
	call initialize()
	call calculate()
	call finalize()
	end program

At each intermediate point one wishes to communicate with a scripting language.  
flook lets you communicate fortran and Lua. For an elaborate example see [Examples](#examples).

## Downloading and installation ##

Installing flook requires you to first fetch the library which is currently
hosted at [github](https://github.com/) at [flook@git].

To fetch all required files do this

	git clone https://github.com/ElectronicStructureLibrary/flook.git
    cd flook
    git submodule init
    git submodule update
    ls -l

At this point you should see (_at least_) the following directories and files:

    drwxr-xr-x 7 USER USER 4.0K Jun  5 17:57 aotus
    -rw-r--r-- 1 USER USER 7.5K Jun  5 17:56 LICENSE
    -rw-r--r-- 1 USER USER  667 Jun  5 17:56 Makefile
    -rw-r--r-- 1 USER USER  298 Jun  5 17:56 README.md
    drwxr-xr-x 2 USER USER 4.0K Jun  5 17:56 src
    drwxr-xr-x 2 USER USER 4.0K Jun  5 17:56 test

To compile flook you need a minimal `arch.make` file.  
The content of `arch.make`, which should be located in the top directory, can be this
minimal content (please correct tabulators to conform to `Makefile` standards):

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

The `$(INC)` is needed for internal reasons, (sorry about the quick mock-up)...

Type `make` and possibly `make check` to run the tests in the [test](test/) directory.

### Linking ###

flook consists intrinsically of 4 libraries:

1. Lua library (`-llua`),
2. fortran to Lua interface (`-lflu`), see [Thanks](#thanks),
3. basic fortran Lua interaction layer (`-laotus`), see [Thanks](#thanks),
4. flook (`-lflook`).

However, two variants of linking is available.

#### Compiling for one link ####

flook can combine the libraries from 1. -- 4. into one library.

To do this simply call make with this command

    make liball

which creates `libflook.a` in the top directory. With this library
you only need to link to flook.

To link flook to your program the following can be used in a `Makefile`

    FLOOK_PATH  = /path/to/flook/parent
    FLOOK_LIBS  = -L$(FLOOK_PATH)/src -lflookall -ldl
    FLOOK_INC   = -I$(FLOOK_PATH)/src

For the sources that you compile you need to add `$(FLOOK_INC)` to the command line, whilst 
for linking the program you need `$(FLOOK_LIBS)` on the command line.
Note that `-ldl` is a requirement for the Lua library, and per-see is more
difficult to incorporate. 

#### Direct linking ####

When [compiling for one link](#compiling-for-one-link) does not work, the following method is less restrictive on the commands used.

In order to link to flook you can use this template (`Makefile`) for
include statements and library linking (note that you should _not_ switch the order of these statements):

    FLOOK_PATH  = /path/to/flook/parent
    FLOOK_LIBS  = -L$(FLOOK_PATH)/src -lflook
    FLOOK_LIBS += -L$(FLOOK_PATH)/aotus/source -laotus
    FLOOK_LIBS += -L$(FLOOK_PATH)/aotus/LuaFortran -lflu
    FLOOK_LIBS += -L/path/to/lua/lib -llua -ldl
    FLOOK_INC   = -I$(FLOOK_PATH)/src

For the sources that you compile you need to add `$(FLOOK_INC)` to the command line, whilst 
for linking the program you need `$(FLOOK_LIBS)` on the command line.

## Examples ##

Several examples exists in the [test](test/) directory where one 
is shown in the following example:

@include exp_flook.f90

The above program is a fortran program which communicates with an embedded Lua
environment. It communicates with Lua 6 times and allows retrieval of elements
and changing the elements at each communication point.  
The communicating Lua code looks like this:

@include exp_flook.lua


## Contributions, issues and bugs ##

I would advice any users to contribute as much feedback and/or PR to further maintain and expand this library.

Please do not hesitate to contribute!

If you find any bugs please form a [bug report][issue].

If you have a fix please consider adding a [pull request][pr].


### Thanks ###

First, I owe [Harald Klimach](https://bitbucket.org/haraldkl) a big thanks 
for the creation of [aotus] for the Lua-Fortran embedment.

Second, I thank [James Spencer](https://github.com/jsspencer) for help regarding the [aotus] API.

Third, I thank [ESL] for hosting a workshop for me to participate 
and create the initial release.

[flook@git]: https://github.com/ElectronicStructureLibrary/flook
[aotus]: https://bitbucket.org/haraldkl/aotus
[ESL]: http://esl.cecam.org/
[issue]: https://github.com/ElectronicStructureLibrary/flook/issues
[pr]: https://github.com/ElectronicStructureLibrary/flook/pulls
