# This contains default compiler options
# for the GNU vendor compiler suite.
V_VENDOR = clang

# Compiler binaries
V_CC = clang
V_CXX = clang
# What to do here?
V_FC = gfortran
V_CPP = clang -E -P -x c
V_FPP = gfortran -E -P -x c


V_MPICC = mpicc
V_MPICXX = mpic++
V_MPIFC = mpif90

# Flag for output directory of modules
V_FC_MODDIR = -J

# Flag for PIC compilation
V_PIC = -fPIC

# default compiler flags
V_CFLAGS =
V_CXXFLAGS =
V_FFLAGS =

# default compiler flags for debugging
V_FLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries
V_CFLAGS_debug = $(V_FLAGS_debug)
V_CXXFLAGS_debug = $(V_FLAGS_debug)
V_FFLAGS_debug = $(V_FLAGS_debug)

# These are clang compiler options
V_FLAGS_weak = -O1
V_CFLAGS_weak = $(V_FLAGS_weak)
V_CXXFLAGS_weak = $(V_FLAGS_weak)
V_FFLAGS_weak = $(V_FLAGS_weak)

V_FLAGS_medium = -O2
V_CFLAGS_medium = $(V_FLAGS_medium)
V_CXXFLAGS_medium = $(V_FLAGS_medium)
V_FFLAGS_medium = $(V_FLAGS_medium)

V_FLAGS_hard = -O3
V_CFLAGS_hard = $(V_FLAGS_hard)
V_CXXFLAGS_hard = $(V_FLAGS_hard)
V_FFLAGS_hard = $(V_FLAGS_hard)


# Local Variables:
#  mode: makefile-gmake
# End:
