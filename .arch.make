CC = gcc
FC = gfortran
CFLAGS = -g
FFLAGS = -g
# If needed use PLATFORM = ?
# for different platform.
.f90.o:
	$(FC) -c $(FFLAGS) $(INC) $<
.F90.o:
	$(FC) -c $(FFLAGS) $(INC) $<
.c.o:
	$(CC) -c $(CFLAGS) $(INC) $<
