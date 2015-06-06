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
