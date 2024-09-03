FC=mpif90
FFLAGS=-g
LDFLAGS=

all:
	$(FC) $(FFLAGS) -o slow_prog slow_prog.f90 $(LDFLAGS)
	$(FC) $(FFLAGS) -o fast_prog fast_prog.f90 $(LDFLAGS)
