FC=mpif90
FFLAGS=-c -O3 -Wall
LDFLAGS=
SOURCES=prime_mpi.f90 
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=calc_primes

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm $(EXECUTABLE) *.mod *.o
