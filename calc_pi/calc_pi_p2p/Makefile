FC=mpif90
FFLAGS=-c -03 -Wall
LDFLAGS=
SOURCES=mpi_pi_send.f 
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=calc_pi_p2p

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.f
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm $(EXECUTABLE) *.mod *.o
