FC=mpif90
FFLAGS=-c -Wall
LDFLAGS=
SOURCES=mpi_tools.f90 jacobian_construction.f90 main.f90 
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=jac_construct

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm $(EXECUTABLE) *.mod *.o
