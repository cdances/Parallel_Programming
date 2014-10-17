FC=mpif90
FFLAGS=-c -Wall
LDFLAGS=
SOURCES=hello_world.f90 main.f90 
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=hello_world

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

clean: 
	rm *.o *.mod $(EXECUTABLE)
