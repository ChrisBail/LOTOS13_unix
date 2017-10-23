# Start of the makefile
# Defining variables
objects = start.o
f90comp = gfortran
switch = -O3
executable = start.exe
# Makefile
all : $(executable)
$(executable) : $(objects)
	$(f90comp) -o $(executable) $(switch) $(objects)
	rm $(objects)
%.o: %.f90
	$(f90comp) -c $(switch) $<
# Cleaning everything
clean: 
	rm $(objects)
	rm $(executable)
# End of the makefile
