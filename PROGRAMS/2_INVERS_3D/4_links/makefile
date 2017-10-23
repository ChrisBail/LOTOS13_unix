# Start of the makefile
# Defining variables
objects = add_matr.o 
f90comp = gfortran
switch = -O3 -Wall
executable = add_matr.exe
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
