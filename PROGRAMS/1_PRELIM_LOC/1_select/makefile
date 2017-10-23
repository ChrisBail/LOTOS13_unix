# Start of the makefile
# Defining variables
objects = select.o
f90comp = gfortran
switch = -O3
# Makefile
all : refrays.exe
refrays.exe : $(objects)
	$(f90comp) -o select.exe $(switch) $(objects)
	rm $(objects)
%.o: %.f90
	$(f90comp) -c $(switch) $<
# Cleaning everything
clean: 
	rm $(objects)
	rm select.exe
# End of the makefile
