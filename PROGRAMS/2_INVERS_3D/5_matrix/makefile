# Start of the makefile
# Defining variables
objects = anom_3D_xyz_lin_v.o flat_sph.o read_3D_mod_v.o read_vref.o sph_flat.o velocity.o vert_anom.o vrefmod.o is_in_3D_block.o ini3D_xyz_lin_v.o read_3D_ini_v.o matr.o 
tmp_src = anom_3D_xyz_lin_v.f90 flat_sph.f90 read_3D_mod_v.f90 read_vref.f90 sph_flat.f90 velocity.f90 vert_anom.f90 vrefmod.f90 is_in_3D_block.f90 ini3D_xyz_lin_v.f90 read_3D_ini_v.f90 
f90comp = gfortran
switch = -O3
executable = matr.exe
# Makefile
all : $(executable)
$(executable) : $(objects)
	rm $(tmp_src)	
	$(f90comp) -o $(executable) $(switch) $(objects)
	rm $(objects)
anom_3D_xyz_lin_v.f90:
	cp ../../subr/3D_model/anom_3D_xyz_lin_v.f90 anom_3D_xyz_lin_v.f90
flat_sph.f90:
	cp ../../subr/convers/flat_sph.f90 flat_sph.f90
read_3D_mod_v.f90:
	cp ../../subr/3D_model/read_3D_mod_v.f90 read_3D_mod_v.f90
read_vref.f90:
	cp ../../subr/rays_locate/read_vref.f90 read_vref.f90
sph_flat.f90:
	cp ../../subr/convers/sph_flat.f90 sph_flat.f90
velocity.f90:
	#cp ../../subr/3D_model/velocity.f90 velocity.f90
	cp ../../subr/ini3D/velocity.f90 velocity.f90
vert_anom.f90:
	cp ../../subr/synth/vert_anom.f90 vert_anom.f90
vrefmod.f90:
	cp ../../subr/rays_locate/vrefmod.f90 vrefmod.f90
is_in_3D_block.f90:
	cp ../../subr/ini3D/is_in_3D_block.f90 is_in_3D_block.f90
ini3D_xyz_lin_v.f90:
	cp ../../subr/ini3D/ini3D_xyz_lin_v.f90 ini3D_xyz_lin_v.f90
read_3D_ini_v.f90:
	cp ../../subr/ini3D/read_3D_ini_v.f90 read_3D_ini_v.f90
%.o: %.f90
	$(f90comp) -c $(switch) $<
# Cleaning everything
clean: 
	rm $(objects)
	rm $(tmp_src)
	rm $(executable)
# End of the makefile
