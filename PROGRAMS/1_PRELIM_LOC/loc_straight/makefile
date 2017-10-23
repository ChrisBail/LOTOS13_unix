# Start of the makefile
# Defining variables
objects = loc_1D.o dv_mod_2d.o goal_fun_lin.o read_vref.o straight_line.o velocity.o vrefmod.o decsf.o flat_sph.o h_lim.o int_best.o read_topo.o read_z_limit.o relief_surf.o sfdec.o z_lim.o 
tmp_src = dv_mod_2d.f90 goal_fun_lin.f90 read_vref.f90 straight_line.f90 velocity.f90 vrefmod.f90 decsf.f90 flat_sph.f90 h_lim.f90 int_best.f90 read_topo.f90 read_z_limit.f90 relief_surf.f90 sfdec.f90 z_lim.f90 
f90comp = gfortran
switch = -O3
# Makefile
all : loc_1D.exe
loc_1D.exe : $(objects)
	rm $(tmp_src)	
	$(f90comp) -o loc_1D.exe $(switch) $(objects)
	rm $(objects)
dv_mod_2d.f90 :
	cp ../../subr/velocity/dv_mod_2d.f90 dv_mod_2d.f90
goal_fun_lin.f90 :
	cp ../../subr/locate/goal_fun_lin.f90 goal_fun_lin.f90
read_vref.f90 :
	cp ../../subr/rays_locate/read_vref.f90 read_vref.f90
straight_line.f90 :
	cp ../../subr/bending/straight_line.f90 straight_line.f90
velocity.f90 :
	cp ../../subr/velocity/velocity.f90 velocity.f90
vrefmod.f90 :
	cp ../../subr/rays_locate/vrefmod.f90 vrefmod.f90
decsf.f90 :
	cp ../../subr/convers/decsf.f90 decsf.f90
flat_sph.f90 :
	cp ../../subr/convers/flat_sph.f90 flat_sph.f90
h_lim.f90 :
	cp ../../subr/rays_locate/h_lim.f90 h_lim.f90
int_best.f90 :
	cp ../../subr/other/int_best.f90 int_best.f90
read_topo.f90 :
	cp ../../subr/rays_locate/read_topo.f90 read_topo.f90
read_z_limit.f90 :
	cp ../../subr/rays_locate/read_z_limit.f90 read_z_limit.f90
relief_surf.f90 :
	cp ../../subr/rays_locate/relief_surf.f90 relief_surf.f90
sfdec.f90 :
	cp ../../subr/convers/sfdec.f90 sfdec.f90
z_lim.f90 :
	cp ../../subr/rays_locate/z_lim.f90 z_lim.f90
%.o: %.f90
	$(f90comp) -c $(switch) $<
# Cleaning everything
clean: 
	rm -f $(objects)
	rm -f $(tmp_src)
	rm -f loc_1D.exe
# End of the makefile
