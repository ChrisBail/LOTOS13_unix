character*8 ar,md,line
logical file_exist
character*200 input_file


iter=4
ar='area'
md='model'


!write(*,*)iter-1

call read_3D_ini_v(ar,md,iter-1)

return

input_file='/media/baillard/Shared/Dropbox/_Moi/Projects/Axial/PROG/LOTOS_utilities/VGRID_x15_y15_z4_100m.lotos'


! Check that initial 3D model exists

inquire( file=input_file , exist=file_exist )
if (.not. file_exist ) then
        write(*,*), "Initial 3D model absent, abort!"
        return
end if

return

open(1,file='../../../DATA/'//ar//'/'//md//'/data/refmod.dat',err=81)

write(*,*) 'hello'
81 close(1)
write(*,*) 'nope'
return

x=5
y=1
z=2
ips=1

dv=anom_3D_xyz_lin_v(x,y,z,ips)

write(*,*) dv

end
