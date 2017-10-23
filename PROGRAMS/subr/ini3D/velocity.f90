function velocity(x,y,z,ips)
common/flat_sf/key_flat1
real ini3D_xyz_lin_v ! CB

vel_min=0.5

rz=6371

velocity=0

!depth = sph_flat(key_flat1,x,y,z) ! CB deprecated

!v0=vrefmod(depth,ips) !CB deprecated because we use 3D block

v0 = ini3D_xyz_lin_v(x,y,z,ips) ! CB get velocity from initial 3D block

if (v0.eq.0.) return ! CB if source is out of bound

!write(*,*) 'x=', x,'y=',y,'z=',z,'v0=',v0 !CB

dv_apr= 0.01 * vert_anom(x,y,z,ips) * v0

dv = anom_3D_xyz_lin_v(x,y,z,ips)

velocity = v0 + dv + dv_apr

if(velocity .lt. vel_min) velocity = vel_min ! CB


return
end
