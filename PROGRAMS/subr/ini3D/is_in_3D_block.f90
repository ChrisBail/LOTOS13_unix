function is_in_3D_block(xxx,yyy,zzz)

common/mod_3D_ini/dv_3D(2,300,300,150),xx1,nxx,dxx,yy1,nyy,dyy,zz1,nzz,dzz
logical is_in_3D_block !CB

! Function made to tell if source/value is out of the 3D model or not
! This can be used to reject some events and to avoid to compute rays
! that are out of bounds

xx2=xx1+(nxx-1)*dxx
yy2=yy1+(nyy-1)*dyy
zz2=zz1+(nzz-1)*dzz

is_in_3D_block= .false.

!write(*,*) xx1,xx2,yy1,yy2,zz1,zz2

if((xxx-xx1)*(xxx-xx2).ge.0) return ! check that given x is in the range (CB)
if((yyy-yy1)*(yyy-yy2).ge.0) return
if((zzz-zz1)*(zzz-zz2).ge.0) return

is_in_3D_block= .true.

return

end


