character*8 ar,md,line
character*1 it0,it1
real xstat(9000),ystat(9000),zstat(9000)
real tall(20),hall(20),aall(20)
real amat1(1000),amat_line(1000)
integer kmat_line(1000)

common/pi/pi,per
common /ray_path/npray,xray(5000),yray(5000),zray(5000)
common/grid/zgrmax,dzlay,dsmin
common/refmod/nref,zref(600),vref(600,2)


one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0

dsmin=1
dzlay=1
zgrmax=300


open(1,file='../../../model.dat')
read(1,'(a8)')ar		! code of the area
read(1,'(a8)')md		! code of the area
read(1,*)iter
close(1)
write(it0,'(i1)')iter-1
write(it1,'(i1)')iter

write(*,*)' MATRIX: area=',ar,' model=',md,' iter=',iter


open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'1D MODEL') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in MAJOR_PARAM.DAT!!!'
pause

554 read(1,*)
read(1,*)zlim1,dzz,dzs
read(1,*)dsmin,dzlay,zgrmax
read(1,*)dz_par
close(1)

open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
	read(1,'(a8)',end=453)line
	if(line.eq.'LOC_PARA') goto 454
end do
453 continue
write(*,*)' cannot find TRACE PARAMETERS in MAJOR_PARAM.DAT!!!'
pause
454 continue
read(1,*)
read(1,*)ds_ini
close(1)

!write(*,*)zlim1,dzz,dzs

if(iter.eq.1) then
	open(1,file='../../../DATA/'//ar//'/'//md//'/ref_start.dat')
else
	open(1,file='../../../DATA/'//ar//'/'//md//'/data/ref'//it0//'.dat')
end if
read(1,*,end=81)vpvs
iref=0
82 continue
    read(1,*,end=81)z,vp,vs
    iref=iref+1
    zref(iref)=z
    vref(iref,1)=vp
    if(vpvs.lt.0.000001) then
        vref(iref,2)=vs
    else
        vref(iref,2)=vref(iref,1)/vpvs
    end if
    goto 82
81 close(1)
nref=iref
write(*,*)' nref=',nref


! Read the coordinates of the stations
open(1,file='../../../DATA/'//ar//'/'//md//'/data/stat_xy.dat')
nst=0
33	read(1,*,end=44)xst,yst,zst
	nst=nst+1
	xstat(nst)=xst
	ystat(nst)=yst
	zstat(nst)=zst
	goto 33
44	close(1)
write(*,*)' nst=',nst

open(11,file='../../../TMP_files/tmp/matr_1d.dat',form='binary')

open(1,file='../../../DATA/'//ar//'/'//md//'/data/rays_it'//it1//'.dat',form='binary')

nzt=0
nray=0
npar_max=0
nonz_tot=0
    992	continue
    read(1,end=991)xzt,yzt,zzt,nkrat
    nzt=nzt+1
    do ikr=1,nkrat
        read(1)ips,ist,tobs,tref
        res=tobs-tref
        nray=nray+1
        xst=xstat(ist)
        yst=ystat(ist)
        zst=0

        dshor = sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
        dsver = abs(zzt-zst)
        dstot = sqrt(dshor*dshor+dsver*dsver)
        npray = dstot/ds_ini+1


        ds = dstot/(npray-1)
        if(nodes.eq.1) then
	        nodes=2
	        ds=dstot
        end if

        !write(*,*)' ztr:',xzt,yzt,zzt
        do inode = 1,npray
	        sss = ds * (inode-1)
	        xray(inode) = xzt + sss * (xst-xzt)/dstot
	        yray(inode) = yzt + sss * (yst-yzt)/dstot
	        zray(inode) = zzt + sss * (zst-zzt)/dstot
	        !write(*,*) sss,xray(inode),yray(inode),zray(inode)
        end do
        !write(*,*)sss,' sta:',xst,yst,zst

! Time along the streight line:

        ttt=0
        do inode=1,npray-1
            x1=xray(inode)
            x2=xray(inode+1)
            xm=(x1+x2)/2.

            y1=yray(inode)
            y2=yray(inode+1)
            ym=(y1+y2)/2.

            z1=zray(inode)
            z2=zray(inode+1)
            zm=(z1+z2)/2.

            ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
            vvv=vrefmod (zm,ips)
            !write(*,*)xm,ym,zm,vvv
            !write(*,*)' vvv=',vvv
            ttt=ttt+ds/vvv
            !write(*,*)' ds=',ds,' sss=',sss
            !write(*,*)' t=',ttt,' s=',sss,' vvv=',vvv
        end do
        !write(*,*)' ttt=',ttt,' sss=',sss


        cosb=(xzt-xst)/dshor
        sinb=(yzt-yst)/dshor
        sina=dshor/dstot
        cosa=(zzt-zst)/dstot
        vzt=vrefmod(zzt,ips)
        dtdd=sina/vzt
        dtdz=-cosa/vzt
        dtdx=dtdd*cosb
        dtdy=dtdd*sinb




        amat1=0
        do il=2,npray
            !write(*,*)' il=',il
            xx1=xray(il-1)
            yy1=yray(il-1)
            zz1=zray(il-1)
            xx2=xray(il)
            yy2=yray(il)
            zz2=zray(il)
            dss=sqrt((xx1-xx2)*(xx1-xx2)+(yy1-yy2)*(yy1-yy2)+(zz1-zz2)*(zz1-zz2))
            xmid=(xx1+xx2)/2
            ymid=(yy1+yy2)/2
            zmid=(zz1+zz2)/2

            vmid=vrefmod(zmid,ips)

            do i_par=1,1000
	            z1=zlim1+(i_par-1)*dz_par
	            z2=zlim1+i_par*dz_par
	            if((zmid-z1)*(zmid-z2).le.0) goto 511
            end do

            write(*,*)' out of the range: zmid=',zmid
            511                 continue
            dv_on_ray_1= (zmid-z2)/(z1-z2)
            dmatr = - dv_on_ray_1 * dss / vmid**2 
            amat1(i_par)=amat1(i_par)+dmatr

            dv_on_ray_2= (zmid-z1)/(z2-z1)
            dmatr = - dv_on_ray_2 * dss / vmid**2 
            amat1(i_par+1)=amat1(i_par+1)+dmatr

        end do



        nonz=0
        do i=1,1000
            if(abs(amat1(i)).lt.1.e-15) cycle
            nonz=nonz+1
            amat_line(nonz)=amat1(i)
            kmat_line(nonz)=i
            if(i.gt.npar_max) npar_max=i
            !write(*,*)nonz,i,amat1(i)
        end do

        nonz_tot = nonz_tot + nonz

        write(11)nonz,res,ips
        !write(*,*)nonz,res,ips
        write(11)nzt,dtdx,dtdy,dtdz
        !write(*,*)nzt,dtdx,dtdy,dtdz
        do i=1,nonz
	        write(11)amat_line(i),kmat_line(i)
	        !write(*,*)amat_line(i),kmat_line(i)
        end do

    end do
goto 992
991 close(1)
close(11)

write(*,*)' npar_max=',npar_max


write(*,*)' nzt=',nzt,' nray=',nray,' nonz_tot=',nonz_tot

open(11,file='../../../DATA/'//ar//'/'//md//'/data/numb_1d.dat')
write(11,*)nray,nonz_tot,npar_max,nzt
close(11)

stop
end