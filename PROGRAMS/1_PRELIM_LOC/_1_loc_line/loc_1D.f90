USE DFPORT
character*8 ar,md,line
real dx_it(10),dy_it(10),dz_it(10)
real res_it1(10),res_it2(10),wps_it(10)
integer kodes(10,20000)
real gkode(20000)
real xvert1(20),yvert1(20),xvert2(20),yvert2(20)
character*2 ver
real xmark(200,20),ymark(200,20),smark(200,20)
integer nmark(100),ngood(500)
character*1 it0,it1


common/refmod/nref,href(600),vref(600,2)

common/pi/pi,per
common/krat/nkrat,istkr(500),ipskr(500),ndrkr(500),tobkr(500),trfkr(500)
common/stations/nst,xstat(500),ystat(500),zstat(500)

common/mod_2d/xmod1,nxmod,dxmod,ymod1,nymod,dymod, dv_mod(300,300)
common/ray_param/ds_ini,ds_part_min,bend_min0,bend_max0
common/loc_param/res_loc1,res_loc2,w_P_S_diff
common/ray/ nodes,xray(1000),yray(1000),zray(1000)


one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0

res_loc1=0.
res_loc2=50./1000.
w_P_S_diff=10

nxmod=0


open(1,file='../../../model.dat')
read(1,'(a8)')ar
read(1,'(a8)')md
read(1,*)ittt
221 close(1)
write(it0,'(i1)')ittt-1
write(it1,'(i1)')ittt
write(*,*)' area=',ar,' model=',md,' it=',it1

write(*,*)' Location of sources using straight lines in the current 1D model'


if(ittt.eq.1) then
	open(1,file='../../../DATA/'//ar//'/'//md//'/ref_start.dat')
else
	open(1,file='../../../DATA/'//ar//'/'//md//'/data/ref'//it0//'.dat')
end if
read(1,*,end=81)vpvs
iref=0
82 continue
    read(1,*,end=81)z,vp,vs
    iref=iref+1
    href(iref)=z
    vref(iref,1)=vp
    if(vpvs.lt.0.000001) then
        vref(iref,2)=vs
    else
        vref(iref,2)=vref(iref,1)/vpvs
    end if
    !write(*,*)' href=',href(iref),' vp=',vref(iref,1),' vs=',vref(iref,2)
    goto 82
81 close(1)
nref=iref
write(*,*)' nrefmod=',nref
!pause


!******************************************************************
key_ft1_xy2=1
open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
    read(1,'(a8)',end=513)line
    if(line.eq.'GENERAL ') goto 514
end do
513 continue
write(*,*)' cannot find GENERAL INFORMATION in MAJOR_PARAM.DAT!!!'
pause
514 continue
read(1,*)
read(1,*)
read(1,*)
read(1,*)
read(1,*,end=441,err=441)key_ft1_xy2
read(1,*,end=441,err=441)key_true1
441 close(1)
!******************************************************************

open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'LOC_PARA') goto 554
end do
553 continue
write(*,*)' cannot find TRACE PARAMETERS in MAJOR_PARAM.DAT!!!'
pause
554 continue
read(1,*)
read(1,*)ds_ini
read(1,*)ds_segm_min
read(1,*)bend_min0
read(1,*)bend_max0
close(1)
!******************************************************************


w_qual=1
open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
	read(1,'(a8)',end=543)line
	if(line.eq.'LIN_LOC_') goto 544
end do
543 continue
write(*,*)' cannot find LIN_LOC_PARAM in MAJOR_PARAM.DAT!!!'
pause

544 continue
read(1,*)krat_min
read(1,*)dist_max
read(1,*)wgs
read(1,*)dist_limit	!=100
read(1,*)n_pwr_dist	!=1
read(1,*)ncyc_av	!=10
read(1,*)
read(1,*)	! For output:
read(1,*)bad_max	!=30
read(1,*)res_1_km
read(1,*)sss_max
read(1,*)
read(1,*)nfreq_print
read(1,*)
read(1,*)niter_loc
do it=1,niter_loc
	read(1,*)
	read(1,*)dx_it(it),dy_it(it),dz_it(it)
	!write(*,*)dx_it(it),dy_it(it),dz_it(it)
	read(1,*)res_it1(it)
	read(1,*)res_it2(it)
	read(1,*)wps_it(it)
end do
close(1)

call read_z_lim(ar,md)
call read_topo(ar)

i=system('mkdir ..\..\..\TMP_files\loc')
i=system('mkdir ..\..\..\PICS\'//ar//'\'//md//'\LOC')

xstart=0; ystart=0; zstart=0
k_star_po=0
open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
do i=1,10000
	read(1,'(a8)',end=643)line
	if(line.eq.'START_PO') goto 644
end do
643 continue
!write(*,*)' cannot find START_POINT in MAJOR_PARAM.DAT!!!'
goto 645

644 continue
k_star_po=1
read(1,*)xstart,ystart,zstart 
645 close(1) 


if(key_ft1_xy2.eq.2) then
 
    open(1,file='../../../DATA/'//ar//'/inidata/stat_xy.dat')
    open(11,file='../../../DATA/'//ar//'/'//md//'/data/stat_xy.dat')
    nst=0
    133	read(1,*,end=144)xst,yst,zst
	    write(11,*)xst,yst,zst
	    nst=nst+1
	    xstat(nst)=xst
	    ystat(nst)=yst
	    zstat(nst)=zst
	    goto 133
    144	close(1)
    close(11)
    write(*,*)' nst=',nst

else if(key_ft1_xy2.eq.1) then

    open(1,file='../../../DATA/'//ar//'/'//md//'/MAJOR_PARAM.DAT')
    do i=1,10000
	    read(1,'(a8)',end=593)line
	    if(line.eq.'AREA_CEN') goto 594
    end do
    593 continue
    write(*,*)' cannot find AREA CENTER in MAJOR_PARAM.DAT!!!'
    pause

    594 read(1,*)fi0,tet0
    !write(*,*)fi0,tet0
    close(1)

    ! Read the coordinates of the stations
    open(1,file='../../../DATA/'//ar//'/inidata/stat_ft.dat')
    open(12,file='../../../DATA/'//ar//'/'//md//'/data/stat_xy.dat')
    nst=0
    33	read(1,*,end=44)fi,tet,zst
	    call SFDEC(fi,tet,0.,X,Y,Z,fi0,tet0)
	    nst=nst+1
	    xstat(nst)=x
	    ystat(nst)=y
	    zstat(nst)=zst
	    write(12,*)xstat(nst),ystat(nst),zstat(nst)
	    !write(*,*)xstat(nst),ystat(nst),zstat(nst)
	    goto 33
    44	close(12)
    close(1)
else
    write(*,*)' key_ft1_xy2=',key_ft1_xy2
    stop
end if
write(*,*)' nst=',nst



if(ittt.eq.1) then
    open(1,file='../../../DATA/'//ar//'/'//md//'/data/rays_selected.dat',form='binary')
else
    open(1,file='../../../DATA/'//ar//'/'//md//'/data/rays_it'//it0//'.dat',form='binary')
end if
open(11,file='../../../DATA/'//ar//'/'//md//'/data/rays_it'//it1//'.dat',form='binary')

nzt=0
errtot=0
nerr=0
nztgood=0
nrp=0
nrs=0
nray=0

dispold=0
dispnew=0

228	continue
    read(1,end=229)xzt,yzt,zzt,nkrat


!write(*,*)' TRUE LOCATION:',xzt0,yzt0
!write(*,*)
    if(k_star_po.eq.1) then
	xzt=xstart
	yzt=ystart
	zzt=zstart
    end if

    xold=xzt; yold=yzt; zold=zzt

    do ikr=1,nkrat
        read(1)ips,ist,tobs,tref
        dispold=dispold+abs(tobs-tref)
        !write(*,*)ikr,ips,ist,tobs
        ipskr(ikr)=ips
        istkr(ikr)=ist
        tobkr(ikr)=tobs
    end do

    xmax=xzt
    ymax=yzt
    zmax=zzt

    nzt=nzt+1
    !if(nzt.lt.3) goto 228

    do iter=niter_loc,niter_loc

        res_loc1=res_it1(iter)
        res_loc2=res_it2(iter)
        w_P_S_diff=wps_it(iter)
        dx_loc=dx_it(iter)
        dy_loc=dy_it(iter)
        dz_loc=dz_it(iter)


        call goal_fun_lin(xmax,ymax,zmax, aver,goal)
        !write(*,*)' old:',xmax,ymax,zmax,goal
        gmax=goal

        nkode=1
        kodes(1,nkode)=0
        kodes(2,nkode)=0
        kodes(3,nkode)=0
        gkode(nkode)=gmax
        ixmax1=0
        iymax1=0
        izmax1=0

        282 continue
        index=0
        do iix=1,5
            ix=ixmax1+iix-3
            dx=dx_loc*ix
            do iiy=1,5
                iy=iymax1+iiy-3
                dy=dy_loc*iy
                call decsf(xmax+dx,ymax+dy,0.,fi0,tet0,fff,ttt,h)
                zlim=z_lim(fff,ttt)
                zlim_up=relief_surf(fff,ttt)
                !write(*,*)' ix=',ix,' iy=',iy
                !write(*,*)' fff=',fff,' ttt=',ttt,' zlim_up=',zlim_up
                do iiz=1,5
                    iz=izmax1+iiz-3
                    dz=dz_loc*iz
                    if(zmax+dz.gt.zlim) cycle
                    if(zmax+dz.lt.zlim_up) cycle

                    if(nkode.ne.0) then
                        do ik=1,nkode
                            if(kodes(1,ik).eq.ix.and.kodes(2,ik).eq.iy.and.kodes(3,ik).eq.iz) goto 281
                        end do
                    end if



                    call goal_fun_lin(xmax+dx,ymax+dy,zmax+dz, aver,goal)


                    !write(*,'(3i3,3f6.1,f7.3)')ix,iy,iz,xmax+dx,ymax+dy,zmax+dz,goal

                    nkode=nkode+1
                    kodes(1,nkode)=ix
                    kodes(2,nkode)=iy
                    kodes(3,nkode)=iz
                    gkode(nkode)=goal

                    if(goal.le.gmax) cycle
                    index=1
                    ixmax=ix
                    iymax=iy
                    izmax=iz
                    gmax=goal
                    !write(*,*)xmax+dx,ymax+dy,zmax+dz,goal
281		     continue
                end do
            end do
        end do

        !write(*,*)ixmax,iymax,izmax,gmax

        if(index.eq.1) then
            ixmax1=ixmax
            iymax1=iymax
            izmax1=izmax
            goto 282
        end if

        xmax=xmax+dx_loc*(ixmax1)
        ymax=ymax+dy_loc*(iymax1)
        zmax=zmax+dz_loc*(izmax1)

        !write(*,*)' after iteration:',iter
        !write(*,*)' x=',xmax,' y=',ymax,' z=',zmax,' g=',gmax



    end do

    xzt=xmax
    yzt=ymax
    zzt=zmax

    call goal_fun_lin(xold,yold,zold, aver,gold)
    call goal_fun_lin(xzt,yzt,zzt, aver,goal)

    nbad=0
    ngood=1
    do i=1,nkrat
        tobkr(i)=tobkr(i)-aver
        ist=istkr(i)
        xs=xstat(ist)
        ys=ystat(ist)
        zs=zstat(ist)
        dist=sqrt((xs-xzt)*(xs-xzt)+(ys-yzt)*(ys-yzt)+(zs-zzt)*(zs-zzt))
        if(dist.gt.sss_max) dist=sss_max
        res_limit=dist*res_1_km
        if(ipskr(i).eq.2)res_limit=res_limit*wgs
        dt=tobkr(i)-trfkr(i)
        !write(*,*)ipskr(i),istkr(i),dt,res_limit
        if(abs(dt).lt.res_limit) cycle
        ngood(i)=0
        nbad=nbad+1
    end do

    nk=nkrat-nbad
    abad=nbad
    akrat=nkrat
    ratio_bad=(abad/akrat)

    !write(*,*)' nbad=',nbad,' ngood=',nk
    if(ratio_bad*100.gt.bad_max) goto 228
    if(nk.lt.krat_min) goto 228


    write(11)xzt,yzt,zzt,nk
    do i=1,nkrat
        if(ngood(i).eq.0) cycle
        write(11)ipskr(i),istkr(i),tobkr(i),trfkr(i)
        dispnew=dispnew+abs(tobkr(i)-trfkr(i))
        dtot=dtot+abs(tobkr(i)-trfkr(i))
        nray=nray+1
        if(ipskr(i).eq.1) nrp=nrp+1
        if(ipskr(i).eq.2) nrs=nrs+1
    end do

    nztgood=nztgood+1

    !nfreq_print=1
    if(mod(nztgood,nfreq_print).eq.0) then
        write(*,488)nzt,xold,yold,zold,gold
        write(*,489)nztgood,xzt,yzt,zzt,goal
        488	format(i6,' Old: x=',f8.2,' y=',f8.2,' z=',f8.2,' ank=',f7.2)
        489	format(i6,' New: x=',f8.2,' y=',f8.2,' z=',f8.2,' ank=',f7.2)
        dcur=dtot/nray
        dold=dispold/nray
        dnew=dispnew/nray
        perc=100*(dold-dnew)/dold
        write(*,*)' dold=',dold,' dnew=',dnew,' reduction=',perc
        write(*,*)' nkrat=',nkrat,' nk=',nk,' nray=',nray
        write(*,*)
    end if

    goto 228
229 close(1)
close(11)

dold=dispold/nray
dnew=dispnew/nray
perc=100*(dold-dnew)/dold

if(ittt.eq.1) then
    open(11,file='../../../DATA/'//ar//'/'//md//'/data/1d_info.dat',status='replace')
else
    open(11,file='../../../DATA/'//ar//'/'//md//'/data/1d_info.dat',status='old',access='append')
end if
write(11,*)ittt,nray,dold,dnew,perc
write(*,*)ittt,nray,' d-old-new=',dold,dnew,' perc=',perc
close(11)

write(*,*)' nztgood=',nztgood,' nray=',nray
write(*,*)' nrp=',nrp,' nrs=',nrs

stop
end