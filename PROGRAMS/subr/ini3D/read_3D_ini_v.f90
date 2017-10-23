subroutine read_3D_ini_v(ar,md,vsdepth)
character*8 ar,md
character*1 ps,rm
character*200 input_file_1, input_file
character*200 input_file_2
logical flag_P,flag_S !CB
integer vsdepth
real vp_z,vs_z,z_value,vpvs_ratio
common/mod_3D_ini/dv_3D(2,300,300,150),xx1,nxx,dxx,yy1,nyy,dyy,zz1,nzz,dzz ! CB
common/refmod/nrefmod,zref(600),vref(600,2) ! CB
common/zlimits/zmin,zmax

! Call subroutines to apply vs dependency with depth

if ( vsdepth == 1 ) call read_vref(ar,md)

! Start process

nxmax=300
nymax=300
nzmax=150

nxx=0
nyy=0
nzz=0

flag_S= .true.


! Define input files for both P and S model
input_file_1='../../../DATA/'//ar//'/'//md//'/ref_3D_mod1.dat' ! P model
!input_file_1='/media/baillard/Shared/Dropbox/_Moi/Projects/Axial/PROG/LOTOS_utilities/VGRID_x15_y15_z4_100m.lotos'

input_file_2='../../../DATA/'//ar//'/'//md//'/ref_3D_mod2.dat' ! S model
!input_file_2='../../../DATA/data/ref_3D_mod2.dat' ! S model

! Check existence of the P and S files

inquire( file=input_file_1 , exist=flag_P )
if (.not. flag_P ) then
        write(*,*), "Initial P 3D model missing, abort!"
        return
end if

inquire( file=input_file_2 , exist=flag_S )
if (.not. flag_S ) then
        if ( vsdepth == 0 ) write(*,*), "Initial S 3D model missing, constant 1.73 Vp/Vs used instead" 
        if ( vsdepth == 1 ) write(*,*), "Initial S 3D model missing, Vs depending on refmod.dat used instead"
        flag_S= .false.
end if

! Start assignment of variables

dv_3D=0

do ips=1,2

        if ( ips==1 ) then
                input_file=input_file_1 ! Load P               
        else if (( ips == 2 ) .and. ( flag_S .eqv. .true.))  then
                input_file=input_file_2 ! Load S
        else
                input_file=input_file_1
        end if

        open(1,file=input_file,form='unformatted',access='stream')
        read(1,end=13)xx10,nxx0,dxx0
        !write(*,*)xx10,nxx0,dxx0
        if(nxx0.eq.0) cycle
        if(nxx0.gt.nxmax) then
                write(*,*)' nxx > nxmax!'
                write(*,*)' Value of the nxx in common file "mod_3D" should be increased'
                pause
        end if
        xx1=xx10
        nxx=nxx0
        dxx=dxx0

        read(1)yy1,nyy,dyy
        !write(*,*)xx10,nxx0,dxx0
        if(nyy.gt.nymax) then
                write(*,*)' nyy > nymax!'
                write(*,*)' Value of the nyy in common file "mod_3D" should be increased'
                pause
        end if


        read(1)zz1,nzz,dzz
        !write(*,*)zz1,nzz,dzz
        if(nzz.gt.nzmax) then
                write(*,*)' nzz > nzmax!'
                write(*,*)' Value of the nzz in common file "mod_3D" should be increased'
                pause
        end if
        !write(*,*)' nzz=',nzz(ips)
        do izz=1,nzz
                read(1)((dv_3D(ips,ixx,iyy,izz),ixx=1,nxx),iyy=1,nyy)
        end do
        close(1)

        !write(*,*) flag_S, ips
        if (( ips==2 ) .and. ( flag_S .eqv. .false. )) then
                if ( vsdepth == 1) then
                        do izz=1,nzz
                                z_value=zz1+(izz-1)*dzz
                                vp_z=vrefmod(z_value,1)
                                vs_z=vrefmod(z_value,2)
                                vpvs_ratio=vp_z/vs_z
                                !write(*,*) 'z_value=',z_value,'vp=',vp_z,'vs=',vs_z
                                dv_3D(ips,:,:,izz)=dv_3D(ips,:,:,izz)/vpvs_ratio
                                !write(*,*) 'dv_3D(1,1,1,iz)=',dv_3D(1,1,1,izz)
                                !write(*,*) 'dv_3D(2,1,1,iz)=',dv_3D(2,1,1,izz)
                        end do
                else
                       !write(*,*) dv_3D(2,1,1,1)
                       dv_3D(ips,:,:,:)=dv_3D(ips,:,:,:)/1.73
                       !write(*,*) dv_3D(2,1,1,1)
               end if

                ! Write output to ref_3D_mod2.dat (S-file) to be tested
                
                open(11,file='../../../DATA/'//ar//'/'//md//'/ref_3D_mod2.dat',form='unformatted',access='stream')
                write(11)xx1,nxx,dxx
                write(11)yy1,nyy,dyy
                write(11)zz1,nzz,dzz

                do izz=1,nzz
                        write(11)((dv_3D(ips,ixx,iyy,izz),ixx=1,nxx),iyy=1,nyy)
                end do

                close(11)

        end if

        !write(*,*)' dv_3D(1,1,1,1)=',dv_3D(1,1,1,1)
        !pause

        !write(*,*)' nxx=',nxx,' nyy=',nyy,' nzz=',nzz
13	continue
end do

return
end
