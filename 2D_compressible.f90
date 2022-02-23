!==========================================================
!  2D Navier-Stokes equation solver by Sylvain Laizet, 2014
!==========================================================

!START OF THE MAIN PROGRAM
!
program navierstokes
!
  implicit none   !-->all the variables MUST be declared

  integer,parameter :: nx=2049,ny=nx,nt=100,ns=3,nf=3,mx=nf*nx,my=nf*ny
  !size of the computational domain (nx x ny) 
  !size of the exchanger (mx x my)
  !number of time step for the simulation

  !Declaration of variables
  real(8),dimension(nx,ny) :: uuu,vvv,rho,eee,pressure,tmp,rou,rov,wz,tuu,tvv
  real(8),dimension(nx,ny) :: roe,tb1,tb2,tb3,tb4,tb5,tb6,tb7,tb8,tb9
  real(8),dimension(nx,ny) :: tba,tbb,fro,fru,frv,fre,gro,gru,grv,gre,eps,ftp,gtp,scp
  real(8),dimension(mx) :: xx
  real(8),dimension(my) :: yy
  real(8),dimension(mx,my) :: tf
  real(8),dimension(2,ns) :: coef
  integer :: i,j,itemp,k,n,ni,nj,imodulo
  real(8) :: x_length,y_length,CFL,dlx,dx,dyn_viscosity,xkt
  real(8) :: lambda,gamma,chp,eta,dlt,u_mean,v_mean,t_mean,x,y,dy
!**************************************************
  character(len=20) nfichier
!*******************************************
  !Name of the file for visualisation:
  990 format('vort',I4.4)
  imodulo=2500 !snapshots to be saved every imodulo time steps

  ! AB2 temporal scheme itemp=1
  ! RK3 temporal scheme itemp=2
  itemp=1

  ! Subroutine for the initialisation of the variables 
  call initl(uuu,vvv,rho,eee,pressure,tmp,rou,rov,roe,nx,ny,x_length,y_length, &
       dyn_viscosity,lambda,gamma,chp,dlx,eta,eps,scp,xkt)

  !we need to define the time step
  dx = x_length / nx !mesh size in x
  dy = y_length / ny !mesh sixe in y
  CFL = 0.025  !CFL number for time step
  dlt = CFL * dlx
  print *,'The time step of the simulation is', dlt
  
  !Computation of the average velocity and temperature at t=0
  call average(uuu, u_mean, nx, ny)
  call average(vvv, v_mean, nx, ny)
  call average(scp, t_mean, nx, ny)
  write(*,*) 'Average values at t=0', u_mean, v_mean, t_mean

!BEGINNING OF TIME LOOP
  do n=1,nt
   if (itemp.eq.1) then   !TEMPORAL SCHEME AB2
      
      call fluxx(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,tb1,tb2,tb3,tb4, &
            tb5,tb6,tb7,tb8,tb9,tba,tbb,fro,fru,frv,fre,x_length,y_length,dyn_viscosity,lambda,eps, &
            eta,ftp,scp,xkt)

      call adams(rho,rou,rov,roe,fro,gro,fru,gru,frv,grv,&
            fre,gre,ftp,gtp,scp,nx,ny,dlt)
      
      call etatt(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,gamma,chp)
        
   endif
        
   if (itemp.eq.2) then !TEMPORAL SCHEME RK3

      !loop for sub-time steps
      do k=1,ns
         call fluxx(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,tb1,tb2,tb3,tb4,&
               tb5,tb6,tb7,tb8,tb9,tba,tbb,fro,fru,frv,fre,x_length,y_length,dyn_viscosity,lambda,eps,&
               eta,ftp,scp,xkt)
      
         call rkutta(rho,rou,rov,roe,fro,gro,fru,gru,frv,grv,&
               fre,gre,ftp,gtp,nx,ny,ns,dlt,coef,scp,k)
   
         call etatt(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,gamma,chp)
         
      enddo
   endif
     
   !loop for the snapshots, to be save every imodulo
   if (mod(n,imodulo) .eq. 0) then
      !this is design for Gnuplot but feel free to implement your
      !own code if you want to use Matlab or Paraview
      write(nfichier, 990) n/imodulo
      x=0.
      do i=1,mx
         xx(i)=x
         x=x+dx 
      enddo

      y=0.
      do j=1,my
         yy(j)=y
         y=y+dy
      enddo

      !computation of the vorticity
      call derix(vvv,nx,ny,tvv,x_length)
      call deriy(uuu,nx,ny,tuu,y_length)

      do j=1,ny
         do i=1,nx
            wz(i,j)=tvv(i,j)-tuu(i,j)
         enddo
      enddo
      
      !using periodicity we copy the vorticity for the heat exchanger
      do ni=1,nf
         do nj=1,nf
            do j=1,ny
               do i=1,nx
                  tf(i+(ni-1)*nx, j+(nj-1)*ny) = wz(i,j)
               enddo
            enddo
         enddo
      enddo

      !this file will be used by gnuplot for visualisations
      open(21,file=nfichier,form='formatted',status='unknown')
      do j=1,my
         do i=1,mx
            write(21,*) xx(i), yy(j), tf(i,j)
         enddo
         write(21,*)
      enddo
      close(21)  
   endif

   !Computation and print of average values
   call average(uuu,u_mean,nx,ny)
   call average(vvv,v_mean,nx,ny)
   call average(scp,t_mean,nx,ny)
   write(*,*) n, u_mean, v_mean, t_mean

  enddo
  !END OF THE TIME LOOP
end program navierstokes
!
!END OF THE MAIN PROGRAMME
!
!############################################
!
subroutine average(array, mean, nx, ny)
!
!computation of the mean value of a 2D field
!############################################
!
  implicit none
!
  real(8),dimension(nx,ny) :: array
  real(8) :: mean
  integer :: i,j,nx,ny

  mean=0.
  do j=1,ny
   do i=1,nx
      mean = mean + array(i,j)
   enddo
  enddo

  mean=mean/(real(nx*ny))

  return
end subroutine average

subroutine derix(phi,nx,ny,phi_deriv,x_length)
!
!First derivative in the x direction
!
	implicit none

	real(8),dimension(nx,ny) :: phi,phi_deriv
	real(8) :: dlx,x_length,udx
	integer :: i,j,nx,ny
		
	dlx = x_length / nx
	udx = 1. / (2 * dlx)

	!   do j=1,ny
	!    phi_deriv(1,j) = udx * (phi(2,j) - phi(nx,j))
	    !   do i=2,nx-1
	    !      phi_deriv(i,j) = udx * (phi(i+1,j) - phi(i-1,j))
	    !   enddo
	!       phi_deriv(nx,j) = udx * (phi(1,j) - phi(nx-1,j))
	!   enddo
	do CONCURRENT (j=1:ny)
		phi_deriv(1,j) = udx * (phi(2,j) - phi(nx,j))
		do CONCURRENT (i=2:nx-1)
			phi_deriv(i,j) = udx * (phi(i+1,j) - phi(i-1,j))
		enddo
		phi_deriv(nx,j) = udx * (phi(1,j) - phi(nx-1,j))
	enddo

  	return
end subroutine derix

subroutine deriy(phi,nx,ny,dfi,y_length)
!
!First derivative in the y direction
!

  implicit none
  
  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dly,y_length,udy
  integer :: i,j,nx,ny
	
  dly = y_length / ny
  udy = 1. / (2 * dly)

  	do CONCURRENT (j=2:ny-1, i=1:nx)
    	dfi(i,j) = udy * (phi(i,j+1) - phi(i,j-1))
	enddo

	do CONCURRENT(i=1:nx)
		dfi(i,1) = udy * (phi(i,2) - phi(i,ny))
		dfi(i,ny) = udy * (phi(i,1) - phi(i,ny-1))
	enddo
	
  return
end subroutine deriy

subroutine derxx(phi,nx,ny,dfi,x_length)
!
!Second derivative in x direction
!

  implicit none

  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dlx,x_length,udx
  integer :: i,j,nx,ny

  dlx = x_length / nx
  udx = 1. / (dlx*dlx)

  do CONCURRENT(j=1:ny)
     dfi(1,j) = udx * (phi(2,j) - (phi(1,j)+phi(1,j)) + phi(nx,j))
     do CONCURRENT(i=2:nx-1)
        dfi(i,j) = udx * (phi(i+1,j) - (phi(i,j)+phi(i,j)) + phi(i-1,j))
     enddo
     dfi(nx,j) = udx * (phi(1,j) - (phi(nx,j)+phi(nx,j)) + phi(nx-1,j))
  enddo

  return
end subroutine derxx

subroutine deryy(phi,nx,ny,dfi,y_length)
!
!Second derivative in the y direction
!

  implicit none

  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dly,y_length,udy
  integer :: i,j,nx,ny

  dly = y_length / ny
  udy = 1. / (dly*dly)

	do CONCURRENT(j=2:ny-1, i=1:nx)
		dfi(i,j) = udy * (phi(i,j+1) - (phi(i,j)+phi(i,j)) + phi(i,j-1))
	enddo

	do CONCURRENT(i=1:nx)
		dfi(i,1) = udy * (phi(i,2) - (phi(i,1)+phi(i,1)) + phi(i,ny))
		dfi(i,ny) = udy * (phi(i,1) - (phi(i,ny)+phi(i,ny)) + phi(i,ny-1))
	enddo
	
  return
end subroutine deryy

!############################################
!
subroutine derix4(phi,nx,ny,dfi,x_length)
!
!Fourth-order first derivative in the x direction
!############################################
  
  implicit none

  real(8),dimension(nx,ny) :: phi,dfi
  real(8) :: dlx,x_length,udx
  integer :: i,j,nx,ny
	

	
  return
end subroutine derix4
!############################################

!############################################
!
subroutine deriy4(phi,nx,ny,dfi,y_length)
!
!Fourth-order first derivative in the y direction
!############################################

  implicit none
  
  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dly,y_length,udy
  integer :: i,j,nx,ny
	

	
  return
end subroutine deriy4
!############################################

!############################################
!
subroutine derxx4(phi,nx,ny,dfi,x_length)
!
!Fourth-order second derivative in y direction
!############################################

  implicit none

  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dlx,x_length,udx
  integer :: i,j,nx,ny


	
  return
end subroutine derxx4
!############################################

!############################################
!
subroutine deryy4(phi,nx,ny,dfi,y_length)
!
!Fourth-order second derivative in the y direction
!############################################

  implicit none

  real(8),dimension(nx,ny) ::  phi,dfi
  real(8) :: dly,y_length,udy
  integer :: i,j,nx,ny


	
  return
end subroutine deryy4
!############################################


!#######################################################################
!
subroutine fluxx(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,tb1,tb2,tb3,&
     tb4,tb5,tb6,tb7,tb8,tb9,tba,tbb,fro,fru,frv,fre,x_length,y_length,dyn_viscosity,lambda,&
     eps,eta,ftp,scp,xkt)
!
!#######################################################################

  implicit none

  real(8),dimension(nx,ny) :: uuu,vvv,rho,pressure,tmp,rou,rov,roe,tb1,tb2,tb3,tb4,tb5,tb6,tb7
  real(8),dimension(nx,ny) :: tb8,tb9,tba,tbb,fro,fru,frv,fre,eps,ftp,scp
  real(8) :: utt,qtt,dyn_viscosity,eta,dmu,x_length,y_length,lambda,xkt
  integer :: i,j,nx,ny

  call derix(rou,nx,ny,tb1,x_length)
  call deriy(rov,nx,ny,tb2,y_length)
  	do CONCURRENT(j=1:ny, i=1:nx)
        fro(i,j)=-tb1(i,j)-tb2(i,j)

        tb1(i,j)=rou(i,j)*uuu(i,j)
        tb2(i,j)=rou(i,j)*vvv(i,j)
  	enddo
	
  call derix(pressure,nx,ny,tb3,x_length)
  call derix(tb1,nx,ny,tb4,x_length)
  call deriy(tb2,nx,ny,tb5,y_length)
  call derxx(uuu,nx,ny,tb6,x_length)
  call deryy(uuu,nx,ny,tb7,y_length)
  call derix(vvv,nx,ny,tb8,x_length)
  call deriy(tb8,nx,ny,tb9,y_length)

  utt=1./3
  qtt=4./3
  	do CONCURRENT(j=1:ny, i=1:nx)
        tba(i,j) = dyn_viscosity * (qtt * tb6(i,j) + tb7(i,j) + utt * tb9(i,j))
        fru(i,j)= -tb3(i,j) - tb4(i,j) - tb5(i,j) + tba(i,j) - (eps(i,j) / eta) * uuu(i,j)

        tb1(i,j)=rou(i,j)*vvv(i,j)
        tb2(i,j)=rov(i,j)*vvv(i,j)
    enddo
	
  call deriy(pressure,nx,ny,tb3,y_length)
  call derix(tb1,nx,ny,tb4,x_length)
  call deriy(tb2,nx,ny,tb5,y_length)
  call derxx(vvv,nx,ny,tb6,x_length)
  call deryy(vvv,nx,ny,tb7,y_length)
  call derix(uuu,nx,ny,tb8,x_length)
  call deriy(tb8,nx,ny,tb9,y_length)

	do CONCURRENT(j=1:ny, i=1:nx)
		tbb(i,j)=dyn_viscosity*(tb6(i,j)+qtt*tb7(i,j)+utt*tb9(i,j))
		frv(i,j)=-tb3(i,j)-tb4(i,j)-tb5(i,j)+tbb(i,j)&
			-(eps(i,j)/eta)*vvv(i,j)
	enddo
!
!Equation for the tempature
!
  call derix(scp,nx,ny,tb1,x_length)
  call deriy(scp,nx,ny,tb2,y_length)
  call derxx(scp,nx,ny,tb3,x_length)
  call deryy(scp,nx,ny,tb4,y_length)

  	do CONCURRENT(j=1:ny, i=1:nx)
        ftp(i,j)=-uuu(i,j)*tb1(i,j)-vvv(i,j)*tb2(i,j)&
             + xkt*(tb3(i,j)+tb4(i,j))&
             - (eps(i,j)/eta)*scp(i,j)
	enddo
  
  call derix(uuu,nx,ny,tb1,x_length)
  call deriy(vvv,nx,ny,tb2,y_length)
  call deriy(uuu,nx,ny,tb3,y_length)
  call derix(vvv,nx,ny,tb4,x_length)

  	dmu=2./3*dyn_viscosity
  	do CONCURRENT(j=1:ny, i=1:nx)
        fre(i,j)=dyn_viscosity*(uuu(i,j)*tba(i,j)+vvv(i,j)*tbb(i,j))&
             +(dyn_viscosity+dyn_viscosity)*(tb1(i,j)*tb1(i,j)+tb2(i,j)*tb2(i,j))&
             -dmu*(tb1(i,j)+tb2(i,j))*(tb1(i,j)+tb2(i,j))&
             +dyn_viscosity*(tb3(i,j)+tb4(i,j))*(tb3(i,j)+tb4(i,j))

        tb1(i,j)=roe(i,j)*uuu(i,j)
        tb2(i,j)=pressure(i,j)*uuu(i,j) 
        tb3(i,j)=roe(i,j)*vvv(i,j)
        tb4(i,j)=pressure(i,j)*vvv(i,j)
  	enddo

  call derix(tb1,nx,ny,tb5,x_length)
  call derix(tb2,nx,ny,tb6,x_length)
  call deriy(tb3,nx,ny,tb7,y_length)
  call deriy(tb4,nx,ny,tb8,y_length)
  call derxx(tmp,nx,ny,tb9,x_length)
  call deryy(tmp,nx,ny,tba,y_length)
   
  	do CONCURRENT(j=1:ny, i=1:nx)
        fre(i,j)=fre(i,j)-tb5(i,j)-tb6(i,j)-tb7(i,j)-tb8(i,j)+lambda*(tb9(i,j)+tba(i,j))
  	enddo
	
  return
end subroutine fluxx
!#######################################################################

!###########################################################
!
subroutine rkutta(rho,rou,rov,roe,fro,gro,fru,gru,frv,grv,&
     fre,gre,ftp,gtp,nx,ny,ns,dlt,coef,scp,k)
!
!###########################################################

  implicit none
!
  real(8),dimension(nx,ny) :: rho,rou,rov,roe,fro,gro,fru,gru,frv
  real(8),dimension(nx,ny) :: grv,fre,gre,scp,ftp,gtp
  real(8),dimension(2,ns) :: coef
  real(8) :: dlt
  integer :: i,j,nx,ny,ns,k 
!	
!coefficient for RK sub-time steps
!!        coef(1,1)=XXX
!!        coef(1,2)=XXX
!!        coef(1,3)=XXX
!!        coef(2,1)=XXX
!!        coef(2,2)=XXX
!!        coef(2,3)=XXX

  do j=1,ny
     do i=1,nx
!!
     enddo
  enddo

  return
end subroutine rkutta
!###########################################################

!###########################################################
!
subroutine adams(rho,rou,rov,roe,fro,gro,fru,gru,frv,grv,&
     fre,gre,ftp,gtp,scp,nx,ny,dlt)
!
!###########################################################

  implicit none

  real(8),dimension(nx,ny) :: rho,rou,rov,roe,fro,gro,fru,gru,frv
  real(8),dimension(nx,ny) :: grv,fre,gre,ftp,gtp,scp
  real(8) :: dlt,ct1,ct2
  integer :: nx,ny,i,j
          
	ct1=1.5*dlt
	ct2=0.5*dlt
  	do CONCURRENT(j=1:ny, i=1:nx)
        rho(i,j)=rho(i,j)+ct1*fro(i,j)-ct2*gro(i,j)
        gro(i,j)=fro(i,j)
        rou(i,j)=rou(i,j)+ct1*fru(i,j)-ct2*gru(i,j)
        gru(i,j)=fru(i,j)
        rov(i,j)=rov(i,j)+ct1*frv(i,j)-ct2*grv(i,j)
        grv(i,j)=frv(i,j)
        roe(i,j)=roe(i,j)+ct1*fre(i,j)-ct2*gre(i,j)
        gre(i,j)=fre(i,j)
        scp(i,j)=scp(i,j)+ct1*ftp(i,j)-ct2*gtp(i,j)
        gtp(i,j)=ftp(i,j)
  	enddo

  return
end subroutine adams
!###########################################################

!###########################################################
!
subroutine initl(uuu,vvv,rho,eee,pressure,tmp,rou,rov,roe,nx,ny,&
     x_length,y_length,dyn_viscosity,lambda,gamma,chp,dlx,eta,eps,scp,xkt)
!
!###########################################################

  implicit none

  real(8),dimension(nx,ny) :: uuu,vvv,rho,eee,pressure,tmp,rou,rov,roe,eps,scp
  real(8) :: x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0
  real(8) :: pi,dlx,dly,eta,radius
  real(8) :: xkt
  integer :: nx,ny,i,j,ic,jc,imin,imax,jmin,jmax

  call param(x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0)

  dlx = x_length / nx
  dly = y_length / ny
  eta = 0.1
  eta = eta / 2.
  radius = cylinder_d / 2.
  xkt = lambda / (chp * rho_inf)
  pi = acos(-1.)

!######for the square cylinder########################################
  ic = nint((x_length / 2. / dlx) + 1) !X coordinate center of square
  jc = nint((y_length/ 2. / dly) + 1) !Y coordinate center of square
!   imax=XXX
!   imin=XXX
!   jmax=XXX
!   jmin=XXX
!######################################################################

!##########CYLINDER DEFINITION#########################################
  	do CONCURRENT(j=1:ny, i=1:nx)
        if (((i*dlx - x_length/2.)**2 + (j*dly - y_length/2.)**2) .lt. radius**2) then
           eps(i,j)=1.
        else
           eps(i,j)=0.
        end if
  	enddo
!######################################################################

  do j=1,ny
     do i=1,nx
        uuu(i,j) = uu0
        ! Add small velocity perturbation
        vvv(i,j) = 0.01 * (sin(4. * pi * i * dlx / x_length) &
                   + sin(7. * pi * i * dlx / x_length)) &
                   * exp(-(j * dly - y_length / 2.) **2)
        tmp(i,j) = temp_inf
        eee(i,j) = chv * tmp(i,j) + 0.5 * (uuu(i,j)*uuu(i,j) + vvv(i,j)*vvv(i,j))
        rho(i,j) = rho_inf
        pressure(i,j) = rho(i,j) * (gamma-1.) / gamma * chp * tmp(i,j)
        rou(i,j) = rho(i,j) * uuu(i,j)
        rov(i,j) = rho(i,j) * vvv(i,j)
        roe(i,j) = rho(i,j) * eee(i,j)
        scp(i,j) = 1.
     enddo
  enddo

  return
end subroutine initl

subroutine param(x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0)

  implicit none

  real(8) :: reynolds,prandtl,rho_inf,c_inf,cylinder_d,chp,gamma,chv,x_length,y_length,uu0,dyn_viscosity,lambda,temp_inf,mach

  reynolds = 200.
  mach = 0.2
  prandtl = 0.7
  rho_inf = 1.
  c_inf = 1. ! speed of sound at inf
  cylinder_d = 1.
  chp = 1. ! heat capacity at const pressure
  gamma = 1.4
	
  chv = chp / gamma ! heat capacity at const volume
  ! Dimensions of computational domain
  x_length = 4. * cylinder_d
  y_length = 4. * cylinder_d
  uu0 = mach * c_inf ! U infinity
  dyn_viscosity = rho_inf * uu0 * cylinder_d / reynolds
  lambda = dyn_viscosity * chp / prandtl ! thermal conductivity
  temp_inf = c_inf**2 / (chp* (gamma-1))
	
  return
end subroutine param

subroutine etatt(uuu,vvv,rho,pressure,tmp,rou,rov,roe,nx,ny,gamma,chp)

  implicit none

  real(8),dimension(nx,ny) ::  uuu,vvv,rho,pressure,tmp,rou,rov,roe
  real(8) :: ct7,gamma,ct8,chp
  integer :: i,j,nx,ny
	
	ct7=gamma-1.
	ct8=gamma/(gamma-1.)

  	do CONCURRENT(j=1:ny, i=1:nx)
        uuu(i,j)=rou(i,j)/rho(i,j)
        vvv(i,j)=rov(i,j)/rho(i,j)
        pressure(i,j)=ct7*(roe(i,j)-0.5*(rou(i,j)*uuu(i,j)+&
             rov(i,j)*vvv(i,j)))
        tmp(i,j)=ct8*pressure(i,j)/(rho(i,j)*chp)
  	enddo
	
  return
end subroutine etatt
