!==========================================================
!  2D Navier-Stokes equation solver by Sylvain Laizet, 2014
!==========================================================

!START OF THE MAIN PROGRAM
!
program navierstokes
!
  implicit none   !-->all the variables MUST be declared

  integer,parameter :: nx=129,ny=nx,nt=100,ns=3,nf=3,mx=nf*nx,my=nf*ny
  !size of the computational domain (nx x ny) 
  !size of the exchanger (mx x my)
  !number of time step for the simulation

  !Declaration of variables
  real(8), allocatable, dimension(:, :, :) :: prev_rho_dots, rho_dots, rho_vals, dummy
  real(8), allocatable, dimension(:, :) :: uuu,vvv,eee,pressure,tmp,wz,tuu,tvv,eps, tf, coef
  real(8), allocatable, dimension(:) :: xx, yy
  integer :: i,j,itemp,n,ni,nj,imodulo
  real(8) :: x_length,y_length,CFL,dlx,dx,dyn_viscosity,xkt
  real(8) :: lambda,gamma,chp,eta,dlt,u_mean,v_mean,t_mean,x,y,dy
  character(len=20) nfichier

  ! See compressible equations for why these are important
  ! rho_vals(:, :, 1) = rho
  ! rho_vals(:, :, 2) = rho * u
  ! rho_vals(:, :, 3) = rho * v
  ! rho_vals(:, :, 4) = rho * e
  ! rho_vals(:, :, 5) = scp? Don't know what this represents

  !Allocate array memory
  allocate(prev_rho_dots(nx, ny, 5), rho_dots(nx, ny, 5), rho_vals(nx, ny, 5), dummy(nx, ny, 11))
  allocate(uuu(nx, ny), vvv(nx, ny), eee(nx, ny), pressure(nx, ny), tmp(nx, ny))
  allocate(eps(nx, ny), wz(nx, ny), tuu(nx, ny), tvv(nx, ny))
  allocate(xx(mx), yy(my), tf(mx, my), coef(2, ns))

  !Name of the file for visualisation:
  990 format('vort',I4.4)
  imodulo=2500 !snapshots to be saved every imodulo time steps

  ! AB2 temporal scheme itemp=1
  ! RK3 temporal scheme itemp=2
  itemp=1

  ! Subroutine for the initialisation of the variables 
  call initl(uuu,vvv,eee,pressure,tmp, rho_vals(:, :, 1), rho_vals(:, :, 2), rho_vals(:, :, 3), rho_vals(:, :, 4), &
  rho_vals(:, :, 5), nx,ny, x_length,y_length, dyn_viscosity,lambda,gamma,chp,dlx,eta,eps,xkt)

  !we need to define the time step
  dx = x_length / nx !mesh size in x
  dy = y_length / ny !mesh sixe in y
  CFL = 0.025d0  !CFL number for time step
  dlt = CFL * dlx
  print *,'The time step of the simulation is', dlt
  
  !$acc data copyin(uuu, vvv, pressure, tmp, eps, rho_vals) create(rho_dots, prev_rho_dots, dummy)

  !Computation of the average velocity and temperature at t=0
  call average(uuu, u_mean, nx, ny)
  call average(vvv, v_mean, nx, ny)
  call average(rho_vals(:, :, 5), t_mean, nx, ny)
  write(*,*) 'Average values at t=0', u_mean, v_mean, t_mean

!BEGINNING OF TIME LOOP
  do n=1,nt
   if (itemp.eq.1) then   !TEMPORAL SCHEME AB2
      
      call fluxx(uuu,vvv,pressure,tmp,rho_vals,nx,ny,rho_dots,x_length,y_length,dyn_viscosity,lambda,eps,eta,xkt, dummy)

      call adams(rho_vals, rho_dots, prev_rho_dots, nx, ny, dlt)
      
      call etatt(uuu,vvv,pressure,tmp,rho_vals,nx,ny,gamma,chp)
        
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
   call average(rho_vals(:, :, 5), t_mean,nx,ny)
   write(*,*) n, u_mean, v_mean, t_mean

  enddo

  !$acc end data
  !END OF THE TIME LOOP

  deallocate(uuu,vvv,eee,pressure,tmp,wz,tuu,tvv)
  deallocate(dummy,eps)
  deallocate(rho_vals, rho_dots, prev_rho_dots)
  deallocate(xx, yy, tf, coef)

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
  integer :: nx, ny, i, j

  mean = 0.d0

  !$acc data present(array)
  !$acc parallel loop reduction(+:mean)

  do j = 1, ny
    do i = 1, nx
      mean = mean + array(i, j)
    enddo
  enddo

  !$acc end parallel loop
  !$acc end data

  mean = mean / (nx * ny)
  
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
  udx = 1. / (2. * dlx)

  !$acc data present(phi, phi_deriv)
  !$acc parallel loop
  do j=1, ny
    phi_deriv(1,j) = udx * (phi(2,j) - phi(nx,j))
    phi_deriv(nx,j) = udx * (phi(1,j) - phi(nx-1,j))

    do i=2, nx-1
      phi_deriv(i,j) = udx * (phi(i+1,j) - phi(i-1,j))
    enddo
  enddo
  !$acc end parallel loop
  !$acc end data

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
  udy = 1. / (2. * dly)

  !$acc data present(phi, dfi)
  !$acc parallel loop
  do i=1, nx
    dfi(i,1) = udy * (phi(i,2) - phi(i,ny))
    dfi(i,ny) = udy * (phi(i,1) - phi(i,ny-1))
  enddo
  !$acc end parallel loop

  !$acc parallel loop
  do j=2, ny-1
    do i=1, nx
      dfi(i,j) = udy * (phi(i,j+1) - phi(i,j-1))
    enddo
  enddo
  !$acc end parallel loop
  !$acc end data

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
  !$acc data present(phi, dfi)
  !$acc parallel loop
  do j=1, ny
    dfi(1,j) = udx * (phi(2,j) - 2 * phi(1,j) + phi(nx,j))
    dfi(nx,j) = udx * (phi(1,j) - 2 * phi(nx,j) + phi(nx-1,j))

    do i=2, nx-1
      dfi(i,j) = udx * (phi(i+1,j) - 2 * phi(i,j) + phi(i-1,j))
    enddo
  enddo
  !$acc end parallel loop
  !$acc end data

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

  !$acc data present(phi, dfi)
  !$acc parallel loop
  do j=2, ny-1
    do i=1, nx
      dfi(i,j) = udy * (phi(i,j+1) - 2 * phi(i,j) + phi(i,j-1))
    enddo
  enddo
  !$acc end parallel loop

  !$acc parallel loop
  do i=1, nx
    dfi(i,1) = udy * (phi(i,2) - 2 * phi(i,1) + phi(i,ny))
    dfi(i,ny) = udy * (phi(i,1) - 2 * phi(i,ny) + phi(i,ny-1))
  enddo
  !$acc end parallel loop
  !$acc end data

  return
end subroutine deryy

!#######################################################################
!
subroutine fluxx(uuu,vvv,pressure,tmp,rho_vals,nx,ny,rho_dots,x_length,y_length,dyn_viscosity,lambda,eps, &
  eta,xkt, dummy)
!
!#######################################################################

  implicit none

  real(8),dimension(nx,ny) :: uuu,vvv,pressure,tmp,eps
  real(8), dimension(nx, ny, 5) :: rho_dots, rho_vals
  real(8), dimension(nx, ny, 11) :: dummy
  real(8) :: utt,qtt,dyn_viscosity,eta,dmu,x_length,y_length,lambda,xkt
  integer :: i,j,nx,ny
  !$acc data present(uuu, vvv, pressure, tmp, dummy, eps, rho_vals, rho_dots)

  call derix(rho_vals(:, :, 2),nx,ny,dummy(:, :, 1),x_length)
  call deriy(rho_vals(:, :, 3),nx,ny,dummy(:, :, 2),y_length)

  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      rho_dots(i,j, 1) = -dummy(i,j, 1) - dummy(i,j, 2)

      dummy(i,j, 1) = rho_vals(i, j, 2) * uuu(i,j)
      dummy(i,j, 2) = rho_vals(i, j, 2) * vvv(i,j)
    enddo
  enddo
  !$acc end parallel loop
  
  call derix(pressure,nx,ny,dummy(:, :, 3),x_length)
  call derix(dummy(:, :, 1),nx,ny,dummy(:, :, 4),x_length)
  call deriy(dummy(:, :, 2),nx,ny,dummy(:, :, 5),y_length)
  call derxx(uuu,nx,ny,dummy(:, :, 6),x_length)
  call deryy(uuu,nx,ny,dummy(:, :, 7),y_length)
  call derix(vvv,nx,ny,dummy(:, :, 8),x_length)
  call deriy(dummy(:, :, 8),nx,ny,dummy(:, :, 9),y_length)

  utt = 1.d0 / 3
  qtt = 4.d0 / 3
  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      dummy(i,j, 10) = dyn_viscosity * (qtt * dummy(i, j, 6) + dummy(i, j, 7) + utt * dummy(i, j, 9))
      rho_dots(i, j, 2) = -dummy(i, j, 3) - dummy(i, j, 4) - dummy(i, j, 5) + dummy(i, j, 10) - (eps(i,j) / eta) * uuu(i,j)

      dummy(i,j, 1) = rho_vals(i, j, 2) * vvv(i,j)
      dummy(i,j, 2) = rho_vals(i, j, 3) * vvv(i,j)
    enddo
  enddo
  !$acc end parallel loop

  call deriy(pressure,nx,ny,dummy(:, :, 3),y_length)
  call derix(dummy(:, :, 1),nx,ny,dummy(:, :, 4),x_length)
  call deriy(dummy(:, :, 2),nx,ny,dummy(:, :, 5),y_length)
  call derxx(vvv,nx,ny,dummy(:, :, 6),x_length)
  call deryy(vvv,nx,ny,dummy(:, :, 7),y_length)
  call derix(uuu,nx,ny,dummy(:, :, 8),x_length)
  call deriy(dummy(:, :, 8),nx,ny,dummy(:, :, 9),y_length)

  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      dummy(i, j, 11) = dyn_viscosity * (dummy(i, j, 6) + qtt * dummy(i, j, 7) + utt * dummy(i, j, 9))
      rho_dots(i, j, 3) = - dummy(i, j, 3) - dummy(i, j, 4) - dummy(i, j, 5) + dummy(i, j, 11)&
        - (eps(i,j) / eta) * vvv(i,j)
    enddo
  enddo
  !$acc end parallel loop

!
!Equation for the tempature
!
  call derix(rho_vals(:, :, 5),nx,ny,dummy(:, :, 1),x_length)
  call deriy(rho_vals(:, :, 5),nx,ny,dummy(:, :, 2),y_length)
  call derxx(rho_vals(:, :, 5),nx,ny,dummy(:, :, 3),x_length)
  call deryy(rho_vals(:, :, 5),nx,ny,dummy(:, :, 4),y_length)

  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      rho_dots(i, j, 5)=-uuu(i,j)*dummy(i, j, 1)-vvv(i,j)*dummy(i, j, 2)&
            + xkt*(dummy(i, j, 3)+dummy(i, j, 4))&
            - (eps(i,j)/eta)*rho_vals(i, j, 5)
    enddo
  enddo
  !$acc end parallel loop

  call derix(uuu,nx,ny,dummy(:, :, 1),x_length)
  call deriy(vvv,nx,ny,dummy(:, :, 2),y_length)
  call deriy(uuu,nx,ny,dummy(:, :, 3),y_length)
  call derix(vvv,nx,ny,dummy(:, :, 4),x_length)

  dmu = 2.d0 / 3 * dyn_viscosity

  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      rho_dots(i, j, 4)=dyn_viscosity*(uuu(i,j)*dummy(i, j, 10)+vvv(i,j)*dummy(i, j, 11))&
            +(dyn_viscosity+dyn_viscosity)*(dummy(i,j,1)*dummy(i,j,1)+dummy(i,j,2)*dummy(i,j,2))&
            -dmu*(dummy(i,j,1)+dummy(i,j,2))*(dummy(i,j,1)+dummy(i,j,2))&
            +dyn_viscosity*(dummy(i,j,3)+dummy(i,j,4))*(dummy(i,j,3)+dummy(i,j,4))

      dummy(i,j,1) = rho_vals(i, j, 4) * uuu(i,j)
      dummy(i,j,2) = pressure(i,j) * uuu(i,j) 
      dummy(i,j,3) = rho_vals(i, j, 4) * vvv(i,j)
      dummy(i,j,4) = pressure(i,j) * vvv(i,j)
    enddo
  enddo
  !$acc end parallel loop

  call derix(dummy(:, :, 1),nx,ny,dummy(:, :, 5),x_length)
  call derix(dummy(:, :, 2),nx,ny,dummy(:, :, 6),x_length)
  call deriy(dummy(:, :, 3),nx,ny,dummy(:, :, 7),y_length)
  call deriy(dummy(:, :, 4),nx,ny,dummy(:, :, 8),y_length)
  call derxx(tmp,nx,ny,dummy(:, :, 9),x_length)
  call deryy(tmp,nx,ny,dummy(:, :, 10),y_length)
  
  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      rho_dots(i,j,4) = rho_dots(i,j,4) - dummy(i,j,5) - dummy(i,j,6) - dummy(i,j,7) - dummy(i,j,8) &
      + lambda * (dummy(i,j,9) + dummy(i,j,10))
    enddo
  enddo
  !$acc end parallel loop
  
  !$acc end data
  return
end subroutine fluxx
!###########################################################

!###########################################################
!
subroutine adams(integrals, derivs, prev_derivs, nx, ny, dlt)
!
!###########################################################

  implicit none

  real(8), dimension(nx, ny, 5) :: integrals, derivs, prev_derivs
  real(8) :: dlt,ct1,ct2
  integer :: nx,ny
          
  ct1 = 1.5 * dlt
  ct2 = 0.5 * dlt

  !$acc data present(integrals, derivs, prev_derivs)

  !$acc kernels
  integrals = integrals + ct1 * derivs - ct2 * prev_derivs
  prev_derivs = derivs
  !$acc end kernels
  !$acc end data

  return
end subroutine adams
!###########################################################

!###########################################################
!
subroutine initl(uuu,vvv,eee,pressure,tmp,rho,rou,rov,roe,scp,nx,ny,&
     x_length,y_length,dyn_viscosity,lambda,gamma,chp,dlx,eta,eps,xkt)
!
!###########################################################

  implicit none

  real(8),dimension(nx,ny) :: uuu,vvv,rho,eee,pressure,tmp,rou,rov,roe,eps,scp
  real(8) :: x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0
  real(8) :: pi,dlx,dly,eta,radius
  real(8) :: xkt
  integer :: nx,ny,i,j

  call param(x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0)

  dlx = x_length / nx
  dly = y_length / ny
  eta = 0.1d0
  eta = eta / 2.
  radius = cylinder_d / 2.
  xkt = lambda / (chp * rho_inf)
  pi = acos(-1.d0)

!##########CYLINDER DEFINITION#########################################
  do j=1, ny
    do i=1, nx
      if (((i*dlx - x_length/2.)**2. + (j*dly - y_length/2.)**2.) .lt. radius**2.) then
          eps(i,j)=1.d0
      else
          eps(i,j)=0.d0
      end if
    enddo
  enddo
!######################################################################

  do j=1,ny
     do i=1,nx
        uuu(i,j) = uu0
        ! Add small velocity perturbation
        vvv(i,j) = 0.01d0 * (sin(4. * pi * i * dlx / x_length) &
                   + sin(7. * pi * i * dlx / x_length)) &
                   * exp(-(j * dly - y_length / 2.) **2.)
        tmp(i,j) = temp_inf
        eee(i,j) = chv * tmp(i,j) + 0.5 * (uuu(i,j)*uuu(i,j) + vvv(i,j)*vvv(i,j))
        rho(i,j) = rho_inf
        pressure(i,j) = rho(i,j) * (gamma-1.) / gamma * chp * tmp(i,j)
        rou(i,j) = rho(i,j) * uuu(i,j)
        rov(i,j) = rho(i,j) * vvv(i,j)
        roe(i,j) = rho(i,j) * eee(i,j)
        scp(i,j) = 1.d0
     enddo
  enddo

  return
end subroutine initl

subroutine param(x_length,y_length,dyn_viscosity,lambda,gamma,chp,rho_inf,cylinder_d,temp_inf,chv,uu0)

  implicit none

  real(8) :: reynolds,prandtl,rho_inf,c_inf,cylinder_d,chp,gamma,chv,x_length,y_length,uu0,dyn_viscosity,lambda,temp_inf,mach
  
  reynolds = 200.d0
  mach = 0.2d0
  prandtl = 0.7d0
  rho_inf = 1.d0
  c_inf = 1.d0 ! speed of sound at inf
  cylinder_d = 1.d0
  chp = 1.d0 ! heat capacity at const pressure
  gamma = 1.4d0
  
  chv = chp / gamma ! heat capacity at const volume
  ! Dimensions of computational domain
  x_length = 4. * cylinder_d
  y_length = 4. * cylinder_d
  uu0 = mach * c_inf ! U infinity
  dyn_viscosity = rho_inf * uu0 * cylinder_d / reynolds
  lambda = dyn_viscosity * chp / prandtl ! thermal conductivity
  temp_inf = c_inf**2. / (chp* (gamma-1.))
  
  return
end subroutine param

subroutine etatt(uuu,vvv,pressure,tmp,rho_vals,nx,ny,gamma,chp)

  implicit none

  real(8),dimension(nx,ny) ::  uuu,vvv,pressure,tmp
  real(8), dimension(nx, ny, 5) :: rho_vals
  real(8) :: ct7,gamma,ct8,chp
  integer :: i,j,nx,ny
  
  ct7=gamma-1.
  ct8=gamma/(gamma-1.)

  !$acc data present(uuu, vvv, pressure, tmp, rho_vals)
  !$acc parallel loop
  do j=1, ny
    do i=1, nx
      uuu(i,j) = rho_vals(i, j, 2) / rho_vals(i, j, 1)
      vvv(i,j) = rho_vals(i, j, 3) / rho_vals(i, j, 1)
      pressure(i,j) = ct7 * (rho_vals(i, j, 4) - 0.5 * (rho_vals(i, j, 2) * uuu(i,j) + rho_vals(i, j, 3) * vvv(i,j)))
      tmp(i,j) = ct8 * pressure(i,j) / (rho_vals(i, j, 1) * chp)
    enddo
  enddo
  !$acc end parallel loop
  !$acc end data

  return
end subroutine etatt
