c  (c) Applied Fluids Engineering, Inc. (2000-2002)
c  Written by Dr. Philip Watts in December, 2000.
c  Modified by Dr. Philip Watts in November, 2001.
c  Modified by Dr. Philip Watts in November, 2002.
c  Version 1.2 of the software package TOPICS:
c  Tsunami Open & Progressive Initial Conditions System
c
c
c  VARIABLE INDEX
c  --------------
c
c  alpha    orientation of mass failure axis from north
c  alfao    orientation of north from the top of grid 
c  ao       initial acceleration
c  area     elemental area of earthquake fault
c  b        length
c  bnd12    boundary element length along x-axis from P1 to P2
c  bnd23    boundary element length in yz-plane frpm P2 to P3
c  bnd34    boundary element length along x-axis from P3 to P4
c  bnd41    boundary element length in yz-plane frpm P4 to P1
c  cdip     cosine dip
c  cna      cosine alpha
c  cnt      cosine theta
c  cut      cutoff width of landslide tsunamis
c  d        depth
c  ddx      step size for peak amplitude algorithm
c  delx     peak to trough separation
c  denom    adjusted wavelength
c  dep      meters per unit depth
c  depth    depth near the center of the fault plane
c  dip      fault plane angle from horizontal
c  dist     distance of motion
c  dmin     minimum depth of bathymetry file
c  dmax     maximum depth of bathymetry file
c  dt       simulation time step, sqrt(2gd)
c  dvar     positive depth of earthquake element
c  dx       x direction grid interval 
c  dy       y direction grid interval
c  elast    material property mu / (mu + lambda)
c  eta      characteristic wave amplitude
c  etal     amplitude based on runout length
c  etat     amplitude based on runout time
c  fact1    factor that reduces slip radially (0-1)
c  fact2    factor that reduces slip near z=0 (0-1)
c  fail     index for chosen tsunami source
c  flag     index for bathymetry file coordinate units
c  fr       Froude number of landslide impact
c  fsmin    free surface minimum elevation
c  fsmax    free surface maximum elevation
c  g        acceleration of gravity
c  gamma    specific density
c  gmo      gamma - 1
c  grd      bathymetry grid file
c  grid     index for format of bathymetry grid file
c  hao      Hammack number at t=to
c  high     typical width or height of fault plane
c  i        grid index in x direction (1,im)
c  idep     index for sign of water depths
c  im       number of columns in grid files
c  input    name of the bathymetry grid file
c  itmp     temporary index
c  itrain   index for edge of wave incidence
c  itsu     index for different uses of topics
c  i4var    function in coseismic displacement
c  i4var    function in coseismic displacement
c  j        grid index in y direction (1,jm)
c  jm       number of rows in grid files
c  k        fault index in x direction (1,nx)
c  kappa    design wave wavenumber
c  l        fault index in y direction (1,ny)
c  lambda   wavelength
c  leng     typical fault plane length
c  linear   measure of design wave linearity
c  lwave    index for leading wave polarity
c  momt     earthquake fault moment
c  mu       shear modulus at earthquake depth
c  mw       earthquake magnitude
c  nmin     adjusted trough amplitude
c  nmax     adjusted peak amplitude
c  nsrc     number assigned as the tsunami source ID
c  nwave    number of design wave wavelengths
c  nx       number of x-direction fault subdivisions
c  ny       number of y-direction fault subdivisions
c  out      free surface grid file
c  output   text extension of output grid files
c  omega    design wave circular frequency
c  pi       irrational number 3.1415...
c  proper   text of tsunami source number
c  pvar     parameter in coseismic displacement
c  qs       nondimensional volume flux
c  quest    index for answering questions
c  qvar     parameter in coseismic displacement
c  radd     radial distance from earthquake element plus depth
c  rado     radial distance from earthquake element
c  rake     angle of slip in fault plane from horizontal
c  rhob     bulk density
c  rhoo     density of water
c  sdip     sine dip
c  sg       Submergence number
c  sharp    radius for slip to drop 50% of maximum
c  shift    adjusted peak to trough separation
c  sim      index of the wave propagation code being run
c  sing     local depth where slip drops to 1% of maximum
c  shall    measure of design wave wavelength
c  slip     maximum value of earthquake fault slip
c  sna      sine alpha
c  snt      sine theta
c  so       characteristic distance
c  steep    measure of design wave steepness
c  t        thickness
c  term     mathematical term
c  test     various branching tests
c  theta    incline angle
c  tm       nondimensional time of motion
c  tnt      tangent theta
c  to       characteristic time
c  trap     index for chosen earthquake description
c  tsrc     absolute time of the tsunami source
c  type     index for format of initial conditions output file
c  u1var    slip component parallel to x-axis
c  u2var    slip component perpendicular to x-axis
c  umax     maximum velocity parallel to x-axis
c  umin     minimum velocity parallel to x-axis
c  ut       characteristic velocity
c  utot     total velocity
c  uvel     u velocity grid file
c  vmax     maximum velocity parallel to y-axis
c  vmin     minimum velocity parallel to y-axis
c  vol      subaerial landslide volume
c  vvel     v velocity grid file
c  w        landslide width
c  wid      adjusted wavelength
c  xo       source x location in absolute coordinates
c  yo       source y location in absolute coordinates
c  xg       wave gauge location
c  xmin     location of wave trough
c  x        rotated grid coordinate
c  x1       x position of trapezoidal fault vertex P1
c  x12      interpolated x position of earthquake element P1 to P2
c  x2       x position of trapezoidal fault vertex P2
c  x3       x position of trapezoidal fault vertex P3
c  x34      interpolated x position of earthquake element P3 to P4
c  x4       x position of trapezoidal fault vertex P4
c  xe       east boundary of grid files
c  xinc     grid increment in x direction
c  xlon     meters per unit longitude
c  xp       local position of earthquake element
c  xvar     relative position of earthquake element
c  xw       west boundary of grid files
c  xx       grid coordinate converted to meters
c  xxx      translated grid coordinate
c  y        rotated grid coordinate
c  y1       y position of trapezoidal fault vertex P1
c  y12      interpolated y position of earthquake element P1 to P2
c  y2       y position of trapezoidal fault vertex P2
c  y3       y position of trapezoidal fault vertex P3
c  y34      interpolated y position of earthquake element P3 to P4
c  y4       y position of trapezoidal fault vertex P4
c  yinc     grid increment in y direction
c  ylat     meters per unit latitude
c  yn       north boundary of grid files
c  yp       local position of earthquake element
c  ys       south boundary of grid files
c  yvar     relative position of earthquake element
c  yy       grid coordinate converted to meters
c  yyy      translated grid coordinate
c  z1       z position of trapezoidal fault vertex P1
c  z12      interpolated z position of earthquake element P1 to P2
c  z2       z position of trapezoidal fault vertex P2
c  z3       z position of trapezoidal fault vertex P3
c  z34      interpolated z position of earthquake element P3 to P4
c  z4       z position of trapezoidal fault vertex P4
c  zmin     characteristic trough amplitude
c  zmax     characteristic peak amplitude
c  zp       local position of earthquake element
c
c
c
c  PROGRAM STRUCTURE
c  -----------------
c
c                        1. Topics
c
c                             |
c        _____________________|_______________
c        |                                    |
c        |                                    |
c
c   3. Choice                            3. Choice
c   2. Gridfile
c   8. Gridinit                               |
c                      _______________________|____________________________________
c        |             |              |              |              |              |
c        |             |              |              |              |              |
c        |
c        |       11. Seismic     5. Slide       6. Slump      15. Splash     24. Pyro 
c        |       14. Charquake  10. Charfail   10. Charfail   18. Charcoast  26. Charflow
c        |
c        |
c        |________________________________________________________________________________________
c                      |              |              |              |              |              |
c                      |              |              |              |              |              |
c 
c                12. Setquake    4. Setfail     4. Setfail    16. Setcoast   23. Setflow    20. Setwave
c                11. Seismic     5. Slide       6. Slump      15. Splash     24. Pyro       21. Wave
c                 7. Gridcheck   7. Gridcheck   7. Gridcheck   7. Gridcheck   7. Gridcheck   7. Gridcheck
c                13. Gridquake   9. Gridfail    9. Gridfail   17. Gridcoast  25. Gridflow   22. Gridwave
c                19. Gridwrite  19. Gridwrite  19. Gridwrite  19. Gridwrite  19. Gridwrite  19. Gridwrite
c
c
c  --------------
c
      program topics
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  1.1 Program header.
c
      write(*,*)
      write(*,*)
      write(*,*) 'Tsunami Open & Progressive Initial Conditions System'
      write(*,*)
      write(*,*) '           Welcome to TOPICS, Version 1.2'
      write(*,*)
      write(*,*) '(c) 2000-2002, Applied Fluids Engineering, Inc.'
      write(*,*)
      write(*,*)
      write(*,*)
      write(*,*)
      write(*,*)
      write(*,*) '           Program errors are indicated by:'
      write(*,*) '           xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      write(*,*)
      write(*,*) '           Program warnings are indicated by:'
      write(*,*) '           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(*,*)
      write(*,*)
      write(*,*) 'TOPICS gives sufficient latitude to make mistakes'
      write(*,*)
      write(*,*) 'TOPICS uses SI units (m, s, kg) exclusively'
      write(*,*)
c
c  1.2 Assume that failures are on earth and water is sea water.
c
      write(*,*) 'Tsunamis are assumed on earth and in sea water'
      write(*,*)
      pi = 4.0D+00 * datan(1.0D+00)
      g = 9.81D+00
      rhoo = 1.025D+03
c
c  1.3 Choose the use for which you would like to run TOPICS.
c
      write(*,*)    
      write(*,*) 'Type 0 for characteristic tsunami amplitudes'
      write(*,*) 'Type 1 for gridded tsunami initial conditions'
      read(*,*) itsu
      write(*,*)
c
c  1.4 Check the entry and quit the program if requested.
c
      if ((itsu.ne.0).and.(itsu.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  1.5 Run the subroutines needed to calculate characteristic amplitudes.   
c
      if (itsu.eq.0) then
        call choice 
        if (fail.eq.0) call seismic
        if (fail.eq.1) call slide
        if (fail.eq.2) call slump
        if (fail.eq.3) call splash
        if (fail.eq.4) call pyro
        if (fail.eq.0) call charquake
        if (fail.eq.1) call charfail
        if (fail.eq.2) call charfail
        if (fail.eq.3) call charcoast
        if (fail.eq.4) call charflow
      endif
c
c  1.6 Run the subroutines needed to calculate initial conditions.   
c
      if (itsu.eq.1) then
        call choice
        call gridfile
        if (fail.eq.0) call setquake
        if (fail.eq.1) call setfail
        if (fail.eq.2) call setfail
        if (fail.eq.3) call setcoast
        if (fail.eq.4) call setflow
        if (fail.eq.5) call setwave
        if (fail.eq.0) call seismic
        if (fail.eq.1) call slide
        if (fail.eq.2) call slump
        if (fail.eq.3) call splash
        if (fail.eq.4) call pyro
        if (fail.eq.5) call wave
        call gridinit
        if (fail.eq.0) call gridquake
        if (fail.eq.1) call gridfail
        if (fail.eq.2) call gridfail
        if (fail.eq.3) call gridcoast
        if (fail.eq.4) call gridflow
        if (fail.eq.5) call gridwave
      endif
c
c  1.7 Close the properties output file.
c
      close (63)
c
      stop
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridfile
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  2.1 You can specify any filename provided the format can be read.  
c 
      write(*,*) 'Enter the bathymetry filename (up to 20 char)'
      read(*,*) input
      write(*,*)
c
c  2.2 Specify the format of the bathymetry file.  
c
      write(*,*) 'Type 0 if this is a Surfer ASCII grid file'
      read(*,*) grid
      write(*,*)
c
c  2.3 Check the last entry.
c
      if (grid.ne.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.4 Specify the format of the initial condition output file.  
c
      write(*,*) 'Type 0 to output a Surfer ASCII grid file'
      read(*,*) type
      write(*,*)
c
c  2.5 Check the last entry and specify file extension.
c
      if (type.ne.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (type.eq.0) then
        output = '.grd'
      endif
c
c  2.6 Specify the wave propagation code being run.  
c
      write(*,*) 'Time steps and grid configurations vary'
      write(*,*) 
      write(*,*) 'Type 0 if you are running TUNAMI-N2'
      write(*,*) 'Type 1 if you are running GEOWAVE'
      read(*,*) sim
      write(*,*)
c
c  2.7 Check the last entry.
c
      if ((sim.ne.0).and.(sim.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.8 Read the grid file in the specified format.  
c
      write(*,*) 'Opening the bathymetry file'
      write(*,*)
      if (grid.eq.0) then
        open (62,file=input)
        read(62,*)
        read(62,*) im,jm
        read(62,*) xw,xe
        read(62,*) ys,yn
        read(62,*) dmin,dmax
        do 24 j=1,jm
         read(62,*)(grd(i,j),i=1,im)
  24    continue
      endif
      close (62)
c
c  2.9 Positive water depths are used within TOPICS.
c
      write(*,*) 'Type 0 if depths are positive'
      write(*,*) 'Type 1 if depths are negative'
      read(*,*) iden
      write(*,*)
c
c  2.10 Check the data for consistency.
c
      if ((iden.ne.0).and.(iden.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (xe.le.xw) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'East and west grid edges overlap'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (yn.le.ys) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'North and south grid edges overlap'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (im.le.3) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Three or fewer columns in the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (jm.le.3) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Three or fewer rows in the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.11 Calculate the spatial increments of the grid.
c
      xinc = (xe - xw) / dfloat(im - 1)
      yinc = (yn - ys) / dfloat(jm - 1)
c
c  2.12 Grid distance must be converted to meters for TOPICS.
c
      write(*,*) 'Type 0 if the grid is in meters'
      write(*,*) 'Type 1 if the grid is in feet'
      write(*,*) 'Type 2 if the grid is in kilometers'
      write(*,*) 'Type 3 if the grid is in miles'
      write(*,*) 'Type 4 if the grid is in decimal degrees'
      read(*,*) flag
      write(*,*)
      if (flag.eq.0) then
        ylat = 1.0D+00
        xlon = 1.0D+00
      endif
      if (flag.eq.1) then
        ylat = 0.3048+00
        xlon = 0.3048D+00
      endif
      if (flag.eq.2) then
        ylat = 1.0D+03
        xlon = 1.0D+03
      endif
      if (flag.eq.3) then
        ylat = 1.609344D+03
        xlon = 1.609344D+03
      endif
      if (flag.eq.4) then
        ylat = 111.265D+03
        xlon = ylat * dcos((yn + ys) * pi / 3.6D+02)
      endif
c
c  2.13 Check the last entry.
c
      if ((flag.lt.0).and.(flag.gt.4)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.14 Calculate the vertical and horizontal grid spacing in meters.
c 
      dx = xinc * xlon
      dy = yinc * ylat
c
c  2.15 Verify that the grid is close to being uniform, dx=dy.
c
      test = 2.0D+02 * (dabs(dx) - dabs(dy)) / (dabs(dx) + dabs(dy))
      if (dabs(test).gt.5.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The bathy grid is more than 5% nonuniform'
        write(*,*) 'This may make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The grid is more than 5% nonuniform'
          write(63,*) 'This may make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif 
c
c  2.16 Get ready to convert the depths into meters.
c
      write(*,*) 'Type 0 if depth is in meters'
      write(*,*) 'Type 1 if depth is in feet'
      write(*,*) 'Type 2 if depth is in kilometers'
      write(*,*) 'Type 3 if depth is in miles'
      write(*,*) 'Type 4 if depth is in fathoms'
      read(*,*) idep
      write(*,*)
      if (idep.eq.0) then
        dep = 1.0D+00
      endif
      if (idep.eq.1) then
        dep = 0.3048+00
      endif
      if (idep.eq.2) then
        dep = 1.0D+03
      endif
      if (idep.eq.3) then
        dep = 1.609344D+03
      endif
      if (idep.eq.4) then
        dep = 1.8288D+00
      endif
c
c  2.17 Check the last entry.
c
      if ((idep.lt.0).and.(idep.gt.4)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  2.18 Convert grid file max and min to meters.  
c 
      dmin = dmin * dep
      dmax = dmax * dep
c
c  2.19 Compute the time step specific to the propogation code.
c
      if ((iden.eq.0).and.(dmax.le.0.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The maximum depth is not positive'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif 
      if ((iden.eq.0).and.(dmax.gt.0.0D+00)) then
        if (sim.eq.0) then
          dt = 0.95D+00 * min(dx,dy) / dsqrt(2.0D+00 * g * dmax)
        endif
        if (sim.eq.1) then
          dt = 0.3D+00 * min(dx,dy) / dsqrt(g * dmax)
        endif
      endif
      if ((iden.eq.1).and.(dmin.ge.0.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The minimum depth is not negative'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((iden.eq.1).and.(dmin.lt.0.0D+00)) then
        if (sim.eq.0) then
          dt = 0.95D+00 * min(dx,dy) / dsqrt(-2.0D+00 * g * dmin)
        endif
        if (sim.eq.1) then
          dt = 0.3D+00 * min(dx,dy) / dsqrt(-g * dmin)
        endif
      endif
c
c  2.20 Prepare to render all depths positive.
c 
      if (iden.eq.0) then
        test = 1.0D+00
      else
        test = -1.0D+00
      endif
c
c  2.21 Rescale the bathymetry data so that it is positive and in meters.
c
      write(*,*) 'Rescaling the bathymetry grid'
      write(*,*)
      do 27 i=1,im
        do 27 j=1,jm
          grd(i,j) = test * dep * grd(i,j)
  27  continue
      if (iden.ne.0) then
        test = dmax
        dmax = -dmin
        dmin = -test
      endif
c
c  2.22 Write data to the properties file.
c
      write(63,*) input
      write(63,*)
      write(63,*) grid, '  grid file format'
      write(63,*) type, '  output file format'
      write(63,*) sim, '  wave propagation code'
      write(63,*) iden, '  sign of depth data'
      write(63,*) flag, '  units of grid data'
      write(63,*) idep, '  units of depth data'
      write(63,*)
      write(63,*) im, '  number of columns'
      write(63,*) jm, '  number of rows'
      write(63,*)
      write(63,*) xw, '  grid west edge'
      write(63,*) xe, '  grid east edge'
      write(63,*) ys, '  grid south edge'
      write(63,*) yn, '  grid north edge'
      write(63,*) dmin, '  minimum depth'
      write(63,*) dmax, '  maximum depth'
      write(63,*)
      write(63,*) xinc, '  horizontal increment'
      write(63,*) yinc, '  vertical increment'
      write(63,*) dx, '  horizontal dx'
      write(63,*) dy, '  vertical dy'
      write(63,*) dt, '  simulation time step'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine choice
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  3.1 Choose the desired tsunami source and check the entry.  
c
      if (itsu.eq.0) then
        write(*,*) 'Type 0 for a coseismic displacement source'
        write(*,*) 'Type 1 for a translational slide source'
        write(*,*) 'Type 2 for a rotational slump source'
        write(*,*) 'Type 3 for a subaerial landslide source'
        write(*,*) 'Type 4 for a pyroclastic flow source'
        read(*,*) fail
        write(*,*)
        if ((fail.lt.0).or.(fail.gt.4)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
      endif
c
c  3.2 Repeat this for gridded tsunami sources.
c
      if (itsu.eq.1) then
        write(*,*) 'Type 0 for a coseismic displacement source'
        write(*,*) 'Type 1 for a translational slide source'
        write(*,*) 'Type 2 for a rotational slump source'
        write(*,*) 'Type 3 for a subaerial landslide source'
        write(*,*) 'Type 4 for a pyroclastic flow source'
        write(*,*) 'Type 5 for a design wave source'
        read(*,*) fail
        write(*,*)
        if ((fail.lt.0).or.(fail.gt.5)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
      endif
c
c  3.3 You choose a specific ID number for this tsunami source.
c
      write(*,*) 'Enter a single digit source ID number'
      read(*,*) nsrc
      write(*,*)
      itmp = mod(nsrc,10)
      proper=char(itmp+48)
c
c  3.4 You choose the absolute time at which tsunami generation begins.
c
      write(*,*) 'Enter beginning time of tsunami generation (s)'
      read(*,*) tsrc
      write(*,*)
c
c  3.5 Open the properties file.
c
      write(*,*) 'Opening the properties file'
      write(*,*)
      open (63,file='out'//proper//'.txt')
c
c  3.6 Write data to the properties file.
c
      write(63,*) '          TOPICS Version 1.2'
      write(63,*) '            (c) 2000-2002'
      write(63,*) '     Applied Fluids Engineering, Inc.'
      write(63,*)
      write(63,*) fail, '  tsunami source'
      write(63,*) nsrc, '  source ID number'
      write(63,*)
      write(63,*) proper
      write(63,*)
      write(63,*) tsrc, '  start tsunami generation'
      write(63,*)
c
c  3.7 Choose the way to describe the earthquake fault geometry.
C
      if (fail.eq.0) then
        write(*,*) 'There are two ways to describe a planar trapezoid'
        write(*,*)
        write(*,*) 'Type 0 for a rectangle from earthquake parameters'
        write(*,*) 'Type 1 for four fault vertices and parameters'
        read(*,*) trap
        write(*,*)
c
c  3.8 Check the entry for validity.
c
        if ((trap.ne.0).and.(trap.ne.1)) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'That was an invalid entry'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
        write(63,*) trap, '  planar trapezoid'
        write(63,*)
      endif
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setfail
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  4.1 Identify the landslide center of mass position on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter initial x-axis mass failure center xo'
      read(*,*) xo
      write(*,*) 'Enter initial y-axis mass failure center yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of failure in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  4.2 Verify that the landslide is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      if (grd(ic,jc).le.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The mass failure is mostly subaerial'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The mass failure is mostly subaerial'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  4.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  4.4 Write data to properties file.
c
      write(63,*) xo, '  failure longitude'
      write(63,*) yo, '  failure latitude'
      write(63,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(63,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine slide
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  5.1 Help define the landslide and source geometry. 
C
      write(*,*) 'Slide motion follows Watts and Grilli (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is an effective mean depth of slide'
      write(*,*) 'An effective slope can mimic basal friction'
      write(*,*) 'The cutoff applies Gaussian decay at y=cut'
      write(*,*)
      write(*,*) 'The source should be in front of the slide'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(63,*) 'Slide motion follows Watts and Grilli (2002)'
      write(63,*)
      write(63,*) 'The positive x-axis is in the failure direction'
      write(63,*) 'The x-axis is also a line of mirror symmetry'
      write(63,*)
      write(63,*) 'The depth is an effective mean depth of slide'
      write(63,*) 'An effective slope can mimic basal friction'
      write(63,*) 'The cutoff applies Gaussian decay at y=cut'
      write(63,*)
      write(63,*) 'The source should be in front of the slide'
      write(63,*) 'The xo,yo position controls source location'
      write(63,*)
c
c  5.2 You choose the configuration of the underwater slide. 
c
      write(*,*) 'Enter initial depth of the middle of slide (m)'
      read(*,*) d
      write(*,*) 'Enter mean slope along failure plane (degrees)'
      read(*,*) theta
      write(*,*) 'Enter initial slide length during failure (m)'
      read(*,*) b
      write(*,*) 'Enter maximum initial slide thickness (m)'
      read(*,*) t
      write(*,*) 'Enter maximum initial slide width (m)'
      read(*,*) w
      if (itsu.eq.1) then
        write(*,*) 'Enter a maximum tsunami cutoff width (m)'
        read(*,*) cut
      else
        cut = 2.0D+02 * w
      endif
      write(*,*) 'Enter the slide bulk density (kg/m^3)'
      read(*,*) rhob         
      write(*,*)
c
c  5.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      theta = dabs(theta)
      b = dabs(b)         
      t = dabs(t)
      w = dabs(w)
      cut = dabs(cut)/2.0D+00
      rhob = dabs(rhob)
c
c  5.4 Check that the inputs are physically possible.  
c         
      if (t.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide thickness cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (b.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.le.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is zero or negative'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.ge.9.0D+01) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is at or beyond vertical'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhoo.ge.rhob) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is less than water density'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhob.ge.4.5D+03) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is unreasonably large'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  5.5 Convert theta to radians and calculate specific density.
c
      write(*,*) 'Calculating slide properties'
      write(*,*)
      theta = theta * pi / 1.8D+02
      cnt = dcos(theta)
      snt = dsin(theta)
      tnt = dtan(theta)
      gamma = rhob / rhoo
      gmo = gamma - 1.0D+00
c
c  5.6 Compute slide motion and tsunami characteristics.
c
      ao = g * snt * gmo / (gamma + 1.0D+00)
      ut = dsqrt(0.5D+00 * g * b * pi * snt * gmo)
      so = ut**2 / ao
      to = ut / ao
      dist = 0.4338D+00 * so
      lambda = to * dsqrt(g * d)
      hao = lambda / b
      sg = so * snt / d
      fr = ut / dsqrt(g * d)
      eta = 0.723D+00 * so * (4.772D-02 - 3.559D-02 * snt + 
     +      8.13D-03 * snt**2) * (t / b) * (b * snt / d)**1.25 * 
     +      1.18D+00 * (1.0D+00 - dexp(-2.2027D+00 * gmo)) 
      xg = (d + t / cnt) / tnt
      term = eta / (so * snt**1.5)
      xmin = 0.95D+00 * (xg + 0.4338D+00 * so * cnt) - xg
      delx = 0.5D+00 * lambda
c
c  5.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  5.8 Check the calculations for accuracy.
c
      if ((d/b).lt.0.02D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth is too small for the physics'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (cut.lt.(w/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The cutoff is much less than landslide width'
        write(*,*) 'This cutoff will be increased to 0.66*width'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          cut = 0.33D+00 * w
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The cutoff is much less than landslide width'
          write(63,*) 'This cutoff will be increased to 0.66*width'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (hao.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Hammack number is <1  ', hao
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Hammack number is <1  ', hao
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sg.gt.0.35D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Submergence number is >0.35  ', sg
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Submergence number is >0.35  ', sg
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (term.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = theta * 1.8D+02 / pi
      if (test.gt.3.0D+01) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The incline angle theta >30 degrees ',  test
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The incline angle theta >30 degrees ',  test
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((d/b).lt.0.12D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio d/b <0.12  ',  d / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio d/b <0.12  ',  d / b
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/b).gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/b >0.2  ',  t / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio t/b >0.2  ',  t / b
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/d).gt.3.33D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/d >3.33  ',  t / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio t/d >3.33  ',  t / d
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop                    
          endif
      endif
      if ((w/b).lt.0.06D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b <0.1  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio w/b <0.1  ',  w / b
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).gt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b >1.0  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio w/b >1.0  ',  w / b
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  5.9 Calculate quantities that determine tsunami shape. 
c
      zmin = -2.1D+00 * eta
      zmax = 0.64D+00 * eta * (0.8D+00 + 0.2D+00 * d / (b * snt))
      nmin = 1.2D+00 * zmin
      nmax = zmax
      denom = lambda
      shift = delx
      wid = lambda
c
c  5.10 Write data to properties file.
c
      write(63,*) d, '  mean initial depth'
      write(63,*) 1.8D+02 * theta / pi, '  mean incline angle'
      write(63,*) b, '  initial length'
      write(63,*) t, '  initial maximum thickness'
      write(63,*) w, '  initial maximum width'
      if (itsu.eq.1) then
        write(63,*) 2.0D+00 * cut, '  tsunami cutoff width'
      endif
      write(63,*) rhob, '  bulk density'
      write(63,*)
      write(63,*) gamma, '  specific density'
      write(63,*) ao, '  initial acceleration'
      write(63,*) ut, '  terminal velocity'
      write(63,*) so, '  characteristic distance'
      write(63,*) to, '  characteristic time'
      write(63,*) lambda, '  tsunami wavelength'
      write(63,*) dist, '  distance traveled in time to'
      write(63,*)
      write(63,*) hao, '  Hammack number'
      write(63,*) sg, '  Submergence number'
      write(63,*) fr, '  maximum Froude number'
      write(63,*) term, '  Quadratic number'
      write(63,*)
      write(63,*) eta, '  characteristic wave amplitude'      
      write(63,*) xg, '  gauge position'
      write(63,*) xmin, '  trough position'
      write(63,*) delx, '  trough to peak distance'
      write(63,*) zmin, '  trough argument'
      write(63,*) zmax, '  peak argument'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine slump
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  6.1 Help define the landslide and source geometry. 
C
      write(*,*) 'Slump motion follows Watts and Grilli (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is an effective mean depth of slump'
      write(*,*) 'The slope can also be an effective slope'
      write(*,*) 'The cutoff applies Gaussian decay at y=cut'
      write(*,*)
      write(*,*) 'The source should be in front of the slump'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(63,*) 'Slump motion follows Watts and Grilli (2002)'
      write(63,*)
      write(63,*) 'The positive x-axis is in the failure direction'
      write(63,*) 'The x-axis is also a line of mirror symmetry'
      write(63,*)
      write(63,*) 'The depth is an effective mean depth of slump'
      write(63,*) 'The slope can also be an effective slope'
      write(63,*) 'The cutoff applies Gaussian decay at y=cut'
      write(63,*)
      write(63,*) 'The source should be in front of the slump'
      write(63,*) 'The xo,yo position controls source location'
      write(63,*)
c
c  6.2 You choose the configuration of the underwater slump. 
c
      write(*,*) 'Enter initial depth of the middle of slump (m)'
      read(*,*) d
      write(*,*) 'Enter mean slope along failure plane (degrees)'
      read(*,*) theta
      write(*,*) 'Enter initial slump length during failure (m)'
      read(*,*) b
      write(*,*) 'Enter maximum initial slump thickness (m)'
      read(*,*) t
      write(*,*) 'Enter maximum initial slump width (m)'
      read(*,*) w
      if (itsu.eq.1) then
        write(*,*) 'Enter a maximum tsunami cutoff width (m)'
        read(*,*) cut
      else
        cut = 2.0D+02 * w
      endif
      write(*,*) 'Enter distance traveled by center of mass (m)'
      read(*,*) dist
      write(*,*) 'Enter the slump bulk density (kg/m^3)'
      read(*,*) rhob
      write(*,*)
c
c  6.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      theta = dabs(theta)
      b = dabs(b)
      t = dabs(t)
      w = dabs(w)
      cut = dabs(cut)/2.0D+00
      dist = dabs(dist)
      rhob = dabs(rhob)
c
c  6.4 Check that the inputs are physically possible.  
c
      if (t.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump thickness cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (b.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dist.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slump has to have moved to make a wave'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.le.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is zero or negative'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (theta.ge.9.0D+01) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of theta is at or beyond vertical'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhoo.ge.rhob) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is less than water density'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rhob.ge.4.5D+03) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The bulk density is unreasonably large'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  6.5 Convert theta to radians and calculate specific density.
c
      write(*,*) 'Calculating slump properties'
      write(*,*)
      theta = theta * pi / 1.8D+02
      cnt = dcos(theta)
      snt = dsin(theta)
      tnt = dtan(theta)
      gamma = rhob / rhoo
      gmo = gamma - 1.0D+00
c
c  6.6 Compute slump motion and tsunami characteristics.
c
      r = 0.125D+00 * b**2 / t + t / 2.0D+00
      dphi = dist / r
      so = dist / 2.0D+00
      to = dsqrt((r * (gamma + 1.0D+00)) / (g * gmo))
      ao = so / to**2
      ut = so / to
      lambda = 2.0D+00* to * dsqrt(g * d)
      hao = 0.5D+00 * lambda / b
      sg = so * snt / d
      fr = ut / dsqrt(g * d)
      eta = 0.723D+00 * so * (1.4662D+00 * gmo - 0.3454D+00 * 
     +      gmo**2) * snt**0.22 * (t / b) * (b / d)**1.25 *
     +      dphi**0.39 * (b / r)**0.63 * 0.1309D+00
      xg = (d + t / cnt) / tnt
      xmin = 0.565D+00 * (xg + 0.4597D+00 * so * cnt) - xg
      term = eta / (so * snt**1.5)
      delx = 0.5D+00 * lambda
c
c  6.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  6.8 Check the calculations for accuracy.
c
      if ((d/b).lt.0.02D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The initial depth is too small for the physics'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (cut.lt.(w/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The cutoff is much less than landslide width'
        write(*,*) 'This cutoff will be increased to 0.66*width'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          cut = 0.33D+00 * w
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The cutoff is much less than landslide width'
          write(63,*) 'This cutoff will be increased to 0.66*width'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (hao.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Hammack number is <1  ', hao
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Hammack number is <1  ', hao
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sg.gt.0.35D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Submergence number is >0.35  ', sg
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Submergence number is >0.35  ', sg
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (term.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The term (eta/so*sinq^1.5) >0.2  ',  term
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = theta * 1.8D+02 / pi
      if (test.gt.3.0D+01) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The incline angle >30 degrees ',  test
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The incline angle >30 degrees ',  test
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((d/b).lt.0.12D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio d/b <0.12  ',  d / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio d/b <0.12  ',  d / b
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/b).gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/b >0.2  ',  t / b
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio t/b >0.2  ',  t / b
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((t/d).gt.3.33D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio t/d >3.33  ',  t / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio t/d >3.33  ',  t / d
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (dphi.gt.0.53D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The angular motion dphi >0.53 radians  ', dphi
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The angular motion dphi >0.53 radians  ', dphi
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (((r/b).gt.2.0D+00).or.((r/b).lt.1.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio r/d >2 or r/d <1  ',  r / d
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio r/d >2 or r/d <1  ',  r / d
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).lt.0.25D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b <0.5  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio w/b <0.5  ',  w / b
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((w/b).gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The ratio w/b >2.0  ',  w / b
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The ratio w/b >2.0  ',  w / b
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  6.9 Calculate quantities that determine tsunami shape. 
c
      zmin = -eta * ((2.480D+00 * 0.2892D+00 - 0.7904D+00 * sg +
     +               1.3376D+00 * sg**2) / (0.2892D+00 +
     +               0.9163D+00 * sg))
      zmax = eta * ((1.686D+00 * 0.3498D+00 - 0.3531D+00 * sg +
     +               0.6466D+00 * sg**2) / (0.3498D+00 +
     +               1.0257D+00 * sg))
      nmin = 1.22D+00 * 1.15D+00 * zmin
      nmax = 1.22D+00 * zmax
      denom = 0.5D+00 * lambda
      shift = 0.8D+00 * delx
      wid = 0.5D+00 * lambda
c
c  6.10 Write data to properties file.
c
      write(63,*) d, '  mean initial depth'
      write(63,*) 1.8D+02 * theta / pi, '  mean incline angle'
      write(63,*) b, '  initial length'
      write(63,*) t, '  initial maximum thickness'
      write(63,*) w, '  initial maximum width'
      if (itsu.eq.1) then
        write(63,*) 2.0D+00 * cut, '  tsunami cutoff width'
      endif
      write(63,*) dist, '  distance of slump motion'
      write(63,*) rhob, '  bulk density'
      write(63,*)
      write(63,*) gamma, '  specific density'
      write(63,*) r, '  radius of curvature'
      write(63,*) dphi, '  angular rotation'
      write(63,*) ao, '  initial acceleration'
      write(63,*) ut, '  maximum velocity'
      write(63,*) so, '  characteristic distance'
      write(63,*) to, '  characteristic time'
      write(63,*) lambda, '  wavelength'
      write(63,*) dist, '  distance traveled'
      write(63,*) 
      write(63,*) hao, '  Hammack number'
      write(63,*) sg, '  Submergence number'
      write(63,*) fr, '  maximum Froude number'
      write(63,*) term, '  Quadratic number'
      write(63,*)
      write(63,*) eta, '  characteristic wave amplitude'
      write(63,*) xg, '  gauge position'
      write(63,*) xmin, '  trough position'
      write(63,*) delx, '  trough to peak distance'
      write(63,*) zmin, '  trough argument'
      write(63,*) zmax, '  peak argument'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridcheck
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  7.1 Stop the program if the grid size is too small to resolve tsunami.  
c
      if (((delx / 7.0D+00).lt.dx).or.((delx / 7.0D+00).lt.dy)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wavelength is too small for grid'
        write(*,*) 'TOPICS should be stopped immediately'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wavelength is too small for grid'
          write(63,*) 'TOPICS should be stopped immediately'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  7.2 Warn if the grid size is too small to be accurate.  
c
      if (((delx / 2.0D+01).lt.dx).or.((delx / 2.0D+01).lt.dy)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The grid size is >0.05 times wavelength'
        write(*,*) 'This may lead to simulation errors'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The grid size is >0.05 times wavelength'
          write(63,*) 'This may lead to simulation errors'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridinit
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  8.1 Open the grid files.
c
      write(*,*) 'Opening the grid files'
      write(*,*)
      open (61,file='surface'//proper//output)
      if (fail.ne.0) then
        open (64,file='uvel'//proper//output)
        open (65,file='vvel'//proper//output)
      endif
c
c  8.2 Initialize the grid files to zero.  
c
      do 85 j=1,jm
        do 85 i=1,im
          out(i,j) = 0.0D+00
          uvel(i,j) = 0.0D+00
          vvel(i,j) = 0.0D+00
  85  continue
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridfail
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  9.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 91 j=1,jm
        do 91 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 1.0D+00 - dexp(-2.0906D+00 * (w / wid) * 
     +          (1.0D+00 + 1.0903D+00 * (w / wid)))
          out(i,j) = term *
     +      (nmin * dexp(-(nmin * (x - xmin) / (denom * nmax))**2) +
     +       nmax * dexp(-((x - xmin - shift) / (denom))**2)) *
     +      (2.0D+00 / (dexp(3.0D+00 * term * y / w) +
     +                 dexp(-3.0D+00 * term * y / w)))**2
          if (dabs(y).gt.cut) then
            out(i,j) = out(i,j) * 
     +                 dexp(-(5.0D+00 * (dabs(y) - cut) / cut)**2)
          endif
c
c  9.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
          if ((x.lt.xmin).and.(out(i,j).gt.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((x.gt.(xmin+shift)).and.(out(i,j).lt.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
  91  continue
c
c  9.4 Estimate linear wave quantities. 
c
      kappa = 2.0D+00 * pi / lambda
      term = kappa * d
      omega = dsqrt(g * kappa * dtanh(term))
c
c  9.5 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      do 96 j=1,jm
        do 96 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.6 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = fact1 * g * dtanh(term) / (d * omega)
          endif
          if (sim.eq.1) then
            utot = fact1 * g * kappa * dcosh(0.469D+00 * term) / 
     +            (omega * dcosh(term))
          endif
          if (fact1.gt.0.0D+00) then
            uvel(i,j) =  -utot * sna
          else
            uvel(i,j) =  0.0D+00
          endif
c
c  9.7 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
   96 continue
c
c  9.8 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 97 j=1,jm
        do 97 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  9.9 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = fact1 * g * dtanh(term) / (d * omega)
          endif
          if (sim.eq.1) then
            utot = fact1 * g * kappa * dcosh(0.469D+00 * term) / 
     +            (omega * dcosh(term))
          endif
          if (fact1.gt.0.0D+00) then
            vvel(i,j) = utot * cna
          else
            vvel(i,j) =  0.0D+00
          endif
c
c  9.10 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
   97 continue
c
c  9.11 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 93 j=1,jm
        do 93 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
   93 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 94 j=1,jm
        do 94 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
   94 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 95 j=1,jm
        do 95 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
   95 continue
c
c  9.12 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) umin, '  trough u velocity'
      write(63,*) umax, '  peak u velocity'
      write(63,*) vmin, '  trough v velocity'
      write(63,*) vmax, '  peak v velocity'
      write(63,*)
c
c  9.13 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charfail
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  10.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.1D-03 * (xmin + 6.0D+00 * shift)
      do 101 j=0,10000
        x = dfloat(j) * ddx - 3.0D+00 * shift
        term = 1.0D+00 - dexp(-2.0906D+00 * (w / wid) * 
     +        (1.0D+00 + 1.0903D+00 * (w / wid)))
        test= term *
     +       (nmin * dexp(-(nmin * (x - xmin) / (denom * nmax))**2) +
     +        nmax * dexp(-((x - xmin - shift) / (denom))**2))
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  101 continue
c
c  10.2 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine seismic
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  11.1 Help define the geometry of the fault plane and vertices. 
C
      write(*,*) 'Fault geometry follows the work of Okada (1985)'
      write(*,*) 'The earthquake magnitude provides input checks'
      write(*,*) 'The shear modulus provides the energy release'
      write(*,*)
      write(*,*) 'There is a local axis system for the fault'
      write(*,*) 'The positive x-axis is in the strike direction'
      write(*,*) 'The positive z-axis is pointing up vertically'
      write(*,*) 'The positive y-axis follows from the right hand'
      write(*,*)
      write(*,*) 'Strike is measured clockwise from north'
      write(*,*) 'Dip is measured positive from horizontal'
      write(*,*) 'Rake provides slip direction in fault plane'
      write(*,*) 'Rake increases counterclockwise from strike'
      write(*,*) 'Add/subtract 180 degrees from rake if needed'
      write(*,*)
      write(*,*) 'The fault plane includes local point 0,0,-D'
      write(*,*)
      write(*,*) 'P1 has positive x and is generally less deep'
      write(*,*) 'P2 has negative x and is generally less deep'
      write(*,*) 'P3 has negative x and is generally more deep'
      write(*,*) 'P4 has positive x and is generally more deep'
      write(*,*)
      write(*,*) 'A Gaussian slip distribution exists about 0,0'
      write(*,*) 'The singularity at zero depth is also removed'
      write(*,*)
      write(63,*) 'Fault geometry follows the work of Okada (1985)'
      write(63,*) 'The earthquake magnitude provides input checks'
      write(63,*) 'The shear modulus provides the energy release'
      write(63,*)
      write(63,*) 'There is a local axis system for the fault'
      write(63,*) 'The positive x-axis is in the strike direction'
      write(63,*) 'The positive z-axis is pointing up vertically'
      write(63,*) 'The positive y-axis follows from the right hand'
      write(63,*)
      write(63,*) 'Strike is measured clockwise from north'
      write(63,*) 'Dip is measured positive from horizontal'
      write(63,*) 'Rake provides slip direction in fault plane'
      write(63,*) 'Rake increases counterclockwise from strike'
      write(63,*) 'Add/subtract 180 degrees from rake if needed'
      write(63,*)
      write(63,*) 'The fault plane includes local point 0,0,-D'
      write(63,*)
      write(63,*) 'P1 has positive x and is generally less deep'
      write(63,*) 'P2 has negative x and is generally less deep'
      write(63,*) 'P3 has negative x and is generally more deep'
      write(63,*) 'P4 has positive x and is generally more deep'
      write(63,*)
      write(63,*) 'A Gaussian slip distribution exists about 0,0'
      write(63,*) 'The singularity at zero depth is also removed'
      write(63,*)
c
c  11.2 You choose the parameters of the fault rectangle. 
C
      If (trap.eq.0) then
        write(*,*) 'Enter fault dip from horizontal (0-180 deg)'
        read(*,*) dip
        write(*,*) 'Enter slip rake from horizontal (0-360 deg)'
        read(*,*) rake
        write(*,*) 'Enter earthquake magnitude from some scale'
        read(*,*) mw
        write(*,*) 'Enter maximum value of earthquake slip (m)'
        read(*,*) slip
        write(*,*) 'Enter the centroid fault plane depth (m)'
        read(*,*) depth
        write(*,*) 'Enter the fault length along strike (m)'
        read(*,*) leng
        write(*,*) 'Enter the fault width perpendicular to strike (m)'
        read(*,*) high        
        write(*,*) 'Enter shear modulus in format 4.0D+10 (kg/m-s^2)'
        read(*,*) mu
        write(*,*) 'Enter radius for slip to drop 50% max (m)'
        read(*,*) sharp
        write(*,*) 'Enter depth where slip drops to 1% max (m)'
        read(*,*) sing  
        write(*,*) 'Enter a typical water depth for earthquake (m)'
        read(*,*) d         
        write(*,*)
c
c  11.3 Make sure that all entries are positive and in radians.
c
        dip = dabs(dip)
        dip = dip * pi / 1.8D+02
        rake = dabs(rake)
        rake = rake * pi / 1.8D+02
        mw = dabs(mw)         
        slip = dabs(slip)
        depth = dabs(depth)
        leng = dabs(leng)
        high = dabs(high)
        mu = dabs(mu)
        sharp = dabs(sharp)
        sing = dabs(sing)
        d = dabs(d)
c
c  11.4 Calculate the four corners of the rectangular fault plane.
c
        x1 = 0.5D+00 * leng
        y1 = 0.5D+00 * high * dcos(dip)
        z1 = -depth + 0.5D+00 * high * dsin(dip)
        x2 = -0.5D+00 * leng
        y2 = 0.5D+00 * high * dcos(dip)
        z2 = -depth + 0.5D+00 * high * dsin(dip)
        x3 = -0.5D+00 * leng
        y3 = -0.5D+00 * high * dcos(dip)
        z3 = -depth - 0.5D+00 * high * dsin(dip)
        x4 = 0.5D+00 * leng
        y4 = -0.5D+00 * high * dcos(dip)
        z4 = -depth - 0.5D+00 * high * dsin(dip)
c
c  11.5 You choose the necessary earthquake parameters. 
C
      else
        write(*,*) 'Enter fault dip from horizontal (0-180 deg)'
        read(*,*) dip
        write(*,*) 'Enter slip rake from horizontal (0-360 deg)'
        read(*,*) rake
        write(*,*) 'Enter earthquake magnitude from some scale'
        read(*,*) mw
        write(*,*) 'Enter maximum value of earthquake slip (m)'
        read(*,*) slip
        write(*,*) 'Enter the centroid fault plane depth (m)'
        read(*,*) depth      
        write(*,*) 'Enter shear modulus in format 4.0D+10 (kg/m-s^2)'
        read(*,*) mu
        write(*,*) 'Enter radius for slip to drop 50% max (m)'
        read(*,*) sharp
        write(*,*) 'Enter depth where slip drops to 1% max (m)'
        read(*,*) sing        
        write(*,*) 'Enter a typical water depth for earthquake (m)'
        read(*,*) d
        write(*,*)
c
c  11.6 Make sure that all entries are positive and that dip is less than pi.
c
        dip = dabs(dip)
        dip = dip * pi / 1.8D+02
        rake = dabs(rake)
        rake = rake * pi / 1.8D+02
        mw = dabs(mw)         
        slip = dabs(slip)
        depth = dabs(depth)
        mu = dabs(mu)
        sharp = dabs(sharp)
        sing = dabs(sing)
        d = dabs(d)
        if (dip.gt.pi) then
          write(*,*)
          write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
          write(*,*) 'The dip cannot be greater than 180 degrees'
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop
        endif
c
c  11.7 You choose the positions of the four trapezoidal vertices.
c
        write(*,*) 'Enter distances of vertices from origin 0,0,-D'
        write(*,*) 'For 45<dip<135 degrees, enter x and z distances'
        write(*,*) 'For other dip angles, enter x and y distances'
        write(*,*) 'The origin need not be the center of trapezoid'
        write(*,*)
        test = pi / 4.0D+00
        write(*,*) 'Enter the positive value of x1 (m)'
        read(*,*) x1
        x1 = dabs(x1)
        if (dip.lt.test) then
          write(*,*) 'Enter the positive value of y1 (m)'
          read(*,*) y1
          y1 = dabs(y1)
          z1 = dtan(dip) * y1 - depth
        endif
        if (dip.gt.(3.*test)) then
          write(*,*) 'Enter the negative value of y1 (m)'
          read(*,*) y1
          y1 = -dabs(y1)
          z1 = dtan(dip) * y1 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.*test))) then
          write(*,*) 'Enter the positive value of z1 (m)'
          read(*,*) z1
          z1 = dabs(z1) - depth
          y1 = dcos(dip) * (z1 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the negative value of x2 (m)'
        read(*,*) x2
        x2 = -dabs(x2)
        if (dip.lt.test) then
          write(*,*) 'Enter the positive value of y2 (m)'
          read(*,*) y2
          y2 = dabs(y2)
          z2 = dtan(dip) * y2 - depth
        endif
        if (dip.gt.(3.*test)) then
          write(*,*) 'Enter the negative value of y2 (m)'
          read(*,*) y2
          y2 = -dabs(y2)
          z2 = dtan(dip) * y2 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.*test))) then
          write(*,*) 'Enter the positive value of z2 (m)'
          read(*,*) z2
          z2 = dabs(z2) - depth
          y2 = dcos(dip) * (z2 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the negative value of x3 (m)'
        read(*,*) x3
        x3 = -dabs(x3)
        if (dip.lt.test) then
          write(*,*) 'Enter the negative value of y3 (m)'
          read(*,*) y3
          y3 = -dabs(y3)
          z3 = dtan(dip) * y3 - depth
        endif
        if (dip.gt.(3.*test)) then
          write(*,*) 'Enter the positive value of y3 (m)'
          read(*,*) y3
          y3 = dabs(y3)
          z3 = dtan(dip) * y3 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.*test))) then
          write(*,*) 'Enter the negative value of z3 (m)'
          read(*,*) z3
          z3 = -dabs(z3) - depth
          y3 = dcos(dip) * (z3 + depth) / dsin(dip)
        endif
        write(*,*) 'Enter the positive value of x4 (m)'
        read(*,*) x4
        x4 = dabs(x4)
        if (dip.lt.test) then
          write(*,*) 'Enter the negative value of y4 (m)'
          read(*,*) y4
          y4 = -dabs(y4)
          z4 = dtan(dip) * y4 - depth
        endif
        if (dip.gt.(3.*test)) then
          write(*,*) 'Enter the positive value of y4 (m)'
          read(*,*) y4
          y4 = dabs(y4)
          z4 = dtan(dip) * y4 - depth
        endif
        if ((dip.ge.test).and.(dip.le.(3.*test))) then
          write(*,*) 'Enter the negative value of z4 (m)'
          read(*,*) z4
          z4 = -dabs(z4) - depth
          y4 = dcos(dip) * (z4 + depth) / dsin(dip)
        endif
        write(*,*)
        leng = dabs(x1 + x4 - x2 - x3) / 2.0D+00
        if ((dip.gt.test).and.(dip.lt.(3.*test))) then
          high = dabs(z1 + z2 - z3 - z4) / (2.0D+00 * dsin(dip))
        else
          high = dabs(y1 + y2 - y3 - y4) / (2.0D+00 * dcos(dip))
        endif
      endif
c
c  11.8 Check that the inputs are physically possible.  
c         
      if (mw.gt.1.0D+01) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The magnitude cannot be greater than 10'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (mw.lt.4.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The magnitude cannot be less than 4'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dip.gt.pi) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The dip cannot be greater than 180 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dip.lt.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The dip cannot be less than 0 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rake.gt.(2.0D+00*pi)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The rake cannot be greater than 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (rake.lt.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The rake cannot be less than 0 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (mw.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The earthquake magnitude cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (slip.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault slip cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (high.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (leng.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (sing.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The surface decay rate cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (sharp.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The slip decay radius cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (z3.ge.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The point P3 cannot be at or above ground'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (z4.ge.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The point P4 cannot be at or above ground'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  11.9 Establish the characteristic period and size of tsunami.
c
      write(*,*) 'Calculating earthquake properties'
      write(*,*)
      to = min(leng,high) / dsqrt(g * d)
      delx = leng
      if (high.lt.leng) delx = high
c
c  11.10 Compare the grid size to the fault size.
c
      if (itsu.eq.1) call gridcheck
c
c  11.11 Check the entries for accuracy.
c
      test = high * dsin(dip) / 2.0D+00
      if ((test.gt.depth).and.(trap.eq.1)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault depth is less than fault height'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The fault depth is less than fault height'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((test.gt.depth).and.(trap.eq.0)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The fault depth is less than fault height'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((mu.lt.1.0D+08).or.(mu.gt.1.0D+12)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The shear modulus is unreasonable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif  
      if ((mu.lt.1.0D+10).or.(mu.gt.1.0D+11)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The shear modulus is exceptional', mu
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The shear modulus is exceptional', mu
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (z1.gt.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault vertex P1 is above ground'
        write(*,*) 'z1 will be reduced to ground level'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          if ((dip.eq.0.0D+00).or.(dip.eq.pi)) stop
          z1 = 0.0D+00
          y1 = dcos(dip) * (z1 + depth) / dsin(dip)
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The fault vertex P1 is above ground'
          write(63,*) 'z1 will be reduced to ground level'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (z2.gt.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault vertex P2 is above ground'
        write(*,*) 'The value of z2 will be reduced to 0'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          if ((dip.eq.0.0D+00).or.(dip.eq.pi)) stop
          z2 = 0.0D+00
          y2 = dcos(dip) * (z2 + depth) / dsin(dip)
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The fault vertex P2 is above ground'
          write(63,*) 'The value of z2 will be reduced to 0'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 10**(0.5D+00 * mw - 3.3D+00)
      if (slip.lt.(test/5.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip is much less than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The slip is much less than predicted', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (slip.gt.(test*5.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Slip is much greater than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Slip is much greater than predicted', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 10**(0.5D+00 * mw + 1.1D+00)
      if (leng.lt.(test/3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The length is less than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The length is less than predicted', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (leng.gt.(test*3.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The length is greater than predicted', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The length is greater than predicted', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = leng / high 
      if ((test.lt.1.5D+00).or.(test.gt.3.5D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The fault aspect ratio is exceptional', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The fault aspect ratio is exceptional', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (high.le.leng) test = high / 1.0D+01
      if (high.gt.leng) test = leng / 1.0D+01
      if (sharp.lt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay radius is unreasonably small'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The slip decay radius is unreasonably small'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sing.lt.(5.0D+00*slip)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay depth is too small'
        write(*,*) 'The depth will be increased to 5*slip'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          sing = 5.0D+00 * slip
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The slip decay depth is too small'
          write(63,*) 'The depth will be increased to 5*slip'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (sing.gt.(5.0D+01*slip)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The slip decay depth is too large'
        write(*,*) 'The depth will be decreased to 50*slip'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          sing = 5.0D+01 * slip
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The slip decay depth is too large'
          write(63,*) 'The depth will be decreased to 50*slip'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  11.12 Write data to properties file.
c
      write(63,*) 1.8D+02 * dip / pi, '  fault dip angle'
      write(63,*) 1.8D+02 * rake / pi, '  slip rake angle'
      write(63,*) mw, '  earthquake magnitude'
      write(63,*) slip, '  maximum fault slip'
      write(63,*) depth, '  centroid depth'
      write(63,*) leng, '  typical fault length'
      write(63,*) high, '  typical fault width'
      write(63,*) mu, '  shear modulus'
      write(63,*) sharp, '  radius for slip to drop 50%'
      write(63,*) sing, '  depth for slip to drop to 1%'
      write(63,*) d, '  typical water depth'
      write(63,*)
      write(63,*) x1, '  x-position of P1'
      write(63,*) y1, '  y-position of P1'
      write(63,*) z1, '  z-position of P1'
      write(63,*)
      write(63,*) x2, '  x-position of P2'
      write(63,*) y2, '  y-position of P2'
      write(63,*) z2, '  z-position of P2'
      write(63,*)
      write(63,*) x3, '  x-position of P3'
      write(63,*) y3, '  y-position of P3'
      write(63,*) z3, '  z-position of P3'
      write(63,*)
      write(63,*) x4, '  x-position of P4'
      write(63,*) y4, '  y-position of P4'
      write(63,*) z4, '  z-position of P4'
      write(63,*)
      write(63,*) to, '  typical tsunami period'
      write(63,*) delx, '  typical fault size'
      write(63,*) 
c
c  11.13 Rotating rake 180 degrees works better.  
c
      rake = rake - pi
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setquake
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  12.1 Identify the center of the fault and strike on the grid.  
C
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis centroid of the fault xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis centroid of the fault yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured clockwise'
      write(*,*)
      write(*,*) 'Enter CW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CW angle of strike in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  12.2 Verify that the earthquake is within the grid.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The value of xo is outside the grid'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The value of yo is outside the grid'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  12.3 Convert degrees to radians, CW to CCW, and compute the cosine and sine. 
c
      alpha = 2.0D+00 * pi - alpha * pi / 1.8D+02
      alfao = 2.0D+00 * pi - alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  12.4 Write data to properties file.
c
      write(63,*) xo, '  fault centroid longitude'
      write(63,*) yo, '  fault centroid latitude'
      test = 3.6D+02 - 1.8D+02 * alfao / pi
      write(63,*) test, '  orientation of grid north'
      test = 3.6D+02 - 1.8D+02 * alpha / pi
      write(63,*) test, '  orientation of strike from north'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridquake
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  13.1 Enter the number of fault subdivisions in x,y directions.
c
      write(*,*) 'Enter number of x-direction subdivisions (20-200)'
      read(*,*) nx
      write(*,*) 'Enter number of y-direction subdivisions (20-200)'
      read(*,*) ny
      write(*,*)
      nx = iabs(nx)
      ny = iabs(ny)
      if ((nx.gt.200).or.(nx.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of nx is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((ny.gt.200).or.(ny.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of ny is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      write(63,*) nx, '  x-direction divisions'
      write(63,*) ny, '  y-direction divisions'
      write(63,*)
c
c  13.2 Compute some constant values for these calculations.
c
      cdip = dcos(dip)
      sdip = dsin(dip)
      elast = 0.5D+00
      u1var = slip * dcos(rake)
      u2var = slip * dsin(rake)
      bnd12 = dabs(x1 - x2) / dfloat(nx)
      bnd23 = dsqrt((z3 - z2)**2 + (y3 - y2)**2) / dfloat(ny)
      bnd34 = dabs(x3 - x4) / dfloat(nx)
      bnd41 = dsqrt((z1 - z4)**2 + (y1 - y4)**2) / dfloat(ny)
c
c  13.3 Zero the value of the earthquake moment to sum up elements.
c
      momt = 0.0D+00
c
c  13.4 Calculate the vertical coseismic displacement over the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 131 j=1,jm
        if (mod(j,100).eq.0) write(*,*)'  j = ',j
        do 131 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  13.5 Interpolate and sum the point sources from the four fault vertices.
c
          do 132 k=1,nx
            do 132 l=1,ny 
              x12 = x2 + (dfloat(k) - 0.5D+00) * (x1 - x2) / 
     +                    dfloat(nx)
              x34 = x3 + (dfloat(k) - 0.5D+00) * (x4 - x3) / 
     +                    dfloat(nx)
              xp = x12 + (dfloat(l) - 0.5D+00) * (x34 - x12) / 
     +                    dfloat(ny)
              xvar = x - xp
              y12 = y2 + (dfloat(k) - 0.5D+00) * (y1 - y2) / 
     +                    dfloat(nx)
              y34 = y3 + (dfloat(k) - 0.5D+00) * (y4 - y3) / 
     +                    dfloat(nx)
              yp = y12 + (dfloat(l) - 0.5D+00) * (y34 - y12) / 
     +                    dfloat(ny)
              yvar = y - yp
              z12 = z2 + (dfloat(k) - 0.5D+00) * (z1 - z2) / 
     +                    dfloat(nx)
              z34 = z3 + (dfloat(k) - 0.5D+00) * (z4 - z3) / 
     +                    dfloat(nx)
              zp = z12 + (dfloat(l) - 0.5D+00) * (z34 - z12) / 
     +                    dfloat(ny)
              dvar = -zp
              area = (bnd12 + (dfloat(l) - 0.5D+00) * 
     +               (bnd34 - bnd12) / dfloat(ny)) * 
     +               (bnd23 + (dfloat(k) - 0.5D+00) * 
     +               (bnd41 - bnd23) / dfloat(nx))
              if (dvar.le.0.0D+00) goto 133
              rado = dsqrt(xvar**2 + yvar**2 + dvar**2)
              radd = rado + dvar
              pvar = yvar * cdip + dvar * sdip 
              qvar = yvar * sdip - dvar * cdip
              fact1 = dexp(-0.6931D+00 * ((xp**2 + yp**2 + 
     +                     (zp + depth)**2) / sharp**2))
              fact2 = dexp(4.6052D+00 * sing / zp)
              term = xvar * (2.0D+00 * rado + dvar) / 
     +                      (rado**3 * radd**2)
              i4var = -elast * yvar * term 
              i5var = elast * (1.0D+00 / (rado * radd) - xvar * term)
              out(i,j) = out(i,j) - area * fact1 * fact2 * 0.5D+00 *
     +                  (u1var * (3.0D+00 * xvar * dvar * qvar / 
     +                            rado**5 + i4var * sdip) +
     +                   u2var * (3.0D+00 * dvar * pvar * qvar / 
     +                            rado**5 - i5var * sdip * cdip)) / pi
              if ((i.eq.1).and.(j.eq.1)) then
                momt = momt + slip * fact1 * fact2 * area * mu
              endif
  133         continue
  132     continue
c
c  13.6 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  131 continue
      if (jm.ge.100) write(*,*)
c
c  13.7 Search for the maximum and minimum amplitudes.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 134 j=1,jm
        do 134 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  134 continue
c
c  13.8 Write data to properties file.
c
      write(63,*) momt, '  earthquake moment'
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*)
c
c  13.9 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charquake
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp 
c
c  --------------------------------------------------------------------
c
c  14.1 Enter the number of fault subdivisions in x,y directions.
c
      write(*,*) 'Enter number of x-direction subdivisions (20-200)'
      read(*,*) nx
      write(*,*) 'Enter number of y-direction subdivisions (20-200)'
      read(*,*) ny
      write(*,*)
      nx = iabs(nx)
      ny = iabs(ny)
      if ((nx.gt.200).or.(nx.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of nx is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((ny.gt.200).or.(ny.lt.20)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of ny is unacceptable'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      write(63,*) nx, '  x-direction divisions'
      write(63,*) ny, '  y-direction divisions'
      write(63,*)
c
c  14.2 Compute some constant values for these calculations.
c
      cdip = dcos(dip)
      sdip = dsin(dip)
      elast = 0.5D+00
      u1var = slip * dcos(rake)
      u2var = slip * dsin(rake)
      bnd12 = dabs(x1 - x2) / dfloat(nx)
      bnd23 = dsqrt((z3 - z2)**2 + (y3 - y2)**2) / dfloat(ny)
      bnd34 = dabs(x3 - x4) / dfloat(nx)
      bnd41 = dsqrt((z1 - z4)**2 + (y1 - y4)**2) / dfloat(ny)
c
c  14.3 Estimate the tsunami minimum and maximum amplitude and earthquake moment. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 1.0D-03 * 2.0D+00 * high
      x = 0.0D+00
      momt = 0.0D+00
      do 141 j=0,1000
        y = -high + dfloat(j) * ddx
        test = 0.0D+00
        do 142 k=1,nx
          do 142 l=1,ny 
            x12 = x2 + (dfloat(k) - 0.5D+00) * (x1 - x2) / 
     +                  dfloat(nx)
            x34 = x3 + (dfloat(k) - 0.5D+00) * (x4 - x3) / 
     +                  dfloat(nx)
            xp = x12 + (dfloat(l) - 0.5D+00) * (x34 - x12) / 
     +                  dfloat(ny)
            xvar = x - xp
            y12 = y2 + (dfloat(k) - 0.5D+00) * (y1 - y2) / 
     +                  dfloat(nx)
            y34 = y3 + (dfloat(k) - 0.5D+00) * (y4 - y3) / 
     +                  dfloat(nx)
            yp = y12 + (dfloat(l) - 0.5D+00) * (y34 - y12) / 
     +                  dfloat(ny)
            yvar = y - yp
            z12 = z2 + (dfloat(k) - 0.5D+00) * (z1 - z2) / 
     +                  dfloat(nx)
            z34 = z3 + (dfloat(k) - 0.5D+00) * (z4 - z3) / 
     +                  dfloat(nx)
            zp = z12 + (dfloat(l) - 0.5D+00) * (z34 - z12) / 
     +                  dfloat(ny)
            dvar = -zp
            area = (bnd12 + (dfloat(l) - 0.5D+00) * 
     +             (bnd34 - bnd12) / dfloat(ny)) * 
     +             (bnd23 + (dfloat(k) - 0.5D+00) * 
     +             (bnd41 - bnd23) / dfloat(nx))
            if (dvar.le.0.0D+00) goto 143
            rado = dsqrt(xvar**2 + yvar**2 + dvar**2)
            radd = rado + dvar
            pvar = yvar * cdip + dvar * sdip 
            qvar = yvar * sdip - dvar * cdip
            fact1 = dexp(-0.6931D+00 * ((xp**2 + yp**2 + 
     +                   (zp + depth)**2) / sharp**2))
            fact2 = dexp(4.6052D+00 * sing / zp)
            term = xvar * (2.0D+00 * rado + dvar) / 
     +                    (rado**3 * radd**2)
            i4var = -elast * yvar * term 
            i5var = elast * (1.0D+00 / (rado * radd) - xvar * term)
            test = test - area * fact1 * fact2 * 0.5D+00 *
     +            (u1var * (3.0D+00 * xvar * dvar * qvar / 
     +                      rado**5 + i4var * sdip) +
     +             u2var * (3.0D+00 * dvar * pvar * qvar / 
     +                      rado**5 - i5var * sdip * cdip)) / pi
            if (j.eq.1) then
              momt = momt + slip * fact1 * fact2 * area * mu
            endif
  143       continue
  142   continue
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  141 continue
c
c  14.4 Write data to properties file.
c
      write(63,*) momt, '  earthquake moment'
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine splash
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  15.1 Help define the landslide and source characteristics. 
C
      write(*,*) 'Source description follows Walder et al. (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the failure direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'The depth is a final depth of landslide deposit'
      write(*,*) 'The velocity provides a check on dynamics'
      write(*,*) 'The width provides a check on geometry'
      write(*,*) 'Two tsunami amplitudes are calculated'
      write(*,*) 'The smallest tsunami amplitude is used'
      write(*,*)
      write(*,*) 'The source will be in front of runout length'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(63,*) 'Source description follows Walder et al. (2002)'
      write(63,*)
      write(63,*) 'The positive x-axis is in the failure direction'
      write(63,*) 'The x-axis is also a line of mirror symmetry'
      write(63,*)
      write(63,*) 'The depth is a mean depth of landslide deposit'
      write(63,*) 'The velocity provides a check on dynamics'
      write(63,*) 'The width provides a check on geometry'
      write(63,*) 'Two tsunami amplitudes are calculated'
      write(63,*) 'The smallest tsunami amplitude is used'
      write(63,*)
      write(63,*) 'The source will be in front of runout length'
      write(63,*) 'The xo,yo position controls source location'
      write(63,*)
c
c  15.2 You choose the configuration of the subaerial landslide. 
c
      write(*,*) 'Enter the water depth near end of landslide (m)'
      read(*,*) d
      write(*,*) 'Enter the landslide volume within water (m^3)'
      read(*,*) vol
      write(*,*) 'Enter landslide velocity at shoreline (m/s)'
      read(*,*) ut
      write(*,*) 'Enter landslide runout length in water (m)'
      read(*,*) so
      write(*,*) 'Enter landslide runout time in water (s)'
      read(*,*) to         
      write(*,*) 'Enter the landslide width at shoreline (m)'
      read(*,*) w
      write(*,*)
c
c  15.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      vol = dabs(vol)
      ut = dabs(ut)         
      so = dabs(so)
      to = dabs(to)
      w = dabs(w)
c
c  15.4 Check that the inputs are physically possible.  
c         
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout time cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (so.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (vol.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide volume cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (w.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (ut.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The landslide velocity cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  15.5 Compute subaerial landslide tsunami characteristics.
c
      write(*,*) 'Calculating landslide properties'
      write(*,*)
      ao = -ut / to
      fr = ut / dsqrt(g * d)
      lambda = 0.27D+00 * to * dsqrt(g * d)
      test = 0.86D+00 * d
      etal = 1.32D+00 * d * (vol * 2.0D+00 * ut / 
     +                      (pi * w * so * d * dsqrt(d * g)))**0.68
      etat = 1.32D+00 * d * (vol / 
     +                      (w * to * d * dsqrt(d * g)))**0.68
      xmin = so + lambda
      delx = lambda
c
c  15.6 Choose the smallest amplitude for two measures of time.
c
      if (etal.lt.etat) then
        eta = etal
        qs = vol * 2.0D+00 * ut / (pi * w * so * d * dsqrt(d * g))
        tm = pi * so * dsqrt(g / d) / (2.0D+00 * ut)
      else
        eta = etat
        qs = vol / (w * to * d * dsqrt(d * g))
        tm = to * dsqrt(g / d)
      endif
c
c  15.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  15.8 Check the calculations for accuracy.
c
      if (eta.gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave amplitude is excessively large'
        write(*,*) 'The amplitude will be decreased to 0.86*depth'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          eta = test
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave amplitude is excessively large'
          write(63,*) 'The amplitude will be decreased to 0.86*depth'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(etal - etat) / (etal + etat)
      if (test.gt.0.4D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The two wave amplitudes differ more than 40%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The two wave amplitudes differ more than 40%'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is >4  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Froude number is >4  ', fr
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is <1  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Froude number is <1  ', fr
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * to * ut / (so * pi)
      if ((test.lt.0.4D+00).or.(test.gt.2.5D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Runout length and time differ  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = to / (4.5D+00 * dsqrt(1.0D+01 * dsqrt(vol / w) / g))
      if ((test.lt.0.3D+00).or.(test.gt.3.3D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Runout length and time differ  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 3.4D+00 * vol**(1.0D+00 / 3.0D+00) / w
      if ((test.lt.0.25D+00).or.(test.gt.4.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Landslide width may be unusual  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Landslide width may be unusual  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  15.9 Calculate quantities that determine tsunami shape. 
c
      zmax = eta
      nmax = zmax
      wid = lambda
c
c  15.10 Write data to properties file.
c
      write(63,*) d, '  typical final depth'
      write(63,*) vol, '  submerged landslide volume'
      write(63,*) ut, '  impact velocity'
      write(63,*) so, '  runout length'
      write(63,*) to, '  runout time'
      write(63,*) w, '  landslide width at shore'
      write(63,*)
      write(63,*) ao, '  landslide deceleration'
      write(63,*) lambda, '  wavelength'
      write(63,*) fr, '  impact Froude number'
      write(63,*) qs, '  nondimensional flux'
      write(63,*) tm, '  nondimensional time'
      write(63,*)
      write(63,*) etal, '  amplitude from runout length'
      write(63,*) etat, '  amplitude from runout time'
      write(63,*) eta, '  characteristic wave amplitude'
      write(63,*) 0.85D+00 * d, '  maximum possible amplitude'
      write(63,*) xmin, '  peak position'
      write(63,*) delx, '  peak width'
      write(63,*) zmax, '  peak argument'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setcoast
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  16.1 Identify the shoreline position of landslide on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis landslide entry at shoreline xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis landslide entry at shoreline yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of failure in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  16.2 Verify that the landslide is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      test = dmax / 1.0D+02
      if (test.lt.dabs(dx + dy)) test = dabs(dx + dy)
      if (dabs(grd(ic,jc)).gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The indicated depth is  ',  grd(ic,jc)
        write(*,*) 'This does not appear to be the shoreline'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The indicated depth is  ',  grd(ic,jc)
          write(63,*) 'This does not appear to be the shoreline'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  16.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  16.4 Write data to properties file.
c
      write(63,*) xo, '  landslide longitude'
      write(63,*) yo, '  landslide latitude'
      write(63,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(63,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridcoast
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  17.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 171 j=1,jm
        do 171 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 4.0D+00 * nmax * (w + wid) * delx / vol
          if (((w + wid) / term).lt.delx) then
            term = (w + wid) / delx
            nmax = term * vol / (4.0D+00 * (w + wid) * delx)
          endif
          out(i,j) = nmax *
     +      (2.0D+00 / (dexp(term * y / (w + wid)) +
     +                 dexp(-term * y / (w + wid))))**2 *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
c
c  17.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  171 continue
c
c  17.4 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      term = nmax / d
      do 176 j=1,jm
        do 176 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.5 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          uvel(i,j) =  -utot * sna
c
c  17.6 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  176 continue
c
c  17.7 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 177 j=1,jm
        do 177 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  17.8 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          vvel(i,j) = utot * cna
c
c  17.9 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  177 continue
c
c  17.10 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 173 j=1,jm
        do 173 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  173 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 174 j=1,jm
        do 174 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  174 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 175 j=1,jm
        do 175 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  175 continue
c
c  17.11 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) umin, '  trough u velocity'
      write(63,*) umax, '  peak u velocity'
      write(63,*) vmin, '  trough v velocity'
      write(63,*) vmax, '  peak v velocity'
      write(63,*)
c
c  17.12 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charcoast
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp 
c
c  --------------------------------------------------------------------
c
c  18.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.2D-03 * xmin * 5.0D+00
      do 181 j=0,5000
        x = dfloat(j) * ddx - 2.0D+00 * xmin
        term = 4.0D+00 * nmax * (w + wid) * delx / vol
        if (((w + wid) / term).lt.delx) then
          term = (w + wid) / delx
          nmax = term * vol / (4.0D+00 * (w + wid) * delx)
        endif
        test = nmax *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  181 continue
c
c  18.2 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) 
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridwrite
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  19.1 Write the free surface grid file in the chosen grid format.
c
      write(*,*) 'Writing tsunami grid files'
      write(*,*)
      if (type.eq.0) then
        write(61,208)
        write(61,211) im,jm
        write(61,210) xw,xe
        write(61,210) ys,yn
        write(61,210) fsmin,fsmax
        do 192 j=1,jm
          write(61,209)(out(i,j),i=1,im)
 192    continue
      endif
      close(61)
c
c  19.2 Write the u-velocity grid file in the chosen grid format.
c
      if (fail.ne.0) then
        if (type.eq.0) then
          write(64,208)
          write(64,211) im,jm
          write(64,210) xw,xe
          write(64,210) ys,yn
          write(64,210) umin,umax
          do 194 j=1,jm
            write(64,209)(uvel(i,j),i=1,im)
 194      continue
        endif
        close (64)
      endif
c
c  19.3 Write the v-velocity grid file in the chosen grid format.
c
      if (fail.ne.0) then
        if (type.eq.0) then
          write(65,208)
          write(65,211) im,jm
          write(65,210) xw,xe
          write(65,210) ys,yn
          write(65,210) vmin,vmax
          do 196 j=1,jm
            write(65,209)(vvel(i,j),i=1,im)
 196      continue
        endif
        close (65)
      endif
c
c  19.4 Format statements for the grid
c
  208 format('DSAA')
  209 format(4f15.6)
  210 format(2f20.6)
  211 format(2i20)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setwave
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  20.1 Identify the wave train entrance edge on the grid.  
c
      write(*,*) 'Type 0 for wave train entry at grid top'
      write(*,*) 'Type 1 for wave train entry at grid left'
      write(*,*) 'Type 2 for wave train entry at grid bottom'
      write(*,*) 'Type 3 for wave train entry at grid right'
      read(*,*) itrain
      write(*,*)
      if ((itrain.lt.0).or.(itrain.gt.3)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.2 Identify the wave train number of wavelengths.  
c
      write(*,*) 'Enter the number of complete wavelengths'
      read(*,*) nwave
      write(*,*)
      if (nwave.le.0) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.3 Identify the wave train leading wave polarity.  
c
      write(*,*) 'Type 0 for a leading depression wave'
      write(*,*) 'Type 1 for a leading elevation wave'
      read(*,*) lwave
      if ((lwave.ne.0).and.(lwave.ne.1)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'That was an invalid entry'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  20.4 Identify the wave train propagation direction.  
c
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of wave front in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  20.5 Verify that the angles are reasonable.
c
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (alpha.lt.0.0D+00) alpha = alpha + 3.6D+02
      test = dfloat(itrain) * 90.0D+00 - 90.0D+00
      if (test.lt.-3.6D+02) test = test + 3.6D+02
      if (test.lt.0.0D+00) test = test + 3.6D+02
      if (alpha.gt.(test-10.0D+00)) then
      if (alpha.lt.(test+10.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Wave front is <10 degrees perpendicular to edge'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      endif
      test = dfloat(itrain) * 90.0D+00 - 270.0D+00
      if (test.lt.-3.6D+02) test = test + 3.6D+02
      if (test.lt.0.0D+00) test = test + 3.6D+02
      if (alpha.gt.(test-10.0D+00)) then
      if (alpha.lt.(test+10.0D+00)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'Wave front is <10 degrees perpendicular to edge'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      endif
      if (itrain.eq.0) then
        if ((alpha.ge.9.0D+01).and.(alpha.lt.1.8D+02)) then
          alpha = alpha + 1.8D+02
        endif
        if ((alpha.ge.1.8D+02).and.(alpha.lt.2.7D+02)) then
          alpha = alpha - 1.8D+02
        endif         
      endif
      if (itrain.eq.1) then
        if (alpha.gt.1.8D+02) then
          alpha = alpha - 1.8D+02
        endif 
      endif
      if (itrain.eq.2) then
        if (alpha.lt.9.0D+01) then
          alpha = alpha + 1.8D+02
        endif
        if (alpha.gt.2.7D+02) then
          alpha = alpha - 1.8D+02
        endif 
      endif
      if (itrain.eq.3) then
        if (alpha.lt.1.8D+02) then
          alpha = alpha + 1.8D+02
        endif 
      endif
c
c  20.6 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  20.7 Write data to properties file.
c
      write(63,*) itrain, '  wave train edge index'
      write(63,*) nwave, '  number of wavelengths'
      write(63,*) lwave, '  leading wave polarity'
      write(63,*)
      write(63,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(63,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine wave
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  21.1 Help define the wave train description. 
C
      write(*,*) 'Wave train follows linear theory in Mei (1989)'
      write(*,*)
      write(*,*) 'The wave train has a finite number of waves'
      write(*,*) 'The wave train is entering the domain'
      write(*,*) 'The wave train length is user defined'
      write(*,*)
      write(*,*) 'The depth is a mean depth below waves'
      write(*,*)
      write(*,*) 'The end of the wave train will be at the edge'
      write(*,*) 'The wave train will extend over entire edge'
      write(*,*)
      write(63,*) 'Wave train follows linear theory in Mei (1989)'
      write(63,*)
      write(63,*) 'The wave train has a finite number of waves'
      write(63,*) 'The wave train is entering the domain'
      write(63,*) 'The wave train length is user defined'
      write(63,*)
      write(63,*) 'The depth is a mean depth below waves'
      write(63,*)
      write(63,*) 'The end of the wave train will be at the edge'
      write(63,*) 'The wave train will extend over entire edge'
      write(63,*)
c
c  21.2 You choose the configuration of the wave train. 
c
      write(*,*) 'Enter mean depth below wave train (m)'
      read(*,*) d
      write(*,*) 'Enter maximum wave train amplitude (m)'
      read(*,*) eta
      write(*,*) 'Enter wavelength of a single wave (m)'
      read(*,*) lambda      
      write(*,*) 'Enter period of a single wave (s)'
      read(*,*) to
      write(*,*)
c
c  21.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      eta = dabs(eta)
      lambda = dabs(lambda)
      to = dabs(to)
c
c  21.4 Check that the inputs are physically possible.  
c
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The mean depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (eta.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The amplitude must exist for a wave'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (lambda.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The wavelength cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The period cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  21.5 Calculate all necessary wave properties.
c
      write(*,*) 'Calculating wave train properties'
      write(*,*)
      kappa = 2.0D+00 * pi / lambda
      w = 2.0D+00 * pi / to
      term = kappa * d
      omega = dsqrt(g * kappa * dtanh(term))
      if (itrain.eq.0) then
        dist = dfloat(nwave) * lambda *dabs(cna) * yinc / dy
        shift = lambda *dabs(cna) * yinc / dy
        xo = (xw + xe) / 2.0D+00 
        yo = yn
      endif
      if (itrain.eq.1) then
        dist = dfloat(nwave) * lambda *dabs(sna) * xinc / dx
        shift = lambda *dabs(sna) * xinc / dx
        xo = xw 
        yo = (yn + ys) / 2.0D+00
      endif
      if (itrain.eq.2) then
        dist = dfloat(nwave) * lambda *dabs(cna) * yinc / dy
        shift = lambda *dabs(cna) * yinc / dy
        xo = (xw + xe) / 2.0D+00 
        yo = ys
      endif
      if (itrain.eq.3) then
        dist = dfloat(nwave) * lambda *dabs(sna) * xinc / dx
        shift = lambda *dabs(sna) * xinc / dx
        xo = xe
        yo = (yn + ys) / 2.0D+00
      endif
      if (lwave.eq.0) dphi = 0.0D+00
      if (lwave.eq.1) dphi = pi
c
c  21.6 Compute some tsunami characteristics.
c
      steep = eta / lambda
      linear = eta / d
      shall = lambda / d
      delx = 0.5D+00 * lambda
c
c  21.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  21.8 Check the calculations for accuracy.
c
      if (linear.gt.0.86D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The amplitude is greater than 0.86*depth'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif        
      if (linear.gt.0.2D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The amplitude is greater than 0.2*depth'
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The amplitude is greater than 0.2*depth'
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(omega - w) / (omega + w)
      if (test.gt.0.5D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave period is off by more than 50%'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave period is off by more than 50%'
          write(63,*) 'The design wave should be recomputed'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      if (ic.lt.1) ic = 1
      if (ic.gt.im) ic = im
      if (jc.lt.1) jc = 1
      if (jc.gt.jm) jc = jm
      if (grd(ic,jc).le.0.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The middle of wave train edge on land'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The middle of wave train edge on land'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (test.gt.0.1D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave period is off by more than 10%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave period is off by more than 10%'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if ((dist + shift).ge.dabs(xe - xw)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave train is longer than grid width'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave train is longer than grid width'
          write(63,*) 'The design wave should be recomputed'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif   
      if ((dist + shift).ge.dabs(yn - ys)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave train is longer than grid height'
        write(*,*) 'The design wave should be recomputed'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave train is longer than grid height'
          write(63,*) 'The design wave should be recomputed'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif   
c
c  21.9 Write data to properties file.
c
      write(63,*) d, '  mean depth'
      write(63,*) eta, '  maximum wave amplitude'
      write(63,*) lambda, '  wavelength'
      write(63,*) to, '  wave period'
      write(63,*)
      write(63,*) kappa, '  wave number'
      write(63,*) omega, '  circular frequency'
      write(63,*) w,'  given frequency'
      write(63,*) dphi, '  wave train phase'
      write(63,*)
      write(63,*) xo, '  wave train x origin'
      write(63,*) yo, '  wave train y origin'
      write(63,*) dist, '  penetration distance'
      write(63,*) shift, '  buffer distance'
      write(63,*)
      write(63,*) steep, '  amplitude over wavelength'
      write(63,*) linear, '  amplitude over depth'
      write(63,*) shall, '  wavelength over depth'
      write(63,*) delx, '  trough to peak distance'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridwave
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  22.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 221 j=1,jm
        do 221 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.2 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          out(i,j) = eta * test
c
c  22.3 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          out(i,j) = out(i,j) * test
c
c  22.4 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  221 continue
c
c  22.5 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      do 226 j=1,jm
        do 226 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.6 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          if (sim.eq.0) then
            utot = eta * g * dtanh(term) * test / (d * omega)
          endif
          if (sim.eq.1) then
            utot = eta * g * kappa * dcosh(0.469D+00 * term) * 
     +             test / (omega * dcosh(term))
          endif
          uvel(i,j) =  utot * sna
c
c  22.7 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          uvel(i,j) = uvel(i,j) * test
c
c  22.8 Modify the initial condition based on bathy and topo data.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  226 continue
c
c  22.9 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 227 j=1,jm
        do 227 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  22.10 Evaluate tsunami shape in local x,y coordinates.
c
          test = -dsin(kappa * x + dphi)
          if (sim.eq.0) then
            utot = eta * g * dtanh(term) * test / (d * omega)
          endif
          if (sim.eq.1) then
            utot = eta * g * kappa * dcosh(0.469D+00 * term) * 
     +             test / (omega * dcosh(term))
          endif
          vvel(i,j) = -utot * cna
c
c  22.11 Modify the initial condition based on distance from edge.
c
          test = 1.0D+00
          if (itrain.eq.0) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.1) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.2) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(yyy) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(yyy).gt.dist) test = 0.0D+00
          endif
          if (itrain.eq.3) then
            test = 0.5D+00 - 0.5D+00 * 
     +             dtanh(1.5D+01 * (dabs(xxx) - dist + 
     +                   0.12D+00 * shift) / shift)
            if (dabs(xxx).gt.dist) test = 0.0D+00
          endif
          vvel(i,j) = vvel(i,j) * test
c
c  22.12 Modify the initial condition based on bathy and topo data.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  227 continue
c
c  22.13 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 223 j=1,jm
        do 223 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  223 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 224 j=1,jm
        do 224 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  224 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 225 j=1,jm
        do 225 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  225 continue
c
c  22.14 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) umin, '  trough u velocity'
      write(63,*) umax, '  peak u velocity'
      write(63,*) vmin, '  trough v velocity'
      write(63,*) vmax, '  peak v velocity'
      write(63,*)
c
c  22.15 Write the initial condition grid files.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine setflow
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  23.1 Identify the shoreline position of flow on the grid.  
c
      write(*,*) 'The next coordinate pair is in the grid units'
      write(*,*)
      write(*,*) 'Enter x-axis of flow entry at shoreline xo'
      read(*,*) xo
      write(*,*) 'Enter y-axis of flow entry at shoreline yo'
      read(*,*) yo
      write(*,*) 'The next two angles are measured counterclockwise'
      write(*,*)
      write(*,*) 'Enter CCW angle of north in degrees from grid top'
      read(*,*) alfao
      write(*,*) 'Enter CCW angle of flow in degrees from north'
      read(*,*) alpha
      write(*,*)
c
c  23.2 Verify that the flow is within the grid and underwater.
c
      if ((xo.gt.xe).or.(xo.lt.xw)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of xo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if ((yo.gt.yn).or.(yo.lt.ys)) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of yo is outside the grid'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alpha).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alpha is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (dabs(alfao).gt.3.6D+02) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The value of alfao is beyond 360 degrees'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      ic = nint((xo - xw) / xinc)
      jc = nint((yo - ys) / yinc)
      test = dmax / 1.0D+02
      if (test.lt.dabs(dx + dy)) test = dabs(dx + dy)
      if (dabs(grd(ic,jc)).gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The indicated depth is  ',  grd(ic,jc)
        write(*,*) 'This does not appear to be the shoreline'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The indicated depth is  ',  grd(ic,jc)
          write(63,*) 'This does not appear to be the shoreline'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  23.3 Convert degrees to radians and compute the cosine and sine. 
c
      alpha = alpha * pi / 1.8D+02
      alfao = alfao * pi / 1.8D+02
      cna = dcos(alpha + alfao)
      sna = dsin(alpha + alfao)
c
c  23.4 Write data to properties file.
c
      write(63,*) xo, '  pyroclastic flow longitude'
      write(63,*) yo, '  pyroclastic flow latitude'
      write(63,*) 1.8D+02 * alfao / pi, '  orientation of north'
      write(63,*) 1.8D+02 * alpha / pi, '  orientation from north'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine pyro
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  24.1 Help define the flow and source characteristics. 
C
      write(*,*) 'Source follows Watts and Waythomas (2002)'
      write(*,*)
      write(*,*) 'The positive x-axis is in the flow direction'
      write(*,*) 'The x-axis is also a line of mirror symmetry'
      write(*,*)
      write(*,*) 'Consider submerged motion of pyroclastic flow'
      write(*,*) 'The depth is a final depth of flow deposit'
      write(*,*) 'The velocity provides a check on dynamics'
      write(*,*) 'The width provides a check on geometry'
      write(*,*) 'Two tsunami amplitudes are calculated'
      write(*,*) 'The smallest tsunami amplitude is used'
      write(*,*)
      write(*,*) 'The source will be in front of runout length'
      write(*,*) 'The xo,yo position controls source location'
      write(*,*)
      write(63,*) 'Source follows Watts and Waythomas (2002)'
      write(63,*)
      write(63,*) 'The positive x-axis is in the flow direction'
      write(63,*) 'The x-axis is also a line of mirror symmetry'
      write(63,*)
      write(63,*) 'Consider submerged motion of pyroclastic flow'
      write(63,*) 'The depth is a mean depth of flow deposit'
      write(63,*) 'The velocity provides a check on dynamics'
      write(63,*) 'The width provides a check on geometry'
      write(63,*) 'Two tsunami amplitudes are calculated'
      write(63,*) 'The smallest tsunami amplitude is used'
      write(63,*)
      write(63,*) 'The source will be in front of runout length'
      write(63,*) 'The xo,yo position controls source location'
      write(63,*)
c
c  24.2 You choose the configuration of the pyroclastic flow. 
c
      write(*,*) 'Enter the water depth near end of flow (m)'
      read(*,*) d
      write(*,*) 'Enter submerged volume in format 1.0D+11 (m^3)'
      read(*,*) vol
      write(*,*) 'Enter flow velocity at shoreline (m/s)'
      read(*,*) ut
      write(*,*) 'Enter flow runout length under water (m)'
      read(*,*) so
      write(*,*) 'Enter flow runout time under water (s)'
      read(*,*) to         
      write(*,*) 'Enter the flow width at the shoreline (m)'
      read(*,*) w
      write(*,*)
c
c  24.3 These values are all supposed to be positive.  
c
      d = dabs(d)
      vol = dabs(vol)
      ut = dabs(ut)         
      so = dabs(so)
      to = dabs(to)
      w = dabs(w)
c
c  24.4 Check that the inputs are physically possible.  
c         
      if (to.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout time cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (so.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The runout length cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (d.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The water depth cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (vol.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow volume cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (w.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow width cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
      if (ut.eq.0.0D+00) then
        write(*,*)
        write(*,*) 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
        write(*,*) 'The flow velocity cannot be zero'
        write(*,*) 'Program stopped, press return'
        pause
        read(*,*)
        stop
      endif
c
c  24.5 Compute the pyroclastic flow tsunami characteristics.
c
      write(*,*) 'Calculating pyroclastic flow properties'
      write(*,*)
      ao = -ut / to
      fr = ut / dsqrt(g * d)
      lambda = 0.27D+00 * to * dsqrt(g * d)
      test = 0.86D+00 * d
      etal = 1.32D+00 * d * (vol * 2.0D+00 * ut / 
     +                      (pi * w * so * d * dsqrt(d * g)))**0.68
      etat = 1.32D+00 * d * (vol / 
     +                      (w * to * d * dsqrt(d * g)))**0.68
      xmin = so + lambda
      delx = lambda
c
c  24.6 Choose the smallest amplitude for two measures of time.
c
      if (etal.lt.etat) then
        eta = etal
        qs = vol * 2.0D+00 * ut / (pi * w * so * d * dsqrt(d * g))
        tm = pi * so * dsqrt(g / d) / (2.0D+00 * ut)
      else
        eta = etat
        qs = vol / (w * to * d * dsqrt(d * g))
        tm = to * dsqrt(g / d)
      endif
c
c  24.7 Compare the grid size to the wavelength.
c
      if (itsu.eq.1) call gridcheck
c
c  24.8 Check the calculations for accuracy.
c
      if (eta.gt.test) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The wave amplitude is excessively large'
        write(*,*) 'The amplitude will be decreased to 0.86*depth'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          eta = test
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The wave amplitude is excessively large'
          write(63,*) 'The amplitude will be decreased to 0.86*depth'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * dabs(etal - etat) / (etal + etat)
      if (test.gt.0.4D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The two wave amplitudes differ more than 40%'
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The two wave amplitudes differ more than 40%'
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.gt.4.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is >4  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Froude number is >4  ', fr
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      if (fr.lt.1.0D+00) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'The Froude number is <1  ', fr
        write(*,*) 'This will make the amplitude inaccurate'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'The Froude number is <1  ', fr
          write(63,*) 'This will make the amplitude inaccurate'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 2.0D+00 * to * ut / (so * pi)
      if ((test.lt.0.4D+00).or.(test.gt.2.5D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Runout length and time differ  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = to / (4.5D+00 * dsqrt(1.0D+01 * dsqrt(vol / w) / g))
      if ((test.lt.0.3D+00).or.(test.gt.3.3D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Runout length and time differ  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Runout length and time differ  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
      test = 30.0D+00 * vol**(1.0D+00 / 3.0D+00) / w
      if ((test.lt.0.25D+00).or.(test.gt.4.0D+00)) then
        write(*,*)
        write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        write(*,*) 'Flow width may be unusual  ', test
        write(*,*) 'This will make the results questionable'
        write(*,*) 'Type 0 if you wish to continue'
        read(*,*) quest
        if (quest.eq.0) then
          write(*,*)
          write(63,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(63,*) 'Flow width may be unusual  ', test
          write(63,*) 'This will make the results questionable'
          write(63,*)
        else
          write(*,*) 'Program stopped, press return'
          pause
          read(*,*)
          stop            
        endif
      endif
c
c  24.9 Calculate quantities that determine tsunami shape. 
c
      zmax = eta
      nmax = zmax
      wid = lambda
c
c  24.10 Write data to properties file.
c
      write(63,*) d, '  typical final depth'
      write(63,*) vol, '  submerged flow volume'
      write(63,*) ut, '  impact velocity'
      write(63,*) so, '  runout length'
      write(63,*) to, '  runout time'
      write(63,*) w, '  flow width at shore'
      write(63,*)
      write(63,*) ao, '  underwater deceleration'
      write(63,*) lambda, '  wavelength'
      write(63,*) fr, '  impact Froude number'
      write(63,*) qs, '  nondimensional flux'
      write(63,*) tm, '  nondimensional time'
      write(63,*)
      write(63,*) etal, '  amplitude from runout length'
      write(63,*) etat, '  amplitude from runout time'
      write(63,*) eta, '  characteristic wave amplitude'
      write(63,*) 0.85D+00 * d, '  maximum possible amplitude'
      write(63,*) xmin, '  peak position'
      write(63,*) delx, '  peak width'
      write(63,*) zmax, '  peak argument'
      write(63,*)
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine gridflow
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp
c
c  --------------------------------------------------------------------
c
c  25.1 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the free surface shape'
      write(*,*)
      do 251 j=1,jm
        do 251 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.2 Evaluate tsunami shape in local x,y coordinates.
c
          term = 4.0D+00 * nmax * (w + wid) * delx / vol
          if (((w + wid) / term).lt.delx) then
            term = (w + wid) / delx
            nmax = term * vol / (4.0D+00 * (w + wid) * delx)
          endif
          out(i,j) = nmax *
     +      (2.0D+00 / (dexp(term * y / (w + wid)) +
     +                 dexp(-term * y / (w + wid))))**2 *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
c
c  25.3 Modify the initial condition based on bathy and topo data.
c
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).ge.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = -grd(i,j) * 0.9999D+00
            endif
          endif
          if ((out(i,j).lt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            out(i,j) = 0.0D+00
          endif
          if ((out(i,j).gt.0.0D+00).and.(grd(i,j).le.0.0D+00)) then
            if (-out(i,j).ge.grd(i,j)) then
              out(i,j) = 0.0D+00
            else
              out(i,j) = out(i,j) + grd(i,j)
            endif
          endif
  251 continue
c
c  25.4 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the u velocities'
      write(*,*)
      term = nmax / d
      do 256 j=1,jm
        do 256 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            xxx = xxx + xinc / 2.0D+00
          endif
          if (sim.eq.1) then
            xxx = xxx
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.5 Evaluate tsunami u velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (i.lt.im) then
              fact1 = (out(i+1,j) + out(i,j)) / 2.0D+00
              fact2 = (grd(i+1,j) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i-1,j)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i-1,j)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          uvel(i,j) =  -utot * sna
c
c  25.6 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = uvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            uvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              uvel(i,j) = 0.0D+00
            endif
          endif
  256 continue
c
c  25.7 Calculate the tsunami initial condition for the grid. 
c
      write(*,*) 'Calculating the v velocities'
      write(*,*)
      do 257 j=1,jm
        do 257 i=1,im
          xxx = xw + dfloat(i-1) * xinc - xo
          yyy = ys + dfloat(j-1) * yinc - yo
          if (sim.eq.0) then
            yyy = yyy + yinc / 2.0D+00
          endif
          if (sim.eq.1) then
            yyy = yyy
          endif
          xx = xlon * xxx
          yy = ylat * yyy
          x = yy * cna - xx * sna
          y = xx * cna + yy * sna
c
c  25.8 Evaluate tsunami v velocities in local x,y coordinates.
c
          if (sim.eq.0) then
            if (j.lt.jm) then
              fact1 = (out(i,j+1) + out(i,j)) / 2.0D+00
              fact2 = (grd(i,j+1) + grd(i,j)) / 2.0D+00
            else
              fact1 = (2.0D+00 * out(i,j) - out(i,j-1)) / 2.0D+00
              fact2 = (2.0D+00 * grd(i,j) - grd(i,j-1)) / 2.0D+00
            endif
          endif
          if (sim.eq.1) then
            fact1 = out(i,j)
            fact2 = grd(i,j)
          endif
          if (sim.eq.0) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 - 0.01334D+00 * term) * fact1 / d
     +                 - 0.97999D+00 * (fact1 / d)**2)
          endif
          if (sim.eq.1) then
            utot = dsqrt(g * d) * (1.0D+00 + term / 2.0D+00) * 
     +                  ((1.0D+00 + 0.17006D+00 * term) * fact1 / d
     +                 - 1.25509D+00 * (fact1 / d)**2)
          endif
          vvel(i,j) = utot * cna
c
c  25.9 Modify the initial condition based on bathy and topo data.
c
          if ((fact1.lt.0.0D+00).and.(fact2.ge.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = vvel(i,j) * 1.0D-04
            endif
          endif
          if ((fact1.lt.0.0D+00).and.(fact2.le.0.0D+00)) then
            vvel(i,j) = 0.0D+00
          endif
          if ((fact1.gt.0.0D+00).and.(fact2.le.0.0D+00)) then
            if (-fact1.ge.fact2) then
              vvel(i,j) = 0.0D+00
            endif
          endif
  257 continue
c
c  25.10 Search for the maxima and minima.
c
      write(*,*) 'Finding the free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      do 253 j=1,jm
        do 253 i=1,im
          if (out(i,j).lt.fsmin) then
            fsmin = out(i,j)
          endif
          if (out(i,j).gt.fsmax) then
            fsmax = out(i,j)
          endif
  253 continue
      umax = 0.0D+00
      umin = 0.0D+00
      do 254 j=1,jm
        do 254 i=1,im
          if (uvel(i,j).lt.umin) then
            umin = uvel(i,j)
          endif
          if (uvel(i,j).gt.umax) then
            umax = uvel(i,j)
          endif
  254 continue
      vmax = 0.0D+00
      vmin = 0.0D+00
      do 255 j=1,jm
        do 255 i=1,im
          if (vvel(i,j).lt.vmin) then
            vmin = vvel(i,j)
          endif
          if (vvel(i,j).gt.vmax) then
            vmax = vvel(i,j)
          endif
  255 continue
c
c  25.11 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) umin, '  trough u velocity'
      write(63,*) umax, '  peak u velocity'
      write(63,*) vmin, '  trough v velocity'
      write(63,*) vmax, '  peak v velocity'
      write(63,*)
c
c  25.12 Write the initial condition grid file.
c
      call gridwrite
c
      return
      end
c
c  ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
c
      subroutine charflow
c
      real*8 out,grd,g,so,to,b,d,r,t
      real*8 alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      real*8 xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      real*8 lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      real*8 hao,eta,xmin,delx,zmin,zmax,shift,denom
      real*8 wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      real*8 term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      real*8 leng,high,depth,mw,sing,dip,rake,mu,sharp
      real*8 slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      real*8 area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      real*8 dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      real*8 fact1,fact2,bnd12,bnd23,bnd34,bnd41
      real*8 x12,x34,y12,y34,z12,z34,xp,yp,zp
      real*8 vol,etal,etat,fr,qs,tm,tsrc
      real*8 uvel,vvel,kappa,omega,steep,linear
      real*8 shall,umin,umax,vmin,vmax,utot
c
      integer im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      integer trap,nx,ny,i,j,k,l,grid,type,sim
      integer itrain,lwave,nwave,nsrc,itmp
c
      character*20 input
      character*4 output
      character proper
c
      dimension out(3000,3000), grd(3000,3000)
      dimension uvel(3000,3000), vvel(3000,3000)
c
      common/block1/  out,grd,g,so,to,b,d,r,t 
      common/block2/  alpha,alfao,xo,yo,pi,cna,sna,x,y,xx,yy,xxx,yyy
      common/block3/  xw,xe,ys,yn,xinc,yinc,dmax,dmin,xlon,ylat
      common/block4/  lambda,ao,ut,rhoo,rhob,gamma,fsmin,fsmax
      common/block5/  hao,eta,xmin,delx,zmin,zmax,shift,denom 
      common/block6/  wid,w,dphi,xg,dist,theta,cnt,snt,tnt,dx,dy
      common/block7/  term,dt,test,dep,cut,nmax,nmin,gmo,sg,ddx
      common/block8/  leng,high,depth,mw,sing,dip,rake,mu,sharp
      common/block9/  slip,x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
      common/block10/ area,cdip,sdip,elast,u1var,u2var,xvar,yvar
      common/block11/ dvar,rado,radd,momt,pvar,qvar,i4var,i5var
      common/block12/ fact1,fact2,bnd12,bnd23,bnd34,bnd41
      common/block13/ x12,x34,y12,y34,z12,z34,xp,yp,zp
      common/block14/ vol,etal,etat,fr,qs,tm,tsrc
      common/block15/ im,jm,flag,fail,ic,jc,iden,idep,quest,itsu
      common/block16/ trap,nx,ny,i,j,k,l,grid,type,sim
      common/block17/ input,output,proper 
      common/block18/ uvel,vvel,kappa,omega,steep,linear
      common/block19/ shall,umin,umax,vmin,vmax,utot
      common/block20/ itrain,lwave,nwave,nsrc,itmp 
c
c  --------------------------------------------------------------------
c
c  26.1 Calculate the tsunami minimum and maximum amplitude. 
c
      write(*,*) 'Finding free surface maxima'
      write(*,*)
      fsmax = 0.0D+00
      fsmin = 0.0D+00
      ddx = 0.2D-03 * xmin * 5.0D+00
      do 261 j=0,5000
        x = dfloat(j) * ddx - 2.0D+00 * xmin
        term = 4.0D+00 * nmax * (w + wid) * delx / vol
        if (((w + wid) / term).lt.delx) then
          term = (w + wid) / delx
          nmax = term * vol / (4.0D+00 * (w + wid) * delx)
        endif
        test = nmax *
     +      (2.0D+00 / (dexp((x - xmin) / delx) +
     +                 dexp(-(x - xmin) / delx)))**2
        if (test.lt.fsmin) fsmin = test
        if (test.gt.fsmax) fsmax = test
  261 continue
c
c  26.2 Write data to properties file.
c
      write(63,*) fsmin, '  trough amplitude'
      write(63,*) fsmax, '  peak amplitude'
      write(63,*) 
c
      return
      end



