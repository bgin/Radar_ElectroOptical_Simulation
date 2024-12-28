
module idealized_topog

! <CONTACT EMAIL="Bruce.Wyman@noaa.gov">
!   Bruce Wyman
! </CONTACT>

! <HISTORY SRC="http://www.gfdl.noaa.gov/fms-cgi-bin/cvsweb.cgi/FMS/"/>
  
! <OVERVIEW>
!   Routines for creating Gaussian-shaped land surface topography
!   for latitude-longitude grids.
! </OVERVIEW>

! <DESCRIPTION>
!   Interfaces generate simple Gaussian-shaped mountains from
!   parameters specified by either argument list or namelist input.
!   The mountain shapes are controlled by the height, half-width,
!   and ridge-width parameters.
! </DESCRIPTION>

!! ADDED CODE FOR SINUSOIDAL MOUNTAIN
! <CONTACT EMAIL="lina.boljka@colostate.edu">
!   Lina Boljka
! <CONTACT>
! <OVERVIEW>
!   Routines for creating sinusoidal-shaped land surface topography
!   for latitude-longitude grids.
! </OVERVIEW>
!! MODIFIED BY BERNARD GINGOLD, contact: beniekg@gmail.com
! <DESCRIPTION>
!   Interfaces generate simple sinusoidal-shaped mountains from
!   parameters specified by either argument list or namelist input.
!   The mountain shapes are controlled by the height, latitudinal
!   extent and wavenumber we want. See Gerber and Polvani (2009)
!   and citations thereafter for further details.
!   Some modifications to that setup may be present below.
! </DESCRIPTION>

!use  fms_mod, only: file_exist, open_namelist_file,  &
 !                   check_nml_error, close_file,     &
 !                   stdlog, write_version_number,    &
!                    mpp_pe, mpp_root_pe,             &
 !                   error_mesg, FATAL

!use constants_mod, only: pi
   use mod_kinds, only : i4,sp
   use omp_lib
   implicit none
   private

   public :: gaussian_topog_init, get_gaussian_topog, sinusoidal_topog_init, get_sinusoidal_topog
   public :: compute_idealized_terrain, modify_idealized_terrain
!-----------------------------------------------------------------------
! <NAMELIST NAME="idealized_topog_nml"><USED FOR GAUSSIAN MOUNTAIN>
!   <DATA NAME="height" UNITS="meter" TYPE="real(kind=sp)" DIM="(mxmtns)" DEFAULT="0.">
!     Height in meters of the Gaussian mountains.
!    </DATA>
!   <DATA NAME="olon, olat" UNITS="degree" TYPE="real(kind=sp)" DIM="(mxmtns)" DEFAULT="0.">
!     The longitude and latitude of mountain origins (in degrees).
!    </DATA>
!   <DATA NAME="wlon, wlat" UNITS="degree" TYPE="real(kind=sp)" DIM="(mxmtns)" DEFAULT="0.">
!     The longitude and latitude half-width of mountain tails (in degrees).
!    </DATA>
!   <DATA NAME="rlon, rlat" UNITS="degree" TYPE="real(kind=sp)" DIM="(mxmtns)" DEFAULT="0.">
!     The longitude and latitude half-width of mountain ridges (in degrees).  For a
!     "standard" Gaussian mountain set rlon=rlat=0.
!    </DATA>
!
!    <DATA NAME="NOTE">
!     The variables in this namelist are only used when routine
!     <TT>gaussian_topog_init</TT> is called.  The namelist variables
!     are dimensioned (by 10), so that multiple mountains can be generated.
!
!     Internal parameter mxmtns = 10. By default no mountains are generated.
!    </DATA>
! <NAMELIST NAME="idealized_topog_nml"><USED FOR SINUSOIDAL MOUNTAIN>
!   <DATA NAME="height_sin" UNITS="meter" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     Height in meters of the sinusoidal mountains (h0). Must be positive definitive (height_sin >=0).
!    </DATA>
!   <DATA NAME="lat0, lat1" UNITS="degree" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     latitudinal bounds for the topographic forcing typically 25 and 65degN (in degrees).
!    </DATA>
!   <DATA NAME="m, Amp2" UNITS="n/a" TYPE="int" DIM="(scalar)" DEFAULT="0.">
!     m is wavenumber of the sine wave (m=1 or 2);
!     Amp2 (=1 or 0) allows an addition of wavenumber 2 wave to wavenumber 1 (only an option if m=1).
!     If Amp2 = 0 only one wavenumber wave is present; if Amp2=1 we have wave-1 and wave-2 topography
!     Amp2 can actually also be more than one - but then we are weighting the wave-2 topography more.
!    </DATA>
!   <DATA NAME="uneven_sin" UNITS="n/a" TYPE="bool" DIM="(n/a)" DEFAULT="0.">
!     if .true. it can only be used with A=0 and m=2 - one mountain will then be taller than the other.
!    </DATA>
!   <DATA NAME="uneven_fac" UNITS="n/a" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     if uneven_sin=.true. define how much taller the 2nd mountain is (e.g. uneven_fac = 2. for twice as tall 2nd mountain).
!    </DATA>

#if !defined (IDEALIZED_TOPOG_VERBOSE)
#define IDEALIZED_TOPOG_VERBOSE 0
#endif 

   type, public :: terrain_config_t
         ! ideal_terrain 0 = none 
         ! 1 = X m flat plate 
         ! 2 = 2d witch of agnesi hill in x
         ! 21 = 2d gaussian hill in x
         ! 3 = 3d witch of agnesi hill 
         ! 31 = 3d gaussian hill
         ! 4 = 2d ridge or isolated cube 
         ! 5 = 3d cube
         ! 6 = 2d idealized t-rex valley
         ! 7 = 3d idealized t-rex valley
         ! 8 = v shaped valley and ridges
         ! 81 = sinusoidal valley and ridges RSA
         ! 9 = Schar idealized advection test
         ! 10 = read data (1-d ht array)
         ! 11 = 2d witch of agnesi hill in y
         ! 13 = DJW OKC read from file
         ! 14 = RSA Granite Mountain read from file
         ! 15 = RSA/JB Askervein read from file 
         integer(kind=i4) :: ideal_terrain ! as above
         integer(kind=i4) :: spec_bdy_width
         real(kind=sp)    :: flat_plate_ht ! default height for flat terrain
         real(kind=sp)    :: dx 
         real(kind=sp)    :: dy 
   end type terrain_config_t

   integer(kind=i4), parameter :: maxmts = 10 !originally was set to 10

   real(kind=sp), dimension(maxmts) :: height = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  olon  = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  olat  = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  wlon  = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  wlat  = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  rlon  = 0.0_sp
   real(kind=sp), dimension(maxmts) ::  rlat  = 0.0_sp
   integer(kind=i4) :: m = 1 ! set m =1 as default
   integer(kind=i4) :: Amp2 = 0 ! set Amp2= 0 as default
   logical :: uneven_sin = .false. ! default is to have e.g. 2 mountains with the same height; if .true. they won't be.
   real(kind=sp) :: uneven_fac = 1.0 ! default is 1 - i.e. 2nd mountain same height as first; the factor can be anything though (if e.g. =2 then 2nd mountain is twice as tall as first).
   real(kind=sp) :: height_sin = 0.0 ! default is no mountain - change to e.g. 3000m in namelist!
   real(kind=sp) :: deltalat = 0.0
   !real(kind=sp) :: lat0 = 25.0 ! this will generally be the same - no need for its definition from namelist
   !real(kind=sp) :: lat1 = 65.0 ! -||-

   !namelist /idealized_topog_nml/ height, olon, olat, wlon, wlat, rlon, rlat, &
   !   m, Amp2, height_sin, uneven_sin, uneven_fac, deltalat !, lat0, lat1
! </NAMELIST>

!-----------------------------------------------------------------------

character(len=128) :: version = '$Id: idealized_topog.f90,v 13.0 2006/03/28 21:43:27 fms Exp $'
character(len=128) :: tagname = '$Name: latest $'

!logical :: do_nml = .true.
 logical :: module_is_initialized = .FALSE.

!-----------------------------------------------------------------------

contains

!#######################################################################

! <SUBROUTINE NAME="gaussian_topog_init">

!   <OVERVIEW>
!     Returns a surface height field that consists
!     of the sum of one or more Gaussian-shaped mountains.
!   </OVERVIEW>
!   <DESCRIPTION>
!     Returns a land surface topography that consists of a "set" of
!     simple Gaussian-shaped mountains.  The height, position,
!     width, and elongation of the mountains can be controlled
!     by variables in namelist <LINK SRC="#NAMELIST">&#38;idealized_topog_nml</LINK>.
!   </DESCRIPTION>
!   <TEMPLATE>
!     <B>call gaussian_topog_init</B> ( lon, lat, zsurf )
!   </TEMPLATE>

!   <IN NAME="lon" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box longitude in radians.
!   </IN>
!   <IN NAME="lat" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box latitude in radians.
!   </IN>
!   <OUT NAME="zsurf" UNITS="meter" TYPE="real(kind=sp)" DIM="(:,:)">
!     The surface height (in meters).
!     The size of this field must be size(lon) by size(lat).
!   </OUT>

subroutine gaussian_topog_init ( lon, lat, zsurf )

real(kind=sp), intent(in)  :: lon(:), lat(:)
real(kind=sp), intent(out) :: zsurf(:,:)

integer(kind=i4) :: n

  !if (.not.module_is_initialized) then
  !   call write_version_number( version, tagname )
  !endif

  if(any(shape(zsurf) /= (/size(lon(:)),size(lat(:))/))) then
     print*, 'shape(zsurf) is not equal to (/size(lon),size(lat)/)'
     return
    !call error_mesg ('get_gaussian_topog in topography_mod', &
    ! 'shape(zsurf) is not equal to (/size(lon),size(lat)/)', FATAL)
  endif

  !if (do_nml) call read_namelist

! compute sum of all non-zero mountains
  !zsurf(:,:) = 0._sp
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ ASSUME_ALIGNED lon:64
    !DIR$ ASSUME_ALIGNED lat:64
    !DIR$ ASSUME_ALIGNED zsurf:64
#endif
  !$OMP PARALLEL DO SCHEDULE(STATIC,8) DEFAULT(NONE) &
  !$OMP& PRIVATE(n) SHARED(maxmts,zsurf,lon,lat,height,olon,olat,wlon,wlat,rlon,rlat)
  do n = 1, maxmts
    if ( height(n) == 0._sp) cycle
    zsurf = zsurf + get_gaussian_topog ( lon, lat, height(n), &
                olon(n), olat(n), wlon(n), wlat(n), rlon(n), rlat(n))
  enddo
  !$OMP END PARALLEL DO
 module_is_initialized = .TRUE.

end subroutine gaussian_topog_init
! </SUBROUTINE>

!#######################################################################

! <FUNCTION NAME="get_gaussian_topog">

!   <OVERVIEW>
!     Returns a simple surface height field that consists of a single
!     Gaussian-shaped mountain.
!   </OVERVIEW>
!   <DESCRIPTION>
!     Returns a single Gaussian-shaped mountain.
!     The height, position, width, and elongation of the mountain
!     is controlled by optional arguments.
!   </DESCRIPTION>
!   <TEMPLATE>
!     zsurf = <B>get_gaussian_topog</B> ( lon, lat, height
!                    [, olond, olatd, wlond, wlatd, rlond, rlatd ] )
!   </TEMPLATE>

!   <IN NAME="lon" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box longitude in radians.
!   </IN>
!   <IN NAME="lat" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box latitude in radians.
!   </IN>
!   <IN NAME="height" UNITS="meter" TYPE="real(kind=sp)" DIM="(scalar)">
!     Maximum surface height in meters.
!   </IN>
!   <IN NAME="olond, olatd" UNITS="degrees" TYPE="real(kind=sp)" DIM="(scalar)">
!     Position/origin of mountain in degrees longitude and latitude.
!     This is the location of the maximum height.
!   </IN>
!   <IN NAME="wlond, wlatd" UNITS="degrees" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="15.">
!     Gaussian half-width of mountain in degrees longitude and latitude.
!   </IN>
!   <IN NAME="rlond, rlatd" UNITS="degrees" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     Ridge half-width of mountain in degrees longitude and latitude.
!                    This is the elongation of the maximum height.
!   </IN>
!   <OUT NAME="zsurf" UNITS="meter" TYPE="real(kind=sp)" DIM="(:,:)">
!     The surface height (in meters).
!              The size of the returned field is size(lon) by size(lat).
!   </OUT>
!   <ERROR MSG="shape(zsurf) is not equal to (/size(lon),size(lat)/)" STATUS="FATAL">
!     Check the input grid size and output field size.
!     The input grid is defined at the midpoint of grid boxes.
!   </ERROR>
!   <NOTE>
!     Mountains do not wrap around the poles.
!   </NOTE>

function get_gaussian_topog ( lon, lat, height,                          &
                              olond, olatd, wlond, wlatd, rlond, rlatd ) &
                     result ( zsurf )
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: get_gaussian_topog
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: get_gaussian_topog
#endif
real(kind=sp), intent(in)  :: lon(:), lat(:)

real(kind=sp), intent(in)  :: height
real(kind=sp), intent(in), optional :: olond, olatd, wlond, wlatd, rlond, rlatd
real(kind=sp) :: zsurf(size(lon,1),size(lat,1))

integer(kind=i4) :: i, j
real(kind=sp)    :: olon, olat, wlon, wlat, rlon, rlat
real(kind=sp)    :: tpi, dtr, dx, dy, xx, yy

  !if (do_nml) call read_namelist

! no need to compute mountain if height=0
  if ( height == 0._sp ) then
       !zsurf(:,:) = 0._sp
       return
  endif

  tpi = 6.2831853071795864769253_sp !2.0_sp*pi
  dtr = 0.0174532925199432957692_sp !tpi/360.

! defaults and convert degrees to radians (dtr)
  olon = 90._sp*dtr;  if (present(olond)) olon=olond*dtr
  olat = 45._sp*dtr;  if (present(olatd)) olat=olatd*dtr
  wlon = 15._sp*dtr;  if (present(wlond)) wlon=wlond*dtr
  wlat = 15._sp*dtr;  if (present(wlatd)) wlat=wlatd*dtr
  rlon =  0._sp    ;  if (present(rlond)) rlon=rlond*dtr
  rlat =  0._sp    ;  if (present(rlatd)) rlat=rlatd*dtr

! compute gaussian-shaped mountain
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ ASSUME_ALIGNED lon:64
    !DIR$ ASSUME_ALIGNED lat:64
#endif
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ ASSUME_ALIGNED zsurf:64
#endif
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ CODE_ALIGN(32)
    !DIR$ PREFETCH lat:0:4
    !DIR$ PREFETCH lat:1:16
    !DIR$ PREFETCH lon:0:4
    !DIR$ PREFETCH lat:1:16
#endif
    do j=1,size(lat(:))
      dy = abs(lat(j) - olat)   ! dist from y origin
      yy = max(0._sp, dy-rlat)/wlat
      !$OMP SIMD LINEAR(i:1) UNROLL PARTIAL(4)
      do i=1,size(lon(:))
        dx = abs(lon(i) - olon) ! dist from x origin
        dx = min(dx, abs(dx-tpi))  ! To ensure that: -pi <= dx <= pi
        xx = max(0._sp, dx-rlon)/wlon
        zsurf(i,j) = height*exp(-xx*xx - yy*yy)
      enddo
    enddo

end function get_gaussian_topog
! </FUNCTION>


!#######################################################################

! <SUBROUTINE NAME="sinusoidal_topog_init">

!   <OVERVIEW>
!     Returns a surface height field that consists
!     of the sum of one or two sinusoidal mountains as specified by namelist.
!   </OVERVIEW>
!   <DESCRIPTION>
!     Returns a land surface topography that consists of a "set" of
!     simple sinusoidal-shaped mountains.  The height, position/width,
!     and number of mountains can be controlled by variables in namelist.
!   </DESCRIPTION>
!   <TEMPLATE>
!     <B>call sinusoidal_topog_init</B> ( lon, lat, zsurf )
!   </TEMPLATE>

!   <IN NAME="lon" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box longitude in radians.
!   </IN>
!   <IN NAME="lat" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box latitude in radians.
!   </IN>
!   <OUT NAME="zsurf" UNITS="meter" TYPE="real(kind=sp)" DIM="(:,:)">
!     The surface height (in meters).
!     The size of this field must be size(lon) by size(lat).
!   </OUT>

subroutine sinusoidal_topog_init ( lon, lat, zsurf )
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: sinusoidal_topog_init
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: sinusoidal_topog_init
#endif
real(kind=sp), intent(in)  :: lon(:), lat(:)

real(kind=sp), intent(out) :: zsurf(:,:)

  
  !if (.not.module_is_initialized) then
  !   call write_version_number( version, tagname )
  !endif

  if(any(shape(zsurf) /= (/size(lon(:)),size(lat(:))/))) then
     print*,'shape(zsurf) is not equal to (/size(lon),size(lat)/)'
     return
    !call error_mesg ('get_sinusoidal_topog in topography_mod', &
     !'shape(zsurf) is not equal to (/size(lon),size(lat)/)', FATAL)
  endif

  !if (do_nml) call read_namelist

  ! Get mountains only if height_sin is not set to zero.
  !zsurf(:,:) = 0.0
  
     if ( height_sin > 0.0_sp ) then
          zsurf = zsurf + get_sinusoidal_topog ( lon, lat, height_sin, &
                    m, Amp2, uneven_sin, uneven_fac, deltalat ) !, lat0, lat1 )
     endif
  
  module_is_initialized = .TRUE.

end subroutine sinusoidal_topog_init
! </SUBROUTINE>

!#######################################################################

! <FUNCTION NAME="get_sinusoidal_topog">

!   <OVERVIEW>
!     Returns a simple surface height field that consists of a sinusoidal-shaped wave-1 and/or wave-2 mountain.
!   </OVERVIEW>
!   <DESCRIPTION>
!     Returns a single sinusoidal-shaped mountain.
!     The height, position/width, number of mountains etc. can be controlled by variables in namelist.
!   </DESCRIPTION>
!   <TEMPLATE>
!     zsurf = <B>get_sinusoidal_topog</B> ( lon, lat, height_sin,
!                     lat0, lat1, m, Amp2, uneven_sin, uneven_fac  )
!   </TEMPLATE>

!   <IN NAME="lon" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box longitude in radians.
!   </IN>
!   <IN NAME="lat" UNITS="radians" TYPE="real(kind=sp)" DIM="(:)">
!     The mean grid box latitude in radians.
!   </IN>
!   <IN NAME="height_sin" UNITS="meter" TYPE="real(kind=sp)" DIM="(scalar)">
!     Maximum surface height in meters.
!   </IN>
!   <IN NAME="lat0, lat1, deltalat" UNITS="degree" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     latitudinal bounds for the topographic forcing typically 25 and 65degN (in degrees).
!     deltalat is used to modify lat0,lat1 (move mountain latitudinally). (e.g.
!     move for 10deg lat north/south with +/-10.); default should be deltalat=0.
!    </IN>
!   <IN NAME="m, Amp2" UNITS="n/a" TYPE="int" DIM="(scalar)" DEFAULT="0.">
!     m is wavenumber of the sine wave (m=1 or 2);
!     Amp2 (=1 or 0) allows an addition of wavenumber 2 wave to wavenumber 1 (only an option if m=1).
!     If Amp2 = 0 only one wavenumber wave is present; if Amp2=1 we have wave-1 and wave-2 topography
!     Amp2 can actually also be more than one - but then we are weighting the wave-2 topography more.
!    </IN>
!   <IN NAME="uneven_sin" UNITS="n/a" TYPE="bool" DIM="n/a" DEFAULT="0.">
!     if .true. it can only be used with A=0 and m=2 - one mountain will then be taller than the other.
!    </IN>
!   <IN NAME="uneven_fac" UNITS="n/a" TYPE="real(kind=sp)" DIM="(scalar)" DEFAULT="0.">
!     if uneven_sin=.true. define how much taller the 2nd mountain is (e.g. uneven_fac = 2. for twice as tall 2nd mountain).
!    </IN>
!   <OUT NAME="zsurf" UNITS="meter" TYPE="real(kind=sp)" DIM="(:,:)">
!     The surface height (in meters).
!              The size of the returned field is size(lon) by size(lat).
!   </OUT>
!   <ERROR MSG="shape(zsurf) is not equal to (/size(lon),size(lat)/)" STATUS="FATAL">
!     Check the input grid size and output field size.
!     The input grid is defined at the midpoint of grid boxes.
!   </ERROR>
!   <NOTE>
!     Mountains do not wrap around the poles.
!   </NOTE>

function get_sinusoidal_topog ( lon, lat, height_sin, m, Amp2, uneven_sin, uneven_fac,deltalat ) & !, lat00, lat11) &
                     result ( zsurf )
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: get_sinusoidal_topog
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: get_sinusoidal_topog
#endif
real(kind=sp), intent(in)  :: lon(:), lat(:)

real(kind=sp), intent(in)  :: height_sin
real(kind=sp), intent(in) :: m, Amp2
logical, intent(in) :: uneven_sin
real(kind=sp), intent(in) :: uneven_fac, deltalat !, lat00, lat11
real(kind=sp) :: zsurf(size(lon,1),size(lat,1))

real(kind=sp), parameter :: pi   = 3.1415926535897932384626_sp
real(kind=sp), parameter :: tpi4 = 2.356194490192344928847_sp
real(kind=sp), parameter :: spi4 = 5.4977871437821381673096_sp
integer(kind=i4) :: i, j
real(kind=sp)    :: tpi, dtr, lat00, lat11

  !if (do_nml) call read_namelist

! no need to compute mountain if height_sin=0
  if ( height_sin == 0. ) then
       !zsurf(:,:) = 0.
       return
  endif

  tpi = 6.2831853071795864769253_sp !2.0*pi
  dtr = 0.0174532925199432957692_sp !tpi/360.

  lat00 = 25._sp + deltalat
  lat11 = 65._sp + deltalat

  lat00 = dtr * lat00
  lat11 = dtr * lat11
  zsurf(:,:) = 0._sp
! compute sinusoidal-shaped mountain
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ ASSUME_ALIGNED lon:64
    !DIR$ ASSUME_ALIGNED lat:64
#endif
#if defined(__INTEL_COMPILER) || defined(__ICC)
    !DIR$ ASSUME_ALIGNED zsurf:64
#endif
  do j=1,size(lat(:))
      !$OMP SIMD LINEAR(i:1) UNROLL PARTIAL(4)
     do i=1,size(lon(:))
        if (lat(j)>lat00 .and. lat(j)<lat11) then
          zsurf(i,j) = height_sin * sin(((lat(j)-lat00) * pi) / (lat11-lat00)) * sin(((lat(j)-lat00) * pi) / (lat11-lat00)) * (cos(m*lon(i)) + (Amp2 * cos(2*lon(i))))
          if (uneven_sin ) then
            ! uneven_sig = .true.; one mountain taller than the other by uneven_fac - half of domain taller;
	    ! this can only be used for Amp2=0; m=2; and height_sin > 0.
            if (lon(i)>tpi4 .and. lon(i)<spi4) then
              zsurf(i,j) = zsurf(i,j) * uneven_fac
            endif
          endif
        !else
        !  zsurf(i,j) = 0.
        endif
     enddo
  enddo

end function get_sinusoidal_topog
! </FUNCTION>

!#######################################################################

!subroutine read_namelist
!
!   integer(kind=i4) :: unit, ierr, io
!   real(kind=sp)    :: dtr

!  read namelist

!  if ( file_exist('input.nml')) then
!      unit = open_namelist_file ( )
!      ierr=1; do while (ierr /= 0)
!         read  (unit, nml=idealized_topog_nml, iostat=io, end=10)
!         ierr = check_nml_error(io,'idealized_topog_nml')
!      enddo
! 10   call close_file (unit)
!   endif

!  write version and namelist to log file

 !  if (mpp_pe() == mpp_root_pe()) then
! !     unit = stdlog()
 !     write (unit, nml=idealized_topog_nml)
 !  endif

 !  do_nml = .false.

!end subroutine read_namelist

!#######################################################################

! Based upon `wrf_ibm` model.

#if 0
WRF Model Version 3.8.1 (August 12, 2016)
http://wrf-model.org/users/users.php

------------------------
WRF PUBLIC DOMAIN NOTICE
------------------------

WRF was developed at the National Center for Atmospheric Research
(NCAR) which is operated by the University Corporation for
Atmospheric Research (UCAR). NCAR and UCAR make no proprietary
claims, either statutory or otherwise, to this version and
release of WRF and consider WRF to be in the public domain for
use by any person or entity for any purpose without any fee or
charge. UCAR requests that any WRF user include this notice on
any partial or full copies of WRF. WRF is provided on an "AS
IS" basis and any warranties, either express or implied,
including but not limited to implied warranties of
non-infringement, originality, merchantability and fitness for a
particular purpose, are disclaimed. In no event shall
UCAR be liable for any damages, whatsoever, whether direct,
indirect, consequential or special, that arise out of or in
connection with the access, use or performance of WRF, including
infringement actions.
#endif 

SUBROUTINE compute_idealized_terrain ( config_flags,                 &
                                       ibm_ht_u, ibm_ht_v,           &
                                       ibm_ht_w, ibm_ht_c,           &
                                       ibm_z0,                       &
                                       ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte )
  IMPLICIT NONE
  !input data
  TYPE(terrain_config_t), INTENT(IN   )               :: config_flags
  REAL(kind=sp), DIMENSION(ims:ime,jms:jme), INTENT(  OUT)     :: ibm_ht_u, &
                                                         ibm_ht_v, &
                                                         ibm_ht_w, &
                                                         ibm_ht_c, &
                                                         ibm_z0
  INTEGER(kind=i4), INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, & !d: domain 
                            ims, ime, jms, jme, kms, kme, & !m:memory 
                            its, ite, jts, jte, kts, kte    !p:patch t:tile 
   
  !local data   
  INTEGER(kind=i4)                                            :: i, j, k, &
                                                         icm,jcm   !mountain position in domain
  REAL(kind=sp)                                                :: hm, xa,  & !mountain height, mountain half width
                                                         offset,  & !offset to make mountain terrain higher than 2nd grid pt.
                                                         slope,   & !RSA
                                                         xa1,     &
                                                         x_dist, y_dist, &
                                                         pi,      &
                                                         ru, rv, rw, rc, du, dv, dw, dc, dmid !DJW added for smoothing to zero near edges
  INTEGER(kind=i4)                                            :: width, ista, iend, jsta, jend !DJW added for putting ridges on terrain
  REAL(kind=sp)                                                :: height !DJW added for putting ridges on terrain
  char(*), parameter :: sub_name = 'compute_idealized_terrain'
!---------------------------------------------------------------------------------
! this subroutine calculates the terrain height and is called once from start_em.F
! The terrain height has twice the resolution of the computational grid
! the terrain height is assigned on u,v,and w points.  It is also assigned at corners.
! Corners are located between v points in the x direction and between u points in the 
! y direction.
! *************Modified by Bernard Gingold (08:48AM, 28 DEC 2024) to fit the REOS project, contact: beniekg@gmail.com*************
! the executable begins here
  
!---------------------------------------------------------------------------------   

  pi = 2.*asin(1.0)

! ideal_terrain 0 = none 
! 1 = X m flat plate 
! 2 = 2d witch of agnesi hill in x
! 21 = 2d gaussian hill in x
! 3 = 3d witch of agnesi hill 
! 31 = 3d gaussian hill
! 4 = 2d ridge or isolated cube 
! 5 = 3d cube
! 6 = 2d idealized t-rex valley
! 7 = 3d idealized t-rex valley
! 8 = v shaped valley and ridges
! 81 = sinusoidal valley and ridges RSA
! 9 = Schar idealized advection test
! 10 = read data (1-d ht array)
! 11 = 2d witch of agnesi hill in y
! 13 = DJW OKC read from file
! 14 = RSA Granite Mountain read from file
! 15 = RSA/JB Askervein read from file

  IF (config_flags%ideal_terrain .EQ. 1) THEN    
    DO j=jts,jte
    DO i=its,ite
        IF (j .NE. jde) ibm_ht_u(i,j) = config_flags%flat_plate_ht
        IF (i .NE. ide) ibm_ht_v(i,j) = config_flags%flat_plate_ht
        IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = config_flags%flat_plate_ht
        ibm_ht_c(i,j) = config_flags%flat_plate_ht
    ENDDO
    ENDDO

  ELSEIF (config_flags%ideal_terrain .EQ. 111) THEN
     !DJW a flat plate with added bumps to trip turbulence
     DO j=jts,jte
     DO i=its,ite
        IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
        IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
        IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
        ibm_ht_c(i,j) = 0.0
     ENDDO
     ENDDO
     height = 6.0
     width = 6
#if (IDEALIZED_TOPOG_VERBOSE) == 1
     write(*,'(A,I3,A,F5.1)') "REOS[compute_idealized_terrain]: adding a cross to the flat plate with width=",width," and height=",height
#endif
     ista = (ide-ids)/2+ids-width
     iend = (ide-ids)/2+ids+width
     DO i=its,ite
        IF ((i .GE. ista) .AND. (i .LE. iend)) THEN
           DO j=jts,jte
              IF (j .NE. jde) ibm_ht_u(i,j) = height
              IF (i .NE. ide) ibm_ht_v(i,j) = height
              IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = height
              ibm_ht_c(i,j) = height
           ENDDO
        ENDIF
     ENDDO
     jsta = (jde-jds)/2+jds-width
     jend = (jde-jds)/2+jds+width
     DO j=jts,jte
        IF ((j .GE. jsta) .AND. (j .LE. jend)) THEN
           DO i=its,ite
              IF (j .NE. jde) ibm_ht_u(i,j) = height
              IF (i .NE. ide) ibm_ht_v(i,j) = height
              IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = height
              ibm_ht_c(i,j) = height
           ENDDO
        ENDIF
     ENDDO
     DO i=its,ite
        DO j=jts,jte
           IF ((i .LT. ids+2*config_flags%spec_bdy_width) .OR. &
               (i .GE. ide-2*config_flags%spec_bdy_width) .OR. &
               (j .LT. jds+2*config_flags%spec_bdy_width) .OR. &
               (j .GE. jde-2*config_flags%spec_bdy_width)) THEN
              IF (i .NE. ide) ibm_ht_u(i,j) = 0.0
              IF (j .NE. jde) ibm_ht_v(i,j) = 0.0
              IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
              ibm_ht_c(i,j) = 0.0
           ENDIF
        ENDDO
     ENDDO

!#if (IDEALIZED_TOPOG_VERBOSE) == 1
!   write(*,'(A,I3,A,F5.1)') "REOS[compute_idealized_terrain]: adding bumps to the flat plate with width=",width," and height=",height
!#endif 
!     ista = (ide-ids)/2+ids-width
!     iend = (ide-ids)/2+ids+width
!     DO i=its,ite
!        IF ((i .GE. ista) .AND. (i .LE. iend)) THEN
!           DO j=jts,jte
!              jsta = width*(j/width)
!              jend = jsta+width
!              IF ((j .NE. jde) .AND. (i .NE. ista) .AND. (j .NE. jsta)) THEN
!                 ibm_ht_u(i,j) = height*(1.0-ABS((ide-ids)/2.0+ids-(i-0.5))/width)*(1.0-ABS(jsta+width/2.0-j)/(width/2.0))
!              ENDIF
!              IF ((i .NE. ide) .AND. (i .NE. ista) .AND. (j .NE. jsta)) THEN
!                 ibm_ht_v(i,j) = height*(1.0-ABS((ide-ids)/2.0+ids-i)/width)*(1.0-ABS(jsta+width/2.0-(j-0.5))/(width/2.0))
!              ENDIF
!              IF ((j .NE. jde) .AND. (i .NE. ista)) THEN
!                 ibm_ht_w(i,j) = height*(1.0-ABS((ide-ids)/2.0+ids-i)/width)*(1.0-ABS(jsta+width/2.0-j)/(width/2.0))
!              ENDIF
!              ibm_ht_c(i,j) = height*(1.0-ABS((ide-ids)/2.0+ids-(i-0.5))/width)*(1.0-ABS(jsta+width/2.0-(j-0.5))/(width/2.0))
!           ENDDO
!        ENDIF
!     ENDDO
!     DO i=its,ite
!        DO j=jts,jte
!           IF ((i .LT. ids+config_flags%spec_bdy_width) .OR. &
!               (i .GE. ide-config_flags%spec_bdy_width) .OR. &
!               (j .LT. jds+config_flags%spec_bdy_width) .OR. &
!               (j .GE. jde-config_flags%spec_bdy_width)) THEN
!              IF (i .NE. ide) ibm_ht_u(i,j) = 0.0
!              IF (j .NE. jde) ibm_ht_v(i,j) = 0.0
!              IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
!              ibm_ht_c(i,j) = 0.0
!           ENDIF
!        ENDDO
!     ENDDO

  ELSEIF (config_flags%ideal_terrain .EQ. 112) THEN    
    DO j=jts,jte
    DO i=its,ite
       IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
       IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
       IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
       ibm_ht_c(i,j) = 0.0
    ENDDO
    ENDDO

    DO j=jts,jte
    DO i=its,ite
       IF ( (MOD(i/10, 2) .EQ. 1) .AND. (MOD(j/10, 2) .EQ. 1) ) THEN
          IF (j .NE. jde) ibm_ht_u(i,j) = 25.0
          IF (i .NE. ide) ibm_ht_v(i,j) = 25.0
          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 25.0
          ibm_ht_c(i,j) = 25.0
       ENDIF
    ENDDO
    ENDDO

    DO j=jts,jte
    DO i=its,ite
!       IF ( ((i .GE. 10*(30/10)) .AND. (i .LE. ide-10*(30/10))) .AND. &
!            ((j .GE. 10*(34/10)) .AND. (j .LE. jde-10*(34/10))) ) THEN
!          IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
!          IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
!          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
!          ibm_ht_c(i,j) = 0.0
!       ENDIF
       IF ( (i .LE. 5) .OR. (i .GE. 10*ide/10-6 ) .OR. &
            (j .LE. 5) .OR. (j .GE. 10*jde/10-6 ) ) THEN
          IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
          IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
          ibm_ht_c(i,j) = 0.0
       ENDIF
    ENDDO
    ENDDO    

  ELSEIF (config_flags%ideal_terrain .EQ. 113) THEN    
    DO j=jts,jte
    DO i=its,ite
       IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
       IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
       IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
       ibm_ht_c(i,j) = 0.0
    ENDDO
    ENDDO

    DO j=jts,jte
    DO i=its,ite
       IF ( (MOD(i/10, 2) .EQ. 1) .AND. (MOD(j/10, 2) .EQ. 1) ) THEN
          IF (j .NE. jde) ibm_ht_u(i,j) = 25.0
          IF (i .NE. ide) ibm_ht_v(i,j) = 25.0
          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 25.0
          ibm_ht_c(i,j) = 25.0
       ENDIF
    ENDDO
    ENDDO

   

    DO j=jts,jte
    DO i=its,ite
!       IF ( ((i .GE. 10*(30/10)) .AND. (i .LE. ide-10*(30/10))) .AND. &
!            ((j .GE. 10*(34/10)) .AND. (j .LE. jde-10*(34/10))) ) THEN
!          IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
!          IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
!          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
!          ibm_ht_c(i,j) = 0.0
!       ENDIF
       IF ( (i .LE. 5) .OR. (i .GE. 10*ide/10-6 ) .OR. &
            (j .LE. 5) .OR. (j .GE. 10*jde/10-6 ) ) THEN
          IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
          IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
          IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
          ibm_ht_c(i,j) = 0.0
       ENDIF
    ENDDO
    ENDDO    

  ELSEIF ((config_flags%ideal_terrain .EQ. 2) .OR. &
          (config_flags%ideal_terrain .EQ. 3) .OR. &
          (config_flags%ideal_terrain .EQ. 33) .OR. &
          (config_flags%ideal_terrain .EQ. 11)) THEN
     !set the dimensions for the gaussian shaped hill 

     ! xa = 3000.0 
     ! 10 degree slope
     ! hm = 830.0   
     ! offset = -32.0
     ! 30 degree slope
     ! hm = 2720.0
     ! offset = -108.0
     ! 50 degree slope
     ! hm = 5615.0
     ! offset = -223.0

     !xa = 200.0
     !hm = 100.0 ! was 75. for 3d hill cases
     !offset = 0.0

     ! 10 degree slope
     ! hm = 218.0
     ! offset = -14.0
     ! 20 degree slope
     ! hm = 451.0
     ! offset = -30.0
     ! 30 degree slope
     !    hm = 716.0
     !   offset = -25.0
     ! 40 degree slope
     ! hm = 1041.0
     ! offset = -71.0
     !xa = 1900.0
     !hm = 2300.0
     !offset = 0.0

     ! for Zangl 2003/2004 case
!     hm = 1500.0
!     xa = 5000.0
     ! for more severe Zangl type atmosphere at rest case
!     hm = 4554.0
!     xa = 5000.0

!     xa = 90.0
!     hm = 180.0
!     offset = 0.0

!     xa = 60.0
!     hm = 30.0
!     offset = 0.0

     !RSA for Lundquist et al. (2012) case
!     hm = 350.0
!     xa = 800.0
!     offset = 0.0

!     hm = 50.0
!     xa = 25.0
!     offset = 0.0

     !DJW for grid resolution study 2019
     hm = 250.0
     xa = 800.0
     offset = 0.0

     icm = ide/2
     jcm = jde/2
     IF (config_flags%ideal_terrain .EQ. 2) THEN
        DO j=jts,jte
        DO i=its,ite

           !! the height function at u and v levels could be specified by with an 
           !! equation or by averaging.  These are the equations.
           !if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+((float(i-icm)-0.5)/xa)**2)
           !if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+(float(i-icm)/xa)**2)   
           !if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm/(1.+(float(i-icm)/xa)**2)
           !ibm_ht_c(i,j) = offset + hm/(1.+((float(i-icm)-0.5)/xa)**2)

           ! hill in the y direction
           ! if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+(float(j-jcm)/xa)**2)
           ! if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+((float(j-jcm)-0.5)/xa)**2)
           ! ibm_ht_w(i,j) = offset + hm/(1.+(float(j-jcm)/xa)**2)
           ! ibm_ht_c(i,j) = offset + hm/(1.+((float(j-jcm)-0.5)/xa)**2)

           ! hill in the x direction
           x_dist = (-ide/2.)*config_flags%dx + i*config_flags%dx
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           x_dist = (-ide/2.)*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
           if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           ibm_ht_c(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           ! hill in the y direction
           ! y_dist = (-jde/2.)*config_flags%dy + j*config_flags%dy
           ! if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm/(1.+(y_dist/xa)**2)
           ! if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+(y_dist/xa)**2)
           ! y_dist = (-jde/2.)*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
           ! if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+(y_dist/xa)**2)
           ! ibm_ht_c(i,j) = offset + hm/(1.+(y_dist/xa)**2)

        ENDDO
        ENDDO
     ELSEIF (config_flags%ideal_terrain .EQ. 11) THEN
        DO j=jts,jte
        DO i=its,ite

           !! the height function at u and v levels could be specified by with an 
           !! equation or by averaging.  These are the equations.
           !if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+((float(i-icm)-0.5)/xa)**2)
           !if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+(float(i-icm)/xa)**2)   
           !if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm/(1.+(float(i-icm)/xa)**2)
           !ibm_ht_c(i,j) = offset + hm/(1.+((float(i-icm)-0.5)/xa)**2)

           ! hill in the y direction
           ! if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+(float(j-jcm)/xa)**2)
           ! if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+((float(j-jcm)-0.5)/xa)**2)
           ! ibm_ht_w(i,j) = offset + hm/(1.+(float(j-jcm)/xa)**2)
           ! ibm_ht_c(i,j) = offset + hm/(1.+((float(j-jcm)-0.5)/xa)**2)

           x_dist = (-jde/2.)*config_flags%dy + j*config_flags%dy
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           if (j /= jde) ibm_ht_u(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           x_dist = (-jde/2.)*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
           if (i /= ide) ibm_ht_v(i,j) = offset + hm/(1.+(x_dist/xa)**2)
           ibm_ht_c(i,j) = offset + hm/(1.+(x_dist/xa)**2)

        ENDDO
        ENDDO
     ELSEIF ((config_flags%ideal_terrain .EQ. 3) .OR. (config_flags%ideal_terrain .EQ. 33)) THEN
        DO j=jts,jte
        DO i=its,ite
           ! use this for gaussian bump
           !if (j /= jde) ibm_ht_u(i,j) = hm/(1.+((float(i-icm)-0.5)/xa)**2+(float(j-jcm)/xa)**2)
           !if (i /= ide) ibm_ht_v(i,j) = hm/(1.+(float(i-icm)/xa)**2+((float(j-jcm)-0.5)/xa)**2) 
           !if ((i /= ide) .AND. (j /= jde)) then
           !    ibm_ht_w(i,j) = hm/(1.+(float(i-icm)/xa)**2+(float(j-jcm)/xa)**2)
           !endif 
           !ibm_ht_c(i,j) = hm/(1.+((float(i-icm)-0.5)/xa)**2+((float(j-jcm)-0.5)/xa)**2)

           x_dist = (-ide/2.0)*config_flags%dx + i*config_flags%dx
           y_dist = (-jde/2.0)*config_flags%dy + j*config_flags%dy
           IF ((i .NE. ide) .AND. (j .NE. jde)) THEN
              ibm_ht_w(i,j) = offset + hm/(1.0+(x_dist/xa)**2+(y_dist/xa)**2)
           ENDIF

           x_dist = (-ide/2.0)*config_flags%dx + i*config_flags%dx
           y_dist = (-jde/2.0)*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
           IF (i .NE. ide) THEN
              ibm_ht_v(i,j) = offset + hm/(1.0+(x_dist/xa)**2+(y_dist/xa)**2)
           ENDIF

           x_dist = (-ide/2.0)*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
           y_dist = (-jde/2.0)*config_flags%dy + j*config_flags%dy
           IF (j .NE. jde) THEN
              ibm_ht_u(i,j) = offset + hm/(1.0+(x_dist/xa)**2+(y_dist/xa)**2)
           ENDIF

           x_dist = (-ide/2.0)*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
           y_dist = (-jde/2.0)*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
           ibm_ht_c(i,j) = offset + hm/(1.0+(x_dist/xa)**2+(y_dist/xa)**2)
        ENDDO
        ENDDO

        IF (config_flags%ideal_terrain .EQ. 33) THEN
            !DJW smooth the edges so that they linearly approach zero at the boundary-zone and
            !    are zero within the boundary-zone.
            dmid = MIN(config_flags%dx*ide, config_flags%dy*jde)/2.0 - config_flags%spec_bdy_width*MAX(config_flags%dx, config_flags%dy)
            offset = (config_flags%spec_bdy_width * MAX(config_flags%dx, config_flags%dy))
            DO j=jts,jte
                DO i=its,ite
                    dw = ( (config_flags%dy*(j-(jde-jds)/2))**2 + &
                           (config_flags%dx*(i-(ide-ids)/2))**2 )**(1.0/2.0)
                    du = ( (config_flags%dy*(j-(jde-jds)/2))**2 + &
                           (config_flags%dx*(i-0.5-(ide-ids)/2))**2 )**(1.0/2.0)
                    dv = ( (config_flags%dy*(j-0.5-(jde-jds)/2))**2 + &
                           (config_flags%dx*(i-(ide-ids)/2))**2 )**(1.0/2.0)
                    dc = ( (config_flags%dy*(j-0.5-(jde-jds)/2))**2 + &
                           (config_flags%dx*(i-0.5-(ide-ids)/2))**2 )**(1.0/2.0)
                    rc = MIN(1.0, MAX((dmid-dc)/offset, 0.0))
                    rw = MIN(1.0, MAX((dmid-dw)/offset, 0.0))
                    ru = MIN(1.0, MAX((dmid-du)/offset, 0.0))
                    rv = MIN(1.0, MAX((dmid-dv)/offset, 0.0))
                    ibm_ht_c(i,j) = ibm_ht_c(i,j)*rc
                    IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = ibm_ht_w(i,j)*rw
                    IF (i .NE. ide) ibm_ht_v(i,j) = ibm_ht_v(i,j)*rv
                    IF (j .NE. jde) ibm_ht_u(i,j) = ibm_ht_u(i,j)*ru
                ENDDO
            ENDDO
#if (IDEALIZED_TOPOG_VERBOSE) == 1
            write(*,'(4(A,I3),A)') "DJW[module_ibm/ibm_terrain]: ibm_ht_w(",its,":",ite,", ",jts,":",jte,") is below,"
            write(*,'(A)',ADVANCE='NO') "      "

            DO i=its,ite-1

               write(*,'(I5,A)',ADVANCE='NO') i," "
            ENDDO
            write(*,'(I5)') ite
            DO j=jte,jts,-1
               write(*,'(I3,A)',ADVANCE='NO') j,"   "
               DO i=its,ite-1
                  write(*,'(F5.1,A)',ADVANCE='NO') ibm_ht_w(i,j)," "
               ENDDO
               write(*,'(F5.1)') ibm_ht_w(ite,j)
            ENDDO
        ENDIF
#endif 
     ENDIF !end of ideal_terrain == 2 or 3
  ELSEIF (config_flags%ideal_terrain .EQ. 21) THEN
     !RSA 2d gaussian hill to roughly match Askervein transect A
     xa = 150.0
     hm = 100.0
     offset = 0.0
     DO j=jts,jte
     DO i=its,ite
        x_dist = (-ide/2.)*config_flags%dx + i*config_flags%dx
        if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm*exp(-0.5*(x_dist/xa)**2)
        if (i /= ide) ibm_ht_v(i,j) = offset + hm*exp(-0.5*(x_dist/xa)**2)
        x_dist = (-ide/2.)*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
        if (j /= jde) ibm_ht_u(i,j) = offset + hm*exp(-0.5*(x_dist/xa)**2)
        ibm_ht_c(i,j) = offset + hm*exp(-0.5*(x_dist/xa)**2)
     ENDDO
     ENDDO
  ELSEIF (config_flags%ideal_terrain .EQ. 31) THEN
     !RSA 3d gaussian hill roughly the scale of Askervein
     xa = 150.0
     hm = 100.0
     offset = 0.0

     DO j=jts,jte
     DO i=its,ite
        x_dist = (-ide/2.0)*config_flags%dx + i*config_flags%dx
        y_dist = (-jde/2.0)*config_flags%dy + j*config_flags%dy
        IF ((i .NE. ide) .AND. (j .NE. jde)) THEN
           ibm_ht_w(i,j) = offset + hm*exp(-0.5*((x_dist/xa)**2+(y_dist/xa)**2))
        ENDIF

        x_dist = (-ide/2.0)*config_flags%dx + i*config_flags%dx
        y_dist = (-jde/2.0)*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
        IF (i .NE. ide) THEN
           ibm_ht_v(i,j) = offset + hm*exp(-0.5*((x_dist/xa)**2+(y_dist/xa)**2))
        ENDIF

        x_dist = (-ide/2.0)*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
        y_dist = (-jde/2.0)*config_flags%dy + j*config_flags%dy
        IF (j .NE. jde) THEN
           ibm_ht_u(i,j) = offset + hm*exp(-0.5*((x_dist/xa)**2+(y_dist/xa)**2))
        ENDIF

        x_dist = (-ide/2.0)*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
        y_dist = (-jde/2.0)*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
        ibm_ht_c(i,j) = offset + hm*exp(-0.5*((x_dist/xa)**2+(y_dist/xa)**2))
     ENDDO
     ENDDO
  ELSEIF (config_flags%ideal_terrain .EQ. 4) THEN
     ! This is for the 2d isolated cube (ridge) case

     ! parameters used for the bolund case
     !xa = 20.0
     !hm = 10.0
     !offset = 0.75

     ! parameters used for the 3d hill case
     xa = 150.0
     hm = 50.0
     offset = 0.0
    
     DO j=jts,jte
     DO i=its,ite
        if (j /= jde) ibm_ht_u(i,j)=offset
        if (i /= ide) ibm_ht_v(i,j)=offset
        if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j)=offset
        ibm_ht_c(i,j)=offset
        icm = jde/4
        x_dist = -icm*config_flags%dy + j*config_flags%dy
        if (ABS(x_dist)<=xa) then
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
           if (i /= ide) ibm_ht_v(i,j) = offset + hm
        endif  
        x_dist = -icm*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
        if (ABS(x_dist)<=xa) then
           if (j /= jde) ibm_ht_u(i,j) = offset + hm
           ibm_ht_c(i,j) = offset + hm
        endif  
     ENDDO
     ENDDO

  ELSEIF (config_flags%ideal_terrain .EQ. 5) THEN   

     ! This is for the 3d isolated cube case
     xa = 48.0
     hm = 60.0
     !hm = 74.6385
     offset = 50.0
     icm = ide/2
     jcm = jde/2
     DO j=jts,jte
     DO i=its,ite    
        x_dist = -icm*config_flags%dx + i*config_flags%dx
        y_dist = -jcm*config_flags%dy + j*config_flags%dy
        IF ((ABS(x_dist) .LE. xa) .AND. (ABS(y_dist) .LE. xa)) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = offset + hm
        ELSE
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = offset
        ENDIF
        y_dist = -jcm*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
        IF ((ABS(x_dist) .LE. xa) .AND. (ABS(y_dist) .LE. xa)) THEN
           IF (i .NE. ide) ibm_ht_v(i,j) = offset + hm
        ELSE
           IF (i .NE. ide) ibm_ht_v(i,j) = offset
        ENDIF     
        x_dist = -icm*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
        y_dist = -jcm*config_flags%dy + j*config_flags%dy
        IF ((ABS(x_dist) .LE. xa) .AND. (ABS(y_dist) .LE. xa)) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = offset + hm
        ELSE
           IF (j .NE. jde) ibm_ht_u(i,j) = offset
        ENDIF     
        y_dist = -jcm*config_flags%dy - config_flags%dy/2.0 + j*config_flags%dy
        IF ((ABS(x_dist) .LE. xa) .AND. (ABS(y_dist) .LE. xa)) THEN
           ibm_ht_c(i,j) = offset + hm
        ELSE
           ibm_ht_c(i,j) = offset
        ENDIF
     ENDDO      
     ENDDO 

!  ! This is for the 3d array of cubes case
!  xa = 30.
!  hm = 150.
!  offset = 25.
!  DO j=jts,jte
!  DO i=its,ite  
!     ! set the base terrain
!     if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset
!     if (i /= ide) ibm_ht_v(i,j) = offset
!     if (j /= jde) ibm_ht_u(i,j) = offset
!     ibm_ht_c(i,j) = offset	 
!
!     icm = ide/4.
!     jcm = jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif
!
!     icm = ide/4.
!     jcm = jde/2.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif	 
!
!     icm = ide/4.
!     jcm = 3*jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif   
!
!     icm = ide/2.
!     jcm = jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif
!
!     icm = ide/2.
!     jcm = jde/2.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif	 
!
!     icm = ide/2.
!     jcm = 3*jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif   
!
!     icm = 3*ide/4.
!     jcm = jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif
!
!     icm = 3*ide/4.
!     jcm = jde/2.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif	 
!
!     icm = 3*ide/4.
!     jcm = 3*jde/4.  
!     x_dist = -icm*config_flags%dx + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + hm
!     endif
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (i /= ide) ibm_ht_v(i,j) = offset + hm
!     endif	 
!     x_dist = -icm*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
!     y_dist = -jcm*config_flags%dy + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  if (j /= jde) ibm_ht_u(i,j) = offset + hm
!     endif	 
!     y_dist = -jcm*config_flags%dy - config_flags%dy/2. + j*config_flags%dy
!     if ((ABS(x_dist)<=xa).AND.(ABS(y_dist)<=xa)) then
!	  ibm_ht_c(i,j) = offset + hm
!     endif		   
!
!   ENDDO      
!   ENDDO	  
  ELSEIF (config_flags%ideal_terrain .EQ. 6) THEN
     hm = 1500.0
     DO j=jts,jte
     DO i=its,ite 
        !calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2.0)*config_flags%dx + i*config_flags%dx
        IF (ABS(x_dist) .LE. 500.0) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
           IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
        ELSEIF ((ABS(x_dist) .GT. 500.0) .AND. (ABS(x_dist) .LT. 9500.0)) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = hm*(0.5 - 0.5*COS((pi*(ABS(x_dist)-500.0))/9000.0))
           IF (i .NE. ide) ibm_ht_v(i,j) = hm*(0.5 - 0.5*COS((pi*(ABS(x_dist)-500.0))/9000.0))
        ELSEIF ((ABS(x_dist) .GE. 9500.0) .AND. (ABS(x_dist) .LE. 10500.0)) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = hm
           IF (i .NE. ide) ibm_ht_v(i,j) = hm
        ELSEIF ((ABS(x_dist) .GT. 10500.0) .AND. (ABS(x_dist) .LT. 19500.0)) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = hm*(0.5 + 0.5*COS((pi*(ABS(x_dist)-10500.0))/9000.0))
           IF (i .NE. ide) ibm_ht_v(i,j) = hm*(0.5 + 0.5*COS((pi*(ABS(x_dist)-10500.0))/9000.0))
        ELSEIF (ABS(x_dist).GE. 19500.0) then
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = 0.0
           IF (i .NE. ide) ibm_ht_v(i,j) = 0.0
        ENDIF
     ENDDO
     ENDDO
     DO j=jts,jte
     DO i=its,ite 
        !calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2.0)*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
        IF (ABS(x_dist) .LE. 500.0) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
           ibm_ht_c(i,j) = 0.0
        ELSEIF ((ABS(x_dist) .GT. 500.0) .AND. (ABS(x_dist) .LT. 9500.0)) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = hm*(0.5 - 0.5*COS((pi*(ABS(x_dist)-500.0))/9000.0))
           ibm_ht_c(i,j) = hm*(0.5 - 0.5*COS((pi*(ABS(x_dist)-500.0))/9000.0))
        ELSEIF ((ABS(x_dist) .GE. 9500.0) .AND. (ABS(x_dist) .LE. 10500.0)) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = hm
           ibm_ht_c(i,j) = hm
        ELSEIF ((ABS(x_dist) .GT. 10500.0) .AND. (ABS(x_dist) .LT. 19500.0)) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = hm*(0.5 + 0.5*COS((pi*(ABS(x_dist)-10500.0))/9000.0))
           ibm_ht_c(i,j) = hm*(0.5 + 0.5*COS((pi*(ABS(x_dist)-10500.0))/9000.0))
        ELSEIF (ABS(x_dist) .GE. 19500.0) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = 0.0
           ibm_ht_c(i,j) = 0.0
        ENDIF
     ENDDO
     ENDDO
  ELSEIF (config_flags%ideal_terrain .EQ. 7) THEN
  ELSEIF (config_flags%ideal_terrain .EQ. 8) THEN
     !RSA v-shaped valley case
     offset = 0.0
     slope = 0.2126 !RSA 12 degrees
     xa = 1000.0 !side ridges
#if (IDEALIZED_TOPOG_VERBOSE) == 1
     write(*,*) "ide=",ide,"ide/2=",ide/2
     write(*,*) "dx=",config_flags%dx
#endif 
     DO j=jts,jte
     DO i=its,ite 
        ! v and w points
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2)*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
        IF (ABS(x_dist) .LT. (ide/2)*config_flags%dx - xa) THEN
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + slope*ABS(x_dist)
           if (i /= ide) ibm_ht_v(i,j) = offset + slope*ABS(x_dist)
        ELSE
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + slope*((ide/2)*config_flags%dx - xa)
           if (i /= ide) ibm_ht_v(i,j) = offset + slope*((ide/2)*config_flags%dx - xa)
        ENDIF
        if (j .eq. jts) write(*,*) "i=",i,"xdist=",x_dist,"ht_w=",ibm_ht_w(i,j)
        ! u and c points
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2)*config_flags%dx + (i-1)*config_flags%dx
        IF (ABS(x_dist) .LT. (ide/2)*config_flags%dx - xa) THEN
           if (j /= jde) ibm_ht_u(i,j) = offset + slope*ABS(x_dist)
           ibm_ht_c(i,j) = offset + slope*ABS(x_dist)
        ELSE
           if (j /= jde) ibm_ht_u(i,j) = offset + slope*((ide/2)*config_flags%dx - xa)
           ibm_ht_c(i,j) = offset + slope*((ide/2)*config_flags%dx - xa)
        ENDIF
        if (j .eq. jts) write(*,*) "i=",i,"xdist=",x_dist,"ht_u=",ibm_ht_u(i,j)
     ENDDO
     ENDDO 
     ! offset = 100.
     ! DO j=jts,jte
     ! DO i=its,ite 
     !    ! 0.0875 for 5 degree slope comparing with WRF 0.5774 for a 30 degree slope
     !    ! calculate x with x=0 as the middle of the domain
     !    x_dist = (-ide/2.)*config_flags%dx + i*config_flags%dx
     !    if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset + 0.0875*ABS(x_dist)
     !    if (i /= ide) ibm_ht_v(i,j) = offset + 0.0875*ABS(x_dist)
     !    ! calculate x with x=0 as the middle of the domain
     !    x_dist = (-ide/2.)*config_flags%dx - config_flags%dx/2 + i*config_flags%dx
     !    if (j /= jde) ibm_ht_u(i,j) = offset + 0.0875*ABS(x_dist)
     !    ibm_ht_c(i,j) = offset + 0.0875*ABS(x_dist)
     ! ENDDO
     ! ENDDO 
     ! !KAL flatten the terrain at the east/west boundary- for using periodic boundary conditions and comparing to IBM
     ! DO j=jts,jte 
     !    if (j /= jde) ibm_ht_w(1,j) = ibm_ht_w(2,j)
     !    if (j /= jde) ibm_ht_w(ite-1,j) = ibm_ht_w(ite-2,j) 
     !    ibm_ht_v(1,j) = ibm_ht_v(2,j)
     !    ibm_ht_v(ite-1,j) = ibm_ht_v(ite-2,j) 
     !    if (j /= jde) ibm_ht_u(1,j) = ibm_ht_w(2,j)
     !    if (j /= jde) ibm_ht_u(2,j) = ibm_ht_w(2,j)
     !    if (j /= jde) ibm_ht_u(ite,j) = ibm_ht_w(ite-2,j) 
     !    if (j /= jde) ibm_ht_u(ite-1,j) = ibm_ht_w(ite-2,j)      
     !    ibm_ht_c(1,j) = ibm_ht_w(2,j)
     !    ibm_ht_c(2,j) = ibm_ht_w(2,j)
     !    ibm_ht_c(ite,j) = ibm_ht_w(ite-2,j) 
     !    ibm_ht_c(ite-1,j) = ibm_ht_w(ite-2,j)     
     ! ENDDO 
  ELSEIF (config_flags%ideal_terrain .EQ. 81) THEN
     !RSA sinusoidal valley case
     offset = 550.0
     xa = 1000.0 !side ridges
#if (IDEALIZED_TOPOG_VERBOSE) == 1
     write(*,*) "ide=",ide,"ide/2=",ide/2
     write(*,*) "dx=",config_flags%dx
#endif 
     DO j=jts,jte
     DO i=its,ite 
        ! v and w points
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2)*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
        IF (ABS(x_dist) .LT. (ide/2)*config_flags%dx - xa) THEN
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset/2 - offset/2*COS(2*pi*ABS(x_dist)/((ide-1)*config_flags%dx-2*xa))
           if (i /= ide) ibm_ht_v(i,j) = offset/2 - offset/2*COS(2*pi*ABS(x_dist)/((ide-1)*config_flags%dx-2*xa))
        ELSE
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = offset
           if (i /= ide) ibm_ht_v(i,j) = offset
        ENDIF
        if (j .eq. jts) write(*,*) "i=",i,"xdist=",x_dist,"ht_w=",ibm_ht_w(i,j)
        ! u and c points
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2)*config_flags%dx + (i-1)*config_flags%dx
        IF (ABS(x_dist) .LT. (ide/2)*config_flags%dx - xa) THEN
           if (j /= jde) ibm_ht_u(i,j) = offset/2 - offset/2*COS(2*pi*ABS(x_dist)/((ide-1)*config_flags%dx-2*xa))
           ibm_ht_c(i,j) = offset/2 - offset/2*COS(2*pi*ABS(x_dist)/((ide-1)*config_flags%dx-2*xa))
        ELSE
           if (j /= jde) ibm_ht_u(i,j) = offset
           ibm_ht_c(i,j) = offset
        ENDIF
        if (j .eq. jts) write(*,*) "i=",i,"xdist=",x_dist,"ht_u=",ibm_ht_u(i,j)
     ENDDO
     ENDDO 
  ELSEIF (config_flags%ideal_terrain .EQ. 9) THEN
     hm = 3000.
     xa = 25000.
     xa1  = 8000.
     DO j=jts,jte
     DO i=its,ite
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2.)*config_flags%dx + i*config_flags%dx
        if (ABS(x_dist) <= xa) then
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = hm*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(xa1))*COS((pi*x_dist)/(xa1))
           if (i /= ide) ibm_ht_v(i,j) = hm*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(xa1))*COS((pi*x_dist)/(xa1))
        elseif (ABS(x_dist) > xa) then
           if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j) = 0.
           if (i /= ide) ibm_ht_v(i,j) = 0.
        endif        
     ENDDO
     ENDDO   
     DO j=jts,jte
     DO i=its,ite
        ! calculate x with x=0 as the middle of the domain
        x_dist = (-ide/2.)*config_flags%dx - config_flags%dx/2. + i*config_flags%dx
        if (ABS(x_dist) <= xa) then
           if (j /= jde) ibm_ht_u(i,j) = hm*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(xa1))*COS((pi*x_dist)/(xa1))
           ibm_ht_c(i,j) = hm*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(2*xa))*COS((pi*x_dist)/(xa1))*COS((pi*x_dist)/(xa1))
        elseif (ABS(x_dist) > xa) then
           if (j /= jde) ibm_ht_u(i,j) = 0.
           ibm_ht_c(i,j) = 0.
        endif        
     ENDDO
     ENDDO         
  ELSEIF (config_flags%ideal_terrain .EQ. 10) then
     !CALL read_terrain_data( ibm_ht_u, ibm_ht_v,           &
     !                        ibm_ht_w, ibm_ht_c,           &
     !                        ibm_z0,                       &
     !                        ids, ide, jds, jde, kds, kde, &
     !                        ims, ime, jms, jme, kms, kme, &
     !                        its, ite, jts, jte, kts, kte )

     offset = 0.0    
     DO j=jts,jte
     DO i=its,ite
        if (j /= jde) ibm_ht_u(i,j)= offset + ibm_ht_u(i,j)
        if (i /= ide) ibm_ht_v(i,j)= offset + ibm_ht_v(i,j)
        if ((i /= ide) .AND. (j /= jde)) ibm_ht_w(i,j)=offset + ibm_ht_w(i,j)
        ibm_ht_c(i,j) = offset + ibm_ht_c(i,j)
     ENDDO
     ENDDO     		   			   

  ELSEIF (config_flags%ideal_terrain .EQ. 12) THEN
     !This is for the 2d isolated ridge case
!     xa = 48.0
     xa = 51.0
     hm = 60.0
     !hm = 74.6385
     offset = 50.0
     icm = ide/2
     DO j=jts,jte
     DO i=its,ite    
        x_dist = -icm*config_flags%dx + i*config_flags%dx
        IF (ABS(x_dist) .LE. xa) THEN
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = offset + hm
           IF (i .NE. ide) ibm_ht_v(i,j) = offset + hm
        ELSE
           IF ((i .NE. ide) .AND. (j .NE. jde)) ibm_ht_w(i,j) = offset
           IF (i .NE. ide) ibm_ht_v(i,j) = offset
        ENDIF
        x_dist = -icm*config_flags%dx - config_flags%dx/2.0 + i*config_flags%dx
        IF (ABS(x_dist) .LE. xa) THEN
           IF (j .NE. jde) ibm_ht_u(i,j) = offset + hm
           ibm_ht_c(i,j) = offset + hm
        ELSE
           IF (j .NE. jde) ibm_ht_u(i,j) = offset
           ibm_ht_c(i,j) = offset
        ENDIF
     ENDDO      
     ENDDO 

  ELSEIF (config_flags%ideal_terrain .EQ. 13) THEN
    
     write(*,'(A,I1)') "REOS[compute_idealized_terrain]: ***Exit*** -- reached invalid parameter: grid%id=",config_flags%ideal_terrain
         return 

  ELSEIF (config_flags%ideal_terrain .EQ. 14) THEN
     
     write(*,'(A,I1)') "REOS[compute_idealized_terrain]: ***Exit*** -- reached invalid parameter: grid%id=",config_flags%ideal_terrain
         return 

  ELSEIF (config_flags%ideal_terrain .EQ. 15) THEN
    
      write(*,'(A,I1)') "REOS[compute_idealized_terrain]: ***Exit*** -- reached invalid parameter: grid%id=",config_flags%ideal_terrain
         return 

  ENDIF ! end of ideal_terrain option			   
  

   
 END SUBROUTINE compute_idealized_terrain

 ! Caller of compute_idealized_terrain 


SUBROUTINE modify_idealized_terrain ( config_flags,                           &
                             ibm_ht_u, ibm_ht_v,                     &
                             ibm_ht_w, ibm_ht_c,                     &
                             ibm_z0,                                 &
                             phb, ph,                                &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             its, ite, jts, jte, kts, kte )
 IMPLICIT NONE
 !input data
 TYPE(terrain_config_t), INTENT(IN   )                   :: config_flags
 REAL, DIMENSION(ims:ime,jms:jme), INTENT(  OUT)         :: ibm_ht_u,       & !ibm terrain height at velocity points
                                                            ibm_ht_v,       & !terrain height has twice the resolution of
                                                            ibm_ht_w,       & !the computational grid							    
                                                            ibm_ht_c,       & !ibm terrain height at corners
                                                            ibm_z0            !ibm z0 at cell center
 REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: phb, ph
 INTEGER, INTENT(IN   )                                  :: ids, ide, jds, jde, kds, kde, & !d: domain 
                                                            ims, ime, jms, jme, kms, kme, & !m: memory
                                                            its, ite, jts, jte, kts, kte    !p: patch t: tile
 !local data
 INTEGER                                                 :: i, j, k
 
 REAL :: bldx, bldy, bldz, gapx, gapy, theta, xoff, yoff, bx, by, xmid, ymid
 LOGICAL :: keepgoing
 char(*), parameter :: sub_name = 'modify_idealized_terrain'

 CALL compute_idealized_terrain ( config_flags,                 &
                    ibm_ht_u, ibm_ht_v,           &
                    ibm_ht_w, ibm_ht_c,           &
                    ibm_z0,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte ) 

 
     IF (config_flags%ideal_terrain .EQ. 0) THEN
        
         DO i=its,ite
             DO j=jts,jte
                 ibm_ht_w(i,j) =  phb(i,   1, j  ) / 9.81
                 ibm_ht_u(i,j) = (phb(i-1, 1, j  ) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_v(i,j) = (phb(i,   1, j-1) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_c(i,j) = (phb(i-1, 1, j-1) + phb(i-1, 1, j  ) + &
                                  phb(i,   1, j-1) + phb(i,   1, j  )) / (4.0*9.81)
             ENDDO
         ENDDO
#if (IDEALIZED_TOPOG_VERBOSE) == 1        
         DO i=its,ite-1
             write(*,'(I5,A)',ADVANCE='NO') i," "
         ENDDO
         write(*,'(I5)') ite
         DO j=jts,jte
             write(*,'(I3,A)',ADVANCE='NO') j," "
             DO i=its,ite-1
                 write(*,'(F5.1,A)',ADVANCE='NO') ibm_ht_w(i,j)," "
             ENDDO
             write(*,'(F5.1)') ibm_ht_w(ite,j)
         ENDDO
#endif 
     ELSEIF (config_flags%ideal_terrain .EQ. 114) THEN
         DO i=its,ite
             DO j=jts,jte
                 ibm_ht_w(i,j) =  phb(i,   1, j  ) / 9.81
                 ibm_ht_u(i,j) = (phb(i-1, 1, j  ) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_v(i,j) = (phb(i,   1, j-1) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_c(i,j) = (phb(i-1, 1, j-1) + phb(i-1, 1, j  ) + &
                                  phb(i,   1, j-1) + phb(i,   1, j  )) / (4.0*9.81)
             ENDDO
         ENDDO
         IF (((its .GE. (ide-ids)/2+ids-5) .OR. (ite .LE. (ide-ids)/2+ids+5)) .AND. &
             ((jts .GE. (jde-jds)/2+jds-5) .OR. (jte .LE. (jde-jds)/2+jds+5))) THEN
             DO i=MAX(its,(ide-ids)/2+ids-5),MIN(ite,(ide-ids)/2+ids+5)
                 DO j=MAX(jts,(jde-jds)/2+jds-5),MIN(jte,(jde-jds)/2+jds+5)
                     ibm_ht_w(i,j) = 400.0
                     ibm_ht_u(i,j) = 400.0
                     ibm_ht_v(i,j) = 400.0
                     ibm_ht_c(i,j) = 400.0
                 ENDDO
             ENDDO
         ENDIF
     ELSEIF (config_flags%ideal_terrain .EQ. 115) THEN
         !This will add an angled set of buildings to the existing IBM height
         DO i=its,ite
             DO j=jts,jte
                 ibm_ht_w(i,j) =  phb(i,   1, j  ) / 9.81
                 ibm_ht_u(i,j) = (phb(i-1, 1, j  ) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_v(i,j) = (phb(i,   1, j-1) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_c(i,j) = (phb(i-1, 1, j-1) + phb(i-1, 1, j  ) + &
                                  phb(i,   1, j-1) + phb(i,   1, j  )) / (4.0*9.81)
             ENDDO
         ENDDO
         bldx = 75.0 !east-west width of building
         bldy = 75.0 !north-south width of building
         bldz = 10.0 !height of building
         gapx = 150.0 !east-west gap between buildings
         gapy = 150.0 !north-south gap between buildings
         theta = 10.0*2.0*4.0*ATAN(1.0_8)/360.0 !angle of buildings (deviation clockwise from north)
         DO i=(its-1)*2,(ite-1)*2
             DO j=(jts-1)*2,(jte-1)*2
                 xoff = j*config_flags%dy/2.0*TAN(theta)
                 bx = MOD(i*config_flags%dx/2.0+xoff, gapx+bldx)
                 IF ((bx .GE. (gapx+bldx)/2.0-bldx/2.0) .AND. (bx .LE. (gapx+bldx)/2.0+bldx/2.0)) THEN
                     yoff = i*config_flags%dx/2.0*TAN(-theta)
                     by = MOD(j*config_flags%dy/2.0+yoff, gapy+bldy)
                     IF ((by .GE. (gapy+bldy)/2.0-bldy/2.0) .AND. (by .LE. (gapy+bldy)/2.0+bldy/2.0)) THEN
                         IF ((FLOOR(i/2.0) .EQ. i/2.0) .AND. (FLOOR(j/2.0) .EQ. j/2.0)) THEN
                             ibm_ht_c(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_c(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSEIF (FLOOR(i/2.0) .EQ. i/2.0) THEN
                             ibm_ht_u(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_u(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSEIF (FLOOR(j/2.0) .EQ. j/2.0) THEN
                             ibm_ht_v(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_v(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSE
                             ibm_ht_w(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_w(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ENDIF
                     ENDIF
                 ENDIF
             ENDDO !j-loop
         ENDDO !i-loop
     ELSEIF (config_flags%ideal_terrain .EQ. 116) THEN
         !This will add a set of buildings offset at an angle to the existing IBM height
         !The result is different than 115 since the buildings in 116 are still
         !oriented north-south instead of rotating to match the offset angle.
         DO i=its,ite
             DO j=jts,jte
                 ibm_ht_w(i,j) =  phb(i,   1, j  ) / 9.81
                 ibm_ht_u(i,j) = (phb(i-1, 1, j  ) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_v(i,j) = (phb(i,   1, j-1) + phb(i,   1, j  )) / (2.0*9.81)
                 ibm_ht_c(i,j) = (phb(i-1, 1, j-1) + phb(i-1, 1, j  ) + &
                                  phb(i,   1, j-1) + phb(i,   1, j  )) / (4.0*9.81)
             ENDDO
         ENDDO
         bldx = 80.0 !east-west width of buildings
         bldy = 80.0 !north-south width of buildings
         bldz = 10.0 !height AGL of buildings
         gapx = 240.0 !east-west gap between buildings
         gapy = 240.0 !north-south gap between buildings
         theta = 30.0 !angle (degrees) of buildings (clockwise from north)
         DO j=(jts-1)*2,(jte-1)*2+1
             by = j*config_flags%dy/2.0-(FLOOR(j*config_flags%dy/2.0/(gapy+bldy))+0.5)*(gapy+bldy)
             keepgoing = .TRUE.
             ymid = (FLOOR(j*config_flags%dy/2.0/(gapy+bldy))+0.5)*(gapy+bldy)
             IF ((ymid-bldy/2.0 .LE. (config_flags%spec_bdy_width-0.5)*config_flags%dy) .OR. &
                 (ymid+bldy/2.0 .GE. (jde-config_flags%spec_bdy_width-0.5)*config_flags%dy)) THEN
                 keepgoing = .FALSE.
             ENDIF
             IF ((by .GE. -bldy/2.0) .AND. (by .LE. bldy/2.0) .AND. keepgoing) THEN
                 yoff = FLOOR(j*config_flags%dy/2.0/(gapy+bldy))*(gapy+bldy)
                 xoff = yoff*TAN(-theta*2.0*4.0*ATAN(1.0_8)/360.0)
                 DO i=(its-1)*2,(ite-1)*2+1
                     bx = (i*config_flags%dx/2.0+xoff)-(FLOOR((i*config_flags%dx/2.0+xoff)/(gapx+bldx))+0.5)*(gapx+bldx)
                     keepgoing = .TRUE.
                     xmid = i*config_flags%dx/2.0-bx
                     IF ((xmid-bldx/2.0 .LE. (config_flags%spec_bdy_width-0.5)*config_flags%dx) .OR. &
                         (xmid+bldx/2.0 .GE. (ide-config_flags%spec_bdy_width-0.5)*config_flags%dx)) THEN
                         keepgoing = .FALSE.
                     ENDIF
                     IF ((bx .GE. -bldx/2.0) .AND. (bx .LE. bldx/2.0) .AND. keepgoing) THEN
                         IF ((FLOOR(i/2.0) .EQ. i/2.0) .AND. (FLOOR(j/2.0) .EQ. j/2.0)) THEN
                             ibm_ht_c(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_c(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSEIF (FLOOR(i/2.0) .EQ. i/2.0) THEN
                             ibm_ht_u(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_u(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSEIF (FLOOR(j/2.0) .EQ. j/2.0) THEN
                             ibm_ht_v(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_v(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ELSE
                             ibm_ht_w(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1) = ibm_ht_w(FLOOR(i/2.0)+1,FLOOR(j/2.0)+1)+bldz
                         ENDIF
                     ENDIF !within x-extents of building
                 ENDDO !x-loop
             ENDIF !within y-extents of building
         ENDDO !y-loop
     ENDIF !ideal_terrain
 
 END SUBROUTINE init_idealized_terrain
 

end module idealized_topog

