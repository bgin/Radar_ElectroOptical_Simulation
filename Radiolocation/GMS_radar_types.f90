


module radar_types


!=====================================================!
! Aggregartion of various data types representing     !
! various  Radar components through its               !
! simulation processes and algorithms.                !
!=====================================================!
     
     use mod_kinds,     only : i1,i4, sp, dp
     use ISO_C_BINDING, only : c_char
     implicit none
     public

     ! Major version
     integer(kind=i4),   parameter :: RADAR_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),   parameter :: RADAR_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),   parameter :: RADAR_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),   parameter :: RADAR_TYPES_FULLVER =   &
            1000*RADAR_TYPES_MAJOR+100*RADAR_TYPES_MINOR+10*RADAR_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: RADAR_TYPES_DATE        = "17-07-2022 16:25 +00200 (SUN 17 JUL 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: RADAR_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RADAR_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RADAR_TYPES_SYNOPSIS    = "Aggregations of various Radar related derived types."

#if !defined (RADAR_TYPES_DATA_PAD)
#define RADAR_TYPES_DATA_PAD 1
#define PADDING(nbytes) character(kind=c_char), dimension(nbytes) :: pad
#endif
     
     type, public :: radar_aos_r4
           sequence
           real(kind=sp), public :: gamm !//m, wavelength
	   real(kind=sp), public :: tf   !//sec, coherent processing time
	   real(kind=sp), public :: rho  !//usec,pulsewidth
	   real(kind=sp), public :: w    !//m, apperture width
           real(kind=sp), public :: Kth  !//beamwidth constant
	   real(kind=sp), public :: Ln   !//pattern constant
	   real(kind=sp), public :: Ts   !//K, system noise temperature
	   real(kind=sp), public :: Fp   !//polarization factor
	   real(kind=sp), public :: La   !//dB, troposepheric attenuation
           real(kind=sp), public :: F    !//radar pattern propagation factor
	   real(kind=sp), public :: Pt   !//kW, transmitter power
           real(kind=sp), public :: tr   !//usec, PRI
	   real(kind=sp), public :: Lt   !//dB, transmitt line loss
	   real(kind=sp), public :: h    !//m, apperture height
	   real(kind=sp), public :: ha   !//m, phase centre
	   real(kind=sp), public :: Frdr !//range dependent response
	   real(kind=sp), public :: Dx   !//dB, detectibility factor
	   real(kind=sp), public :: Bt   !//Mhz, tuneable bandwidth
	   real(kind=sp), public :: Flen !//dB, tropospheric attenuation
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(52)	  
#endif
     end type radar_aos_r4


     type, public :: radar_aos_r8
           sequence
           real(kind=dp), public :: gamm !//m, wavelength
	   real(kind=dp), public :: tf   !//sec, coherent processing time
	   real(kind=dp), public :: rho  !//usec,pulsewidth
	   real(kind=dp), public :: w    !//m, apperture width
           real(kind=dp), public :: Kth  !//beamwidth constant
	   real(kind=dp), public :: Ln   !//pattern constant
	   real(kind=dp), public :: Ts   !//K, system noise temperature
	   real(kind=dp), public :: Fp   !//polarization factor
	   real(kind=dp), public :: La   !//dB, troposepheric attenuation
           real(kind=dp), public :: F    !//radar pattern propagation factor
	   real(kind=dp), public :: Pt   !//kW, transmitter power
           real(kind=dp), public :: tr   !//usec, PRI
	   real(kind=dp), public :: Lt   !//dB, transmitt line loss
	   real(kind=dp), public :: h    !//m, apperture height
	   real(kind=dp), public :: ha   !//m, phase centre
	   real(kind=dp), public :: Frdr !//range dependent response
	   real(kind=dp), public :: Dx   !//dB, detectibility factor
	   real(kind=dp), public :: Bt   !//Mhz, tuneable bandwidth
	   real(kind=dp), public :: Flen !//dB, tropospheric attenuation
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(40)		  
#endif
     end type radar_aos_r8
     










end module radar_types
