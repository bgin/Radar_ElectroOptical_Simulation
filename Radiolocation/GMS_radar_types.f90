

module radar_types


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'radar_types'
 !          
 !          Purpose:
  !                     This module contains various implementations of
 !                      Radar simulation formulae helper data-types.
 !          History:
 !                        
 !                        Date: 05-04-2022
 !                        Time: 14:01 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp,dp
     use mod_vectypes, only : YMM8r4_t,YMM4r8_t,ZMM16r4_t,ZMM8r8_t
     implicit none
     public

      !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: RADAR_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: RADAR_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: RADAR_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4), parameter :: RADAR_TYPES_FULLVER = &
          1000*RADAR_TYPES_MAJOR+100*RADAR_TYPES_MINOR+10*RADAR_TYPES_MICRO
     ! Module creation date
     character(*),       parameter :: RADAR_TYPES_CREATION_DATE = "05-04-2022 14:05 +00200 (TUE 05 APR 2022 GMT+2)"
     ! Module build date
     character(*),       parameter :: RADAR_TYPES_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: RADAR_TYPES_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: RADAR_TYPES_SYNOPSIS      = "Radar simulation formulae helper data-types."


     type, public :: RadarParamAoS_R4_1

        real(kind=sp) ::  gamm !m, wavelength
        real(kind=sp) ::  tf   !sec, coherent processing time
        real(kind=sp) ::  rho  !usec,pulsewidth
        real(kind=sp) ::  w    !m, apperture width
        real(kind=sp) ::  Kth  !beamwidth constant
        real(kind=sp) ::  Ln   !pattern constant
        real(kind=sp) ::  Ts   !K, system noise temperature
	real(kind=sp) ::  Fp   !polarization factor
	real(kind=sp) ::  La   !dB, troposepheric attenuation
	real(kind=sp) ::  F    !radar pattern propagation factor
	real(kind=sp) ::  Pt   !kW, transmitter power
	real(kind=sp) ::  tr   !usec, PRI
	real(kind=sp) ::  Lt   !dB, transmitt line loss
	real(kind=sp) ::  h    !m, apperture height
	real(kind=sp) ::  ha   !m, phase centre
	real(kind=sp) ::  Frdr !range dependent response
	real(kind=sp) ::  Dx   !dB, detectibility factor
	real(kind=sp) ::  Bt   !Mhz, tuneable bandwidth
        real(kind=sp) ::  Flen !dB, tropospheric attenuation
     end type RadarParamAoS_R4_1
     

     type, public :: RadarParamAoS_R8_1

        real(kind=dp) ::  gamm !m, wavelength
        real(kind=dp) ::  tf   !sec, coherent processing time
        real(kind=dp) ::  rho  !usec,pulsewidth
        real(kind=dp) ::  w    !m, apperture width
        real(kind=dp) ::  Kth  !beamwidth constant
        real(kind=dp) ::  Ln   !pattern constant
        real(kind=dp) ::  Ts   !K, system noise temperature
	real(kind=dp) ::  Fp   !polarization factor
	real(kind=dp) ::  La   !dB, troposepheric attenuation
	real(kind=dp) ::  F    !radar pattern propagation factor
	real(kind=dp) ::  Pt   !kW, transmitter power
	real(kind=dp) ::  tr   !usec, PRI
	real(kind=dp) ::  Lt   !dB, transmitt line loss
	real(kind=dp) ::  h    !m, apperture height
	real(kind=dp) ::  ha   !m, phase centre
	real(kind=dp) ::  Frdr !range dependent response
	real(kind=dp) ::  Dx   !dB, detectibility factor
	real(kind=dp) ::  Bt   !Mhz, tuneable bandwidth
        real(kind=dp) ::  Flen !dB, tropospheric attenuation
     end type RadarParamAoS_R8_1 


     type, public :: JammerParamAoS_R4_1
        
         real(kind=sp) :: sig  !m, RSC of target
	 real(kind=sp) :: Pj   !W, jammer power
	 real(kind=sp) :: Gj   !dB, jammer antenna gain
	 real(kind=sp) :: Qj   !dB, jammer noise quality
         real(kind=sp) :: Flenj!dB, jammer lens factor
	 real(kind=sp) :: Rj   !km, jammer range
	 real(kind=sp) :: Bj   !Mhz,jammer noise BW
	 real(kind=sp) :: Ltj  !dB, jammer transmit loss
	 real(kind=sp) :: Fpj  !dB, jammer polarization
	 real(kind=sp) :: Rmj  !km, jammer screening range
	 real(kind=sp) :: Fj   !dB, jammer pattern factor of propagation
	 real(kind=sp) :: Laj  !dB, jammer troposhperic loss
     end type JammerParamAoS_R4_1

  
     type, public :: JammerParamAoS_R8_1
        
         real(kind=dp) :: sig  !m, RSC of target
	 real(kind=dp) :: Pj   !W, jammer power
	 real(kind=dp) :: Gj   !dB, jammer antenna gain
	 real(kind=dp) :: Qj   !dB, jammer noise quality
         real(kind=dp) :: Flenj!dB, jammer lens factor
	 real(kind=dp) :: Rj   !km, jammer range
	 real(kind=dp) :: Bj   !Mhz,jammer noise BW
	 real(kind=dp) :: Ltj  !dB, jammer transmit loss
	 real(kind=dp) :: Fpj  !dB, jammer polarization
	 real(kind=dp) :: Rmj  !km, jammer screening range
	 real(kind=dp) :: Fj   !dB, jammer pattern factor of propagation
	 real(kind=dp) :: Laj  !dB, jammer troposhperic loss
     end type JammerParamAoS_R8_1

     
     type, public :: RadarParamSIMD_R4_8

         type(YMM8r4_t) ::  gamm !m, wavelength
	 type(YMM8r4_t) ::  tf   !sec, coherent processing time
	 type(YMM8r4_t) ::  rho  !usec,pulsewidth
	 type(YMM8r4_t) ::  w    !m, apperture width
         type(YMM8r4_t) ::  Kth  !beamwidth constant
	 type(YMM8r4_t) ::  Ln   !pattern constant
	 type(YMM8r4_t) ::  Ts   !K, system noise temperature
	 type(YMM8r4_t) ::  Fp   !polarization factor
	 type(YMM8r4_t) ::  La   !dB, troposepheric attenuation
	 type(YMM8r4_t) ::  F    !radar pattern propagation factor
	 type(YMM8r4_t) ::  Pt   !kW, transmitter power
	 type(YMM8r4_t) ::  tr   !usec, PRI
	 type(YMM8r4_t) ::  Lt   !dB, transmitt line loss
	 type(YMM8r4_t) ::  h    !m, apperture height
	 type(YMM8r4_t) ::  ha   !m, phase centre
	 type(YMM8r4_t) ::  Frdr !range dependent response
	 type(YMM8r4_t) ::  Dx   !dB, detectibility factor
	 type(YMM8r4_t) ::  Bt   !Mhz, tuneable bandwidth
	 type(YMM8r4_t) ::  Flen !dB, tropospheric attenuation  
     end type RadarParamSIMD_R4_8


     type, public :: RadarParamSIMD_R8_4

         type(YMM4r8_t) ::  gamm !m, wavelength
	 type(YMM4r8_t) ::  tf   !sec, coherent processing time
	 type(YMM4r8_t) ::  rho  !usec,pulsewidth
	 type(YMM4r8_t) ::  w    !m, apperture width
         type(YMM4r8_t) ::  Kth  !beamwidth constant
	 type(YMM4r8_t) ::  Ln   !pattern constant
	 type(YMM4r8_t) ::  Ts   !K, system noise temperature
	 type(YMM4r8_t) ::  Fp   !polarization factor
	 type(YMM4r8_t) ::  La   !dB, troposepheric attenuation
	 type(YMM4r8_t) ::  F    !radar pattern propagation factor
	 type(YMM4r8_t) ::  Pt   !kW, transmitter power
	 type(YMM4r8_t) ::  tr   !usec, PRI
	 type(YMM4r8_t) ::  Lt   !dB, transmitt line loss
	 type(YMM4r8_t) ::  h    !m, apperture height
	 type(YMM4r8_t) ::  ha   !m, phase centre
	 type(YMM4r8_t) ::  Frdr !range dependent response
	 type(YMM4r8_t) ::  Dx   !dB, detectibility factor
	 type(YMM4r8_t) ::  Bt   !Mhz, tuneable bandwidth
	 type(YMM4r8_t) ::  Flen !dB, tropospheric attenuation  
     end type RadarParamSIMD_R8_4


    


     type, public :: RadarParamSIMD_R4_16

         type(ZMM16r4_t) ::  gamm !m, wavelength
	 type(ZMM16r4_t) ::  tf   !sec, coherent processing time
	 type(ZMM16r4_t) ::  rho  !usec,pulsewidth
	 type(ZMM16r4_t) ::  w    !m, apperture width
         type(ZMM16r4_t) ::  Kth  !beamwidth constant
	 type(ZMM16r4_t) ::  Ln   !pattern constant
	 type(ZMM16r4_t) ::  Ts   !K, system noise temperature
	 type(ZMM16r4_t) ::  Fp   !polarization factor
	 type(ZMM16r4_t) ::  La   !dB, troposepheric attenuation
	 type(ZMM16r4_t) ::  F    !radar pattern propagation factor
	 type(ZMM16r4_t) ::  Pt   !kW, transmitter power
	 type(ZMM16r4_t) ::  tr   !usec, PRI
	 type(ZMM16r4_t) ::  Lt   !dB, transmitt line loss
	 type(ZMM16r4_t) ::  h    !m, apperture height
	 type(ZMM16r4_t) ::  ha   !m, phase centre
	 type(ZMM16r4_t) ::  Frdr !range dependent response
	 type(ZMM16r4_t) ::  Dx   !dB, detectibility factor
	 type(ZMM16r4_t) ::  Bt   !Mhz, tuneable bandwidth
	 type(ZMM16r4_t) ::  Flen !dB, tropospheric attenuation  
     end type RadarParamSIMD_R4_16


     type, public :: RadarParamSIMD_R8_8

         type(ZMM8r8_t) ::  gamm !m, wavelength
	 type(ZMM8r8_t) ::  tf   !sec, coherent processing time
	 type(ZMM8r8_t) ::  rho  !usec,pulsewidth
	 type(ZMM8r8_t) ::  w    !m, apperture width
         type(ZMM8r8_t) ::  Kth  !beamwidth constant
	 type(ZMM8r8_t) ::  Ln   !pattern constant
	 type(ZMM8r8_t) ::  Ts   !K, system noise temperature
	 type(ZMM8r8_t) ::  Fp   !polarization factor
	 type(ZMM8r8_t) ::  La   !dB, troposepheric attenuation
	 type(ZMM8r8_t) ::  F    !radar pattern propagation factor
	 type(ZMM8r8_t) ::  Pt   !kW, transmitter power
	 type(ZMM8r8_t) ::  tr   !usec, PRI
	 type(ZMM8r8_t) ::  Lt   !dB, transmitt line loss
	 type(ZMM8r8_t) ::  h    !m, apperture height
	 type(ZMM8r8_t) ::  ha   !m, phase centre
	 type(ZMM8r8_t) ::  Frdr !range dependent response
	 type(ZMM8r8_t) ::  Dx   !dB, detectibility factor
	 type(ZMM8r8_t) ::  Bt   !Mhz, tuneable bandwidth
	 type(ZMM8r8_t) ::  Flen !dB, tropospheric attenuation  
     end type RadarParamSIMD_R8_8



end module radar_types
