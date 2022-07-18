

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
     ! Tab:1011 col - Type  function and subroutine code blocks.
     use mod_kinds,      only : i4,sp,dp
     use ISO_C_BINDING,  only : c_char
     use mod_vectypes,   only : YMM8r4_tYMM4r8_tZMM16r4_tZMM8r8_t
     implicit none
     public

      !=====================================================59
     !  File and module information:
     !  version creation and build date author description
     !=====================================================59

     ! Major version
     integer(kind=i4) parameter :: RADAR_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4) parameter :: RADAR_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4) parameter :: RADAR_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4) parameter :: RADAR_TYPES_FULLVER = &
          1000*RADAR_TYPES_MAJOR+100*RADAR_TYPES_MINOR+10*RADAR_TYPES_MICRO
     ! Module creation date
     character(*)       parameter :: RADAR_TYPES_CREATION_DATE = "05-04-2022 14:05 +00200 (TUE 05 APR 2022 GMT+2)"
     ! Module build date
     character(*)       parameter :: RADAR_TYPES_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*)       parameter :: RADAR_TYPES_AUTHOR        = "Programmer: Bernard Gingold contact: beniekg@gmail.com"
     ! Short description
     character(*)       parameter :: RADAR_TYPES_SYNOPSIS      = "Radar simulation formulae helper data-types."

#if !defined (RADAR_TYPES_DATA_PAD)
#define RADAR_TYPES_DATA_PAD 1
#define PADDING(nbytes) character(kind=c_char), dimension(nbytes) :: pad
#endif

     type, public :: RadarParamAoS_R4_1
        sequence
        real(kind=sp) ::  gamm !m wavelength
        real(kind=sp) ::  tf   !sec coherent processing time
        real(kind=sp) ::  rho  !usec pulse width
        real(kind=sp) ::  w    !m apperture width
        real(kind=sp) ::  Kth  !beamwidth constant
        real(kind=sp) ::  Ln   !pattern constant
        real(kind=sp) ::  Ts   !K system noise temperature
	real(kind=sp) ::  Fp   !polarization factor
	real(kind=sp) ::  La   !dB troposepheric attenuation
	real(kind=sp) ::  F    !radar pattern propagation factor
	real(kind=sp) ::  Pt   !kW transmitter power
	real(kind=sp) ::  tr   !usec PRI
	real(kind=sp) ::  Lt   !dB transmitt line loss
	real(kind=sp) ::  h    !m apperture height
	real(kind=sp) ::  ha   !m phase centre
	real(kind=sp) ::  Frdr !range dependent response
	real(kind=sp) ::  Dx   !dB detectibility factor
	real(kind=sp) ::  Bt   !Mhz tuneable bandwidth
        real(kind=sp) ::  Flen !dB tropospheric attenuation
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(52)	  
#endif
     end type RadarParamAoS_R4_1
     

     type, public :: RadarParamAoS_R8_1
        sequence
        real(kind=dp) ::  gamm !m wavelength
        real(kind=dp) ::  tf   !sec coherent processing time
        real(kind=dp) ::  rho  !usec pulse width
        real(kind=dp) ::  w    !m apperture width
        real(kind=dp) ::  Kth  !beamwidth constant
        real(kind=dp) ::  Ln   !pattern constant
        real(kind=dp) ::  Ts   !K system noise temperature
	real(kind=dp) ::  Fp   !polarization factor
	real(kind=dp) ::  La   !dB troposepheric attenuation
	real(kind=dp) ::  F    !radar pattern propagation factor
	real(kind=dp) ::  Pt   !kW transmitter power
	real(kind=dp) ::  tr   !usec PRI
	real(kind=dp) ::  Lt   !dB transmitt line loss
	real(kind=dp) ::  h    !m apperture height
	real(kind=dp) ::  ha   !m phase centre
	real(kind=dp) ::  Frdr !range dependent response
	real(kind=dp) ::  Dx   !dB detectibility factor
	real(kind=dp) ::  Bt   !Mhz tuneable bandwidth
        real(kind=dp) ::  Flen !dB tropospheric attenuation
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(52)	  
#endif
     end type RadarParamAoS_R8_1 


     type, public :: RadarParamSoA_R4
        real(kind=sp), dimension(:), allocatable  ::  gamm !m wavelength
        real(kind=sp), dimension(:), allocatable  ::  tf   !sec coherent processing time
        real(kind=sp), dimension(:), allocatable  ::  rho  !usec pulse width
        real(kind=sp), dimension(:), allocatable  ::  w    !m apperture width
        real(kind=sp), dimension(:), allocatable  ::  Kth  !beamwidth constant
        real(kind=sp), dimension(:), allocatable  ::  Ln   !pattern constant
        real(kind=sp), dimension(:), allocatable  ::  Ts   !K system noise temperature
	real(kind=sp), dimension(:), allocatable  ::  Fp   !polarization factor
	real(kind=sp), dimension(:), allocatable  ::  La   !dB troposepheric attenuation
	real(kind=sp), dimension(:), allocatable  ::  F    !radar pattern propagation factor
	real(kind=sp), dimension(:), allocatable  ::  Pt   !kW transmitter power
	real(kind=sp), dimension(:), allocatable  ::  tr   !usec PRI
	real(kind=sp), dimension(:), allocatable  ::  Lt   !dB transmitt line loss
	real(kind=sp), dimension(:), allocatable  ::  h    !m apperture height
	real(kind=sp), dimension(:), allocatable  ::  ha   !m phase centre
	real(kind=sp), dimension(:), allocatable  ::  Frdr !range dependent response
	real(kind=sp), dimension(:), allocatable  ::  Dx   !dB detectibility factor
	real(kind=sp), dimension(:), allocatable  ::  Bt   !Mhz tuneable bandwidth
        real(kind=sp), dimension(:), allocatable  ::  Flen !dB tropospheric attenuation
        !dir$ attributes align : 64 :: gamm
        !dir$ attributes align : 64 :: tf
        !dir$ attributes align : 64 :: rho
        !dir$ attributes align : 64 :: w
        !dir$ attributes align : 64 :: Kth
        !dir$ attributes align : 64 :: Ln
        !dir$ attributes align : 64 :: Ts
        !dir$ attributes align : 64 :: Fp
        !dir$ attributes align : 64 :: La
        !dir$ attributes align : 64 :: F
        !dir$ attributes align : 64 :: Pt
        !dir$ attributes align : 64 :: tr
        !dir$ attributes align : 64 :: Lt
        !dir$ attributes align : 64 :: h
        !dir$ attributes align : 64 :: ha
        !dir$ attributes align : 64 :: Frdr
        !dir$ attributes align : 64 :: Dx
        !dir$ attributes align : 64 :: Bt
        !dir$ attributes align : 64 :: Flen
        logical(kind=i4)                          :: is_alloc
     end type RadarParamSoA_R4


     type, public :: RadarParamSoA_R8
        real(kind=dp), dimension(:), allocatable  ::  gamm !m wavelength
        real(kind=dp), dimension(:), allocatable  ::  tf   !sec coherent processing time
        real(kind=dp), dimension(:), allocatable  ::  rho  !usec pulse width
        real(kind=dp), dimension(:), allocatable  ::  w    !m apperture width
        real(kind=dp), dimension(:), allocatable  ::  Kth  !beamwidth constant
        real(kind=dp), dimension(:), allocatable  ::  Ln   !pattern constant
        real(kind=dp), dimension(:), allocatable  ::  Ts   !K system noise temperature
	real(kind=dp), dimension(:), allocatable  ::  Fp   !polarization factor
	real(kind=dp), dimension(:), allocatable  ::  La   !dB troposepheric attenuation
	real(kind=dp), dimension(:), allocatable  ::  F    !radar pattern propagation factor
	real(kind=dp), dimension(:), allocatable  ::  Pt   !kW transmitter power
	real(kind=dp), dimension(:), allocatable  ::  tr   !usec PRI
	real(kind=dp), dimension(:), allocatable  ::  Lt   !dB transmitt line loss
	real(kind=dp), dimension(:), allocatable  ::  h    !m apperture height
	real(kind=dp), dimension(:), allocatable  ::  ha   !m phase centre
	real(kind=dp), dimension(:), allocatable  ::  Frdr !range dependent response
	real(kind=dp), dimension(:), allocatable  ::  Dx   !dB detectibility factor
	real(kind=dp), dimension(:), allocatable  ::  Bt   !Mhz tuneable bandwidth
        real(kind=dp), dimension(:), allocatable  ::  Flen !dB tropospheric attenuation
        !dir$ attributes align : 64 :: gamm
        !dir$ attributes align : 64 :: tf
        !dir$ attributes align : 64 :: rho
        !dir$ attributes align : 64 :: w
        !dir$ attributes align : 64 :: Kth
        !dir$ attributes align : 64 :: Ln
        !dir$ attributes align : 64 :: Ts
        !dir$ attributes align : 64 :: Fp
        !dir$ attributes align : 64 :: La
        !dir$ attributes align : 64 :: F
        !dir$ attributes align : 64 :: Pt
        !dir$ attributes align : 64 :: tr
        !dir$ attributes align : 64 :: Lt
        !dir$ attributes align : 64 :: h
        !dir$ attributes align : 64 :: ha
        !dir$ attributes align : 64 :: Frdr
        !dir$ attributes align : 64 :: Dx
        !dir$ attributes align : 64 :: Bt
        !dir$ attributes align : 64 :: Flen
        logical(kind=i4)                            :: isalloc
     end type RadarParamSoA_R8



     type, public :: JammerParamAoS_R4_1
         sequence
         real(kind=sp) :: sig  !m RSC of target
	 real(kind=sp) :: Pj   !W jammer power
	 real(kind=sp) :: Gj   !dB jammer antenna gain
	 real(kind=sp) :: Qj   !dB jammer noise quality
         real(kind=sp) :: Flenj!dB jammer lens factor
	 real(kind=sp) :: Rj   !km jammer range
	 real(kind=sp) :: Bj   !Mhzjammer noise BW
	 real(kind=sp) :: Ltj  !dB jammer transmit loss
	 real(kind=sp) :: Fpj  !dB jammer polarization
	 real(kind=sp) :: Rmj  !km jammer screening range
	 real(kind=sp) :: Fj   !dB jammer pattern factor of propagation
	 real(kind=sp) :: Laj  !dB jammer troposhperic loss
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(16)	  
#endif
     end type JammerParamAoS_R4_1

  
     type, public :: JammerParamAoS_R8_1
         sequence
         real(kind=dp) :: sig  !m RSC of target
	 real(kind=dp) :: Pj   !W jammer power
	 real(kind=dp) :: Gj   !dB jammer antenna gain
	 real(kind=dp) :: Qj   !dB jammer noise quality
         real(kind=dp) :: Flenj!dB jammer lens factor
	 real(kind=dp) :: Rj   !km jammer range
	 real(kind=dp) :: Bj   !Mhzjammer noise BW
	 real(kind=dp) :: Ltj  !dB jammer transmit loss
	 real(kind=dp) :: Fpj  !dB jammer polarization
	 real(kind=dp) :: Rmj  !km jammer screening range
	 real(kind=dp) :: Fj   !dB jammer pattern factor of propagation
	 real(kind=dp) :: Laj  !dB jammer troposhperic loss
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(32)	  
#endif
     end type JammerParamAoS_R8_1


     type, public :: JammerParamSoA_R4
         sequence
         real(kind=sp), dimension(:), allocatable  :: sig  !m RSC of target
	 real(kind=sp), dimension(:), allocatable  :: Pj   !W jammer power
	 real(kind=sp), dimension(:), allocatable  :: Gj   !dB jammer antenna gain
	 real(kind=sp), dimension(:), allocatable  :: Qj   !dB jammer noise quality
         real(kind=sp), dimension(:), allocatable  :: Flenj!dB jammer lens factor
	 real(kind=sp), dimension(:), allocatable  :: Rj   !km jammer range
	 real(kind=sp), dimension(:), allocatable  :: Bj   !Mhzjammer noise BW
	 real(kind=sp), dimension(:), allocatable  :: Ltj  !dB jammer transmit loss
	 real(kind=sp), dimension(:), allocatable  :: Fpj  !dB jammer polarization
	 real(kind=sp), dimension(:), allocatable  :: Rmj  !km jammer screening range
	 real(kind=sp), dimension(:), allocatable  :: Fj   !dB jammer pattern factor of propagation
	 real(kind=sp), dimension(:), allocatable  :: Laj  !dB jammer troposhperic loss
         !dir$ attributes align : 64 :: sig
         !dir$ attributes align : 64 :: Pj
         !dir$ attributes align : 64 :: Gj
         !dir$ attributes align : 64 :: Qj
         !dir$ attributes align : 64 :: Flenj
         !dir$ attributes align : 64 :: Rj
         !dir$ attributes align : 64 :: Bj
         !dir$ attributes align : 64 :: Ltj
         !dir$ attributes align : 64 :: Fpj
         !dir$ attributes align : 64 :: Rmj
         !dir$ attributes align : 64 :: Fj
         !dir$ attributes align : 64 :: Laj
         logical(kind=i4)                          :: isalloc
     end type JammerParamSoA_R4


     type, public :: JammerParamSoA_R8
         sequence
         real(kind=dp), dimension(:), allocatable  :: sig  !m RSC of target
	 real(kind=dp), dimension(:), allocatable  :: Pj   !W jammer power
	 real(kind=dp), dimension(:), allocatable  :: Gj   !dB jammer antenna gain
	 real(kind=dp), dimension(:), allocatable  :: Qj   !dB jammer noise quality
         real(kind=dp), dimension(:), allocatable  :: Flenj!dB jammer lens factor
	 real(kind=dp), dimension(:), allocatable  :: Rj   !km jammer range
	 real(kind=dp), dimension(:), allocatable  :: Bj   !Mhzjammer noise BW
	 real(kind=dp), dimension(:), allocatable  :: Ltj  !dB jammer transmit loss
	 real(kind=dp), dimension(:), allocatable  :: Fpj  !dB jammer polarization
	 real(kind=dp), dimension(:), allocatable  :: Rmj  !km jammer screening range
	 real(kind=dp), dimension(:), allocatable  :: Fj   !dB jammer pattern factor of propagation
	 real(kind=dp), dimension(:), allocatable  :: Laj  !dB jammer troposhperic loss
         !dir$ attributes align : 64 :: sig
         !dir$ attributes align : 64 :: Pj
         !dir$ attributes align : 64 :: Gj
         !dir$ attributes align : 64 :: Qj
         !dir$ attributes align : 64 :: Flenj
         !dir$ attributes align : 64 :: Rj
         !dir$ attributes align : 64 :: Bj
         !dir$ attributes align : 64 :: Ltj
         !dir$ attributes align : 64 :: Fpj
         !dir$ attributes align : 64 :: Rmj
         !dir$ attributes align : 64 :: Fj
         !dir$ attributes align : 64 :: Laj
         logical(kind=i4)                          :: isalloc
     end type JammerParamSoA_R8

     
     type, public :: RadarParamSIMD_R4_8
         sequence
         type(YMM8r4_t) ::  gamm !m wavelength
	 type(YMM8r4_t) ::  tf   !sec coherent processing time
	 type(YMM8r4_t) ::  rho  !usecpulsewidth
	 type(YMM8r4_t) ::  w    !m apperture width
         type(YMM8r4_t) ::  Kth  !beamwidth constant
	 type(YMM8r4_t) ::  Ln   !pattern constant
	 type(YMM8r4_t) ::  Ts   !K system noise temperature
	 type(YMM8r4_t) ::  Fp   !polarization factor
	 type(YMM8r4_t) ::  La   !dB troposepheric attenuation
	 type(YMM8r4_t) ::  F    !radar pattern propagation factor
	 type(YMM8r4_t) ::  Pt   !kW transmitter power
	 type(YMM8r4_t) ::  tr   !usec PRI
	 type(YMM8r4_t) ::  Lt   !dB transmitt line loss
	 type(YMM8r4_t) ::  h    !m apperture height
	 type(YMM8r4_t) ::  ha   !m phase centre
	 type(YMM8r4_t) ::  Frdr !range dependent response
	 type(YMM8r4_t) ::  Dx   !dB detectibility factor
	 type(YMM8r4_t) ::  Bt   !Mhz tuneable bandwidth
	 type(YMM8r4_t) ::  Flen !dB tropospheric attenuation  
     end type RadarParamSIMD_R4_8


     type, public :: RadarParamSIMD_R8_4
         sequence
         type(YMM4r8_t) ::  gamm !m wavelength
	 type(YMM4r8_t) ::  tf   !sec coherent processing time
	 type(YMM4r8_t) ::  rho  !usecpulsewidth
	 type(YMM4r8_t) ::  w    !m apperture width
         type(YMM4r8_t) ::  Kth  !beamwidth constant
	 type(YMM4r8_t) ::  Ln   !pattern constant
	 type(YMM4r8_t) ::  Ts   !K system noise temperature
	 type(YMM4r8_t) ::  Fp   !polarization factor
	 type(YMM4r8_t) ::  La   !dB troposepheric attenuation
	 type(YMM4r8_t) ::  F    !radar pattern propagation factor
	 type(YMM4r8_t) ::  Pt   !kW transmitter power
	 type(YMM4r8_t) ::  tr   !usec PRI
	 type(YMM4r8_t) ::  Lt   !dB transmitt line loss
	 type(YMM4r8_t) ::  h    !m apperture height
	 type(YMM4r8_t) ::  ha   !m phase centre
	 type(YMM4r8_t) ::  Frdr !range dependent response
	 type(YMM4r8_t) ::  Dx   !dB detectibility factor
	 type(YMM4r8_t) ::  Bt   !Mhz tuneable bandwidth
	 type(YMM4r8_t) ::  Flen !dB tropospheric attenuation  
     end type RadarParamSIMD_R8_4


    


     type, public :: RadarParamSIMD_R4_16
         sequence
         type(ZMM16r4_t) ::  gamm !m wavelength
	 type(ZMM16r4_t) ::  tf   !sec coherent processing time
	 type(ZMM16r4_t) ::  rho  !usecpulsewidth
	 type(ZMM16r4_t) ::  w    !m apperture width
         type(ZMM16r4_t) ::  Kth  !beamwidth constant
	 type(ZMM16r4_t) ::  Ln   !pattern constant
	 type(ZMM16r4_t) ::  Ts   !K system noise temperature
	 type(ZMM16r4_t) ::  Fp   !polarization factor
	 type(ZMM16r4_t) ::  La   !dB troposepheric attenuation
	 type(ZMM16r4_t) ::  F    !radar pattern propagation factor
	 type(ZMM16r4_t) ::  Pt   !kW transmitter power
	 type(ZMM16r4_t) ::  tr   !usec PRI
	 type(ZMM16r4_t) ::  Lt   !dB transmitt line loss
	 type(ZMM16r4_t) ::  h    !m apperture height
	 type(ZMM16r4_t) ::  ha   !m phase centre
	 type(ZMM16r4_t) ::  Frdr !range dependent response
	 type(ZMM16r4_t) ::  Dx   !dB detectibility factor
	 type(ZMM16r4_t) ::  Bt   !Mhz tuneable bandwidth
	 type(ZMM16r4_t) ::  Flen !dB tropospheric attenuation  
     end type RadarParamSIMD_R4_16


     type, public :: RadarParamSIMD_R8_8
         sequence
         type(ZMM8r8_t) ::  gamm !m wavelength
	 type(ZMM8r8_t) ::  tf   !sec coherent processing time
	 type(ZMM8r8_t) ::  rho  !usecpulsewidth
	 type(ZMM8r8_t) ::  w    !m apperture width
         type(ZMM8r8_t) ::  Kth  !beamwidth constant
	 type(ZMM8r8_t) ::  Ln   !pattern constant
	 type(ZMM8r8_t) ::  Ts   !K system noise temperature
	 type(ZMM8r8_t) ::  Fp   !polarization factor
	 type(ZMM8r8_t) ::  La   !dB troposepheric attenuation
	 type(ZMM8r8_t) ::  F    !radar pattern propagation factor
	 type(ZMM8r8_t) ::  Pt   !kW transmitter power
	 type(ZMM8r8_t) ::  tr   !usec PRI
	 type(ZMM8r8_t) ::  Lt   !dB transmitt line loss
	 type(ZMM8r8_t) ::  h    !m apperture height
	 type(ZMM8r8_t) ::  ha   !m phase centre
	 type(ZMM8r8_t) ::  Frdr !range dependent response
	 type(ZMM8r8_t) ::  Dx   !dB detectibility factor
	 type(ZMM8r8_t) ::  Bt   !Mhz tuneable bandwidth
	 type(ZMM8r8_t) ::  Flen !dB tropospheric attenuation  
     end type RadarParamSIMD_R8_8


    type, public ::  JammerParamSIMD_R4_8

            type(YMM8r4_t) ::  sig  !m RSC of target
	    type(YMM8r4_t) ::  Pj   !W !jammer power
	    type(YMM8r4_t) ::  Gj   !dB !jammer antenna gain
	    type(YMM8r4_t) ::  Qj   !dB !jammer noise quality
	    type(YMM8r4_t) ::  Flenj !dB jammer lens factor
	    type(YMM8r4_t) ::  Rj   !km !jammer range
	    type(YMM8r4_t) ::  Bj   !Mhz !jammer noise BW
	    type(YMM8r4_t) ::  Ltj  !dB !jammer transmit loss
	    type(YMM8r4_t) ::  Fpj  !dB !jammer polarization
	    type(YMM8r4_t) ::  Rmj  !km !jammer screening range
	    type(YMM8r4_t) ::  Fj   !dB !jammer pattern factor of propagation
	    type(YMM8r4_t) ::  Laj  !dB !jammer troposhperic loss
     end type  JammerParamSIMD_R4_8


     type, public ::  JammerParamSIMD_R8_4

            type(YMM4r8_t) ::  sig  !m RSC of target
	    type(YMM4r8_t) ::  Pj   !W !jammer power
	    type(YMM4r8_t) ::  Gj   !dB !jammer antenna gain
	    type(YMM4r8_t) ::  Qj   !dB !jammer noise quality
	    type(YMM4r8_t) ::  Flenj !dB jammer lens factor
	    type(YMM4r8_t) ::  Rj   !km !jammer range
	    type(YMM4r8_t) ::  Bj   !Mhz !jammer noise BW
	    type(YMM4r8_t) ::  Ltj  !dB !jammer transmit loss
	    type(YMM4r8_t) ::  Fpj  !dB !jammer polarization
	    type(YMM4r8_t) ::  Rmj  !km !jammer screening range
	    type(YMM4r8_t) ::  Fj   !dB !jammer pattern factor of propagation
	    type(YMM4r8_t) ::  Laj  !dB !jammer troposhperic loss
     end type  JammerParamSIMD_R8_4


     type, public ::  JammerParamSIMD_R4_8

            type(YMM8r4_t) ::  sig  !m RSC of target
	    type(YMM8r4_t) ::  Pj   !W !jammer power
	    type(YMM8r4_t) ::  Gj   !dB !jammer antenna gain
	    type(YMM8r4_t) ::  Qj   !dB !jammer noise quality
	    type(YMM8r4_t) ::  Flenj !dB jammer lens factor
	    type(YMM8r4_t) ::  Rj   !km !jammer range
	    type(YMM8r4_t) ::  Bj   !Mhz !jammer noise BW
	    type(YMM8r4_t) ::  Ltj  !dB !jammer transmit loss
	    type(YMM8r4_t) ::  Fpj  !dB !jammer polarization
	    type(YMM8r4_t) ::  Rmj  !km !jammer screening range
	    type(YMM8r4_t) ::  Fj   !dB !jammer pattern factor of propagation
	    type(YMM8r4_t) ::  Laj  !dB !jammer troposhperic loss
     end type  JammerParamSIMD_R4_8

 
     type, public ::  JammerParamSIMD_R4_16

            type(ZMM16r4_t) ::  sig  !m RSC of target
	    type(ZMM16r4_t)  ::  Pj   !W !jammer power
	    type(ZMM16r4_t)  ::  Gj   !dB !jammer antenna gain
	    type(ZMM16r4_t)  ::  Qj   !dB !jammer noise quality
	    type(ZMM16r4_t)  ::  Flenj !dB jammer lens factor
	    type(ZMM16r4_t)  ::  Rj   !km !jammer range
	    type(ZMM16r4_t)  ::  Bj   !Mhz !jammer noise BW
	    type(ZMM16r4_t)  ::  Ltj  !dB !jammer transmit loss
	    type(ZMM16r4_t)  ::  Fpj  !dB !jammer polarization
	    type(ZMM16r4_t)  ::  Rmj  !km !jammer screening range
	    type(ZMM16r4_t)  ::  Fj   !dB !jammer pattern factor of propagation
	    type(ZMM16r4_t)  ::  Laj  !dB !jammer troposhperic loss
     end type  JammerParamSIMD_R4_16

   
     type, public ::  JammerParamSIMD_R8_8

            type(ZMM8r8_t) ::  sig  !m RSC of target
	    type(ZMM8r8_t)  ::  Pj   !W !jammer power
	    type(ZMM8r8_t)  ::  Gj   !dB !jammer antenna gain
	    type(ZMM8r8_t)  ::  Qj   !dB !jammer noise quality
	    type(ZMM8r8_t)  ::  Flenj !dB jammer lens factor
	    type(ZMM8r8_t)  ::  Rj   !km !jammer range
	    type(ZMM8r8_t)  ::  Bj   !Mhz !jammer noise BW
	    type(ZMM8r8_t)  ::  Ltj  !dB !jammer transmit loss
	    type(ZMM8r8_t)  ::  Fpj  !dB !jammer polarization
	    type(ZMM8r8_t)  ::  Rmj  !km !jammer screening range
	    type(ZMM8r8_t)  ::  Fj   !dB !jammer pattern factor of propagation
	    type(ZMM8r8_t)  ::  Laj  !dB !jammer troposhperic loss
     end type  JammerParamSIMD_R8_8

   
     !    // Platform dependent errors 
     !    // Calculates the angle and range measurement errors cause
     !    // by the errors in position and orientation of radar platform
     
     type, public :: PlatformErrAoS_R4_1
            sequence
            real(kind=sp) ::  R      !//nm, target range
	    real(kind=sp) ::  psi    !//deg,target azimuth
	    real(kind=sp) ::  theta  !//deg,target elevation
	    real(kind=sp) ::  da1    !//deg, platform error of yaw angle measurement
	    real(kind=sp) ::  da2    !//deg, platform error of pitch angle measurement
	    real(kind=sp) ::  da3    !//deg, platform error of roll angle measurement
	    real(kind=sp) ::  dx1    !//m,   platform center of gravity error x1-axis
	    real(kind=sp) ::  dx2    !//m,   platform center of gravity error x2-axis
	    real(kind=sp) ::  dx3    !//m,   platform center of gravity erorr x3-axis
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(28)	  
#endif
     end type PlatformErrAoS_R4_1


     type, public :: PlatformErrAoS_R8_1
            sequence
            real(kind=dp) ::  R      !//nm, target range
	    real(kind=dp) ::  psi    !//deg,target azimuth
	    real(kind=dp) ::  theta  !//deg,target elevation
	    real(kind=dp) ::  da1    !//deg, platform error of yaw angle measurement
	    real(kind=dp) ::  da2    !//deg, platform error of pitch angle measurement
	    real(kind=dp) ::  da3    !//deg, platform error of roll angle measurement
	    real(kind=dp) ::  dx1    !//m,   platform center of gravity error x1-axis
	    real(kind=dp) ::  dx2    !//m,   platform center of gravity error x2-axis
	    real(kind=dp) ::  dx3    !//m,   platform center of gravity erorr x3-axis
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(56)	  
#endif
     end type PlatformErrAoS_R8_1


     type, public :: PlatformErrSoA_R4
            sequence
            real(kind=sp), dimension(:), allocatable ::  R      !//nm, target range
	    real(kind=sp), dimension(:), allocatable ::  psi    !//deg,target azimuth
	    real(kind=sp), dimension(:), allocatable ::  theta  !//deg,target elevation
	    real(kind=sp), dimension(:), allocatable ::  da1    !//deg, platform error of yaw angle measurement
	    real(kind=sp), dimension(:), allocatable ::  da2    !//deg, platform error of pitch angle measurement
	    real(kind=sp), dimension(:), allocatable ::  da3    !//deg, platform error of roll angle measurement
	    real(kind=sp), dimension(:), allocatable ::  dx1    !//m,   platform center of gravity error x1-axis
	    real(kind=sp), dimension(:), allocatable ::  dx2    !//m,   platform center of gravity error x2-axis
	    real(kind=sp), dimension(:), allocatable ::  dx3    !//m,   platform center of gravity erorr x3-axis
            !dir$ attributes align : 64 :: R
            !dir$ attributes align : 64 :: psi
            !dir$ attributes align : 64 :: theta
            !dir$ attributes align : 64 :: da1
            !dir$ attributes align : 64 :: da2
            !dir$ attributes align : 64 :: da3
            !dir$ attributes align : 64 :: dx1
            !dir$ attributes align : 64 :: dx2
            !dir$ attributes align : 64 :: dx3
            logical(kind=i4)                         :: isalloc
     end type PlatformErrSoA_R4


     type, public :: PlatformErrSoA_R8
            sequence
            real(kind=dp), dimension(:), allocatable ::  R      !//nm, target range
	    real(kind=dp), dimension(:), allocatable ::  psi    !//deg,target azimuth
	    real(kind=dp), dimension(:), allocatable ::  theta  !//deg,target elevation
	    real(kind=dp), dimension(:), allocatable ::  da1    !//deg, platform error of yaw angle measurement
	    real(kind=dp), dimension(:), allocatable ::  da2    !//deg, platform error of pitch angle measurement
	    real(kind=dp), dimension(:), allocatable ::  da3    !//deg, platform error of roll angle measurement
	    real(kind=dp), dimension(:), allocatable ::  dx1    !//m,   platform center of gravity error x1-axis
	    real(kind=dp), dimension(:), allocatable ::  dx2    !//m,   platform center of gravity error x2-axis
	    real(kind=dp), dimension(:), allocatable ::  dx3    !//m,   platform center of gravity erorr x3-axis
            !dir$ attributes align : 64 :: R
            !dir$ attributes align : 64 :: psi
            !dir$ attributes align : 64 :: theta
            !dir$ attributes align : 64 :: da1
            !dir$ attributes align : 64 :: da2
            !dir$ attributes align : 64 :: da3
            !dir$ attributes align : 64 :: dx1
            !dir$ attributes align : 64 :: dx2
            !dir$ attributes align : 64 :: dx3
            logical(kind=i4)                         :: isalloc
     end type PlatformErrSoA_R8


     type, public :: PlatformErrSIMD_R4_8
            
            type(YMM8r4_t) ::  R      !//nm, target range
	    type(YMM8r4_t) ::  psi    !//deg,target azimuth
	    type(YMM8r4_t) ::  theta  !//deg,target elevation
	    type(YMM8r4_t) ::  da1    !//deg, platform error of yaw angle measurement
	    type(YMM8r4_t) ::  da2    !//deg, platform error of pitch angle measurement
	    type(YMM8r4_t) ::  da3    !//deg, platform error of roll angle measurement
	    type(YMM8r4_t) ::  dx1    !//m,   platform center of gravity error x1-axis
	    type(YMM8r4_t) ::  dx2    !//m,   platform center of gravity error x2-axis
	    type(YMM8r4_t) ::  dx3    !//m,   platform center of gravity erorr x3-axis
     end type PlatformErrSIMD_R4_8


     type, public :: PlatformErrSIMD_R8_4
            
            type(YMM4r8_t) ::  R      !//nm, target range
	    type(YMM4r8_t) ::  psi    !//deg,target azimuth
	    type(YMM4r8_t) ::  theta  !//deg,target elevation
	    type(YMM4r8_t) ::  da1    !//deg, platform error of yaw angle measurement
	    type(YMM4r8_t) ::  da2    !//deg, platform error of pitch angle measurement
	    type(YMM4r8_t) ::  da3    !//deg, platform error of roll angle measurement
	    type(YMM4r8_t) ::  dx1    !//m,   platform center of gravity error x1-axis
	    type(YMM4r8_t) ::  dx2    !//m,   platform center of gravity error x2-axis
	    type(YMM4r8_t) ::  dx3    !//m,   platform center of gravity erorr x3-axis
     end type PlatformErrSIMD_R8_4


     type, public :: PlatformErrSIMD_R4_16
            
            type(ZMM16r4_t) ::  R      !//nm, target range
	    type(ZMM16r4_t) ::  psi    !//deg,target azimuth
	    type(ZMM16r4_t) ::  theta  !//deg,target elevation
	    type(ZMM16r4_t) ::  da1    !//deg, platform error of yaw angle measurement
	    type(ZMM16r4_t) ::  da2    !//deg, platform error of pitch angle measurement
	    type(ZMM16r4_t) ::  da3    !//deg, platform error of roll angle measurement
	    type(ZMM16r4_t) ::  dx1    !//m,   platform center of gravity error x1-axis
	    type(ZMM16r4_t) ::  dx2    !//m,   platform center of gravity error x2-axis
	    type(ZMM16r4_t) ::  dx3    !//m,   platform center of gravity erorr x3-axis
     end type PlatformErrSIMD_R4_16


     type, public :: PlatformErrSIMD_R8_8
            
            type(ZMM8r8_t) ::  R      !//nm, target range
	    type(ZMM8r8_t) ::  psi    !//deg,target azimuth
	    type(ZMM8r8_t) ::  theta  !//deg,target elevation
	    type(ZMM8r8_t) ::  da1    !//deg, platform error of yaw angle measurement
	    type(ZMM8r8_t) ::  da2    !//deg, platform error of pitch angle measurement
	    type(ZMM8r8_t) ::  da3    !//deg, platform error of roll angle measurement
	    type(ZMM8r8_t) ::  dx1    !//m,   platform center of gravity error x1-axis
	    type(ZMM8r8_t) ::  dx2    !//m,   platform center of gravity error x2-axis
	    type(ZMM8r8_t) ::  dx3    !//m,   platform center of gravity erorr x3-axis
     end type PlatformErrSIMD_R8_8


     !  // Propagation errors aggregating data type

     type, public :: PropagationErrAoS_R4_1
           sequence
           real(kind=sp) ::  R             !//nm, target range
	   real(kind=sp) ::  ht            !//m,  target height above surface
	   real(kind=sp) ::  ha            !//m,  antenna height above surface
           real(kind=sp) ::  psi0          !//    Fresnel reflection coefficient
	   real(kind=sp) ::  psis          !//    specular scattering coefficient
	   real(kind=sp) ::  psiv          !//    vegetation coefficient
	   real(kind=sp) ::  beta          !//deg,surface slope
	   real(kind=sp) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   real(kind=sp) ::  thmax         !//deg,beam axis elevation angle
	   real(kind=sp) ::  kme           !//    tracking error slope in elevation channel
	   real(kind=sp) ::  Ns            !//    refractivity at the radar site
	   integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(16)	  
#endif           
     end type PropagationErrAoS_R4_1


     type, public :: PropagationErrAoS_R8_1
           sequence
           real(kind=dp) ::  R             !//nm, target range
	   real(kind=dp) ::  ht            !//m,  target height above surface
	   real(kind=dp) ::  ha            !//m,  antenna height above surface
           real(kind=dp) ::  psi0          !//    Fresnel reflection coefficient
	   real(kind=dp) ::  psis          !//    specular scattering coefficient
	   real(kind=dp) ::  psiv          !//    vegetation coefficient
	   real(kind=dp) ::  beta          !//deg,surface slope
	   real(kind=dp) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   real(kind=dp) ::  thmax         !//deg,beam axis elevation angle
	   real(kind=dp) ::  kme           !//    tracking error slope in elevation channel
	   real(kind=dp) ::  Ns            !//    refractivity at the radar site
	   integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
#if (RADAR_TYPES_DATA_PAD) == 1			   
	   PADDING(36)	  
#endif
     end type PropagationErrAoS_R8_1

     
     type, public :: PropagationErrSoA_R4
           
           real(kind=sp), dimension(:), allocatable ::  R             !//nm, target range
	   real(kind=sp), dimension(:), allocatable ::  ht            !//m,  target height above surface
	   real(kind=sp), dimension(:), allocatable ::  ha            !//m,  antenna height above surface
           real(kind=sp), dimension(:), allocatable ::  psi0          !//    Fresnel reflection coefficient
	   real(kind=sp), dimension(:), allocatable ::  psis          !//    specular scattering coefficient
	   real(kind=sp), dimension(:), allocatable ::  psiv          !//    vegetation coefficient
	   real(kind=sp), dimension(:), allocatable ::  beta          !//deg,surface slope
	   real(kind=sp), dimension(:), allocatable ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   real(kind=sp), dimension(:), allocatable ::  thmax         !//deg,beam axis elevation angle
	   real(kind=sp), dimension(:), allocatable ::  kme           !//    tracking error slope in elevation channel
	   real(kind=sp), dimension(:), allocatable ::  Ns            !//    refractivity at the radar site
           !dir$ attributes align : 64 :: R
           !dir$ attributes align : 64 :: ht
           !dir$ attributes align : 64 :: ha
           !dir$ attributes align : 64 :: psi0
           !dir$ attributes align : 64 :: psis
           !dir$ attributes align : 64 :: psiv
           !dir$ attributes align : 64 :: beta
           !dir$ attributes align : 64 :: th3
           !dir$ attributes align : 64 :: thmax
           !dir$ attributes align : 64 :: kme
           !dir$ attributes align : 64 :: Ns
           logical(kind=i4) :: isalloc
	   integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -

		                           !//    - refractive index
          
     end type PropagationErrSoA_R4


     type, public :: PropagationErrSoA_R8
           
           real(kind=dp), dimension(:), allocatable ::  R             !//nm, target range
	   real(kind=dp), dimension(:), allocatable ::  ht            !//m,  target height above surface
	   real(kind=dp), dimension(:), allocatable ::  ha            !//m,  antenna height above surface
           real(kind=dp), dimension(:), allocatable ::  psi0          !//    Fresnel reflection coefficient
	   real(kind=dp), dimension(:), allocatable ::  psis          !//    specular scattering coefficient
	   real(kind=dp), dimension(:), allocatable ::  psiv          !//    vegetation coefficient
	   real(kind=dp), dimension(:), allocatable ::  beta          !//deg,surface slope
	   real(kind=dp), dimension(:), allocatable ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   real(kind=dp), dimension(:), allocatable ::  thmax         !//deg,beam axis elevation angle
	   real(kind=dp), dimension(:), allocatable ::  kme           !//    tracking error slope in elevation channel
	   real(kind=dp), dimension(:), allocatable ::  Ns            !//    refractivity at the radar site
           !dir$ attributes align : 64 :: R
           !dir$ attributes align : 64 :: ht
           !dir$ attributes align : 64 :: ha
           !dir$ attributes align : 64 :: psi0
           !dir$ attributes align : 64 :: psis
           !dir$ attributes align : 64 :: psiv
           !dir$ attributes align : 64 :: beta
           !dir$ attributes align : 64 :: th3
           !dir$ attributes align : 64 :: thmax
           !dir$ attributes align : 64 :: kme
           !dir$ attributes align : 64 :: Ns
           logical(kind=i4) :: isalloc
	   integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -

		                           !//    - refractive index
          
     end type PropagationErrSoA_R8
     


     type, public :: PropagationErrSIMD_R4_8

           type(YMM8r4_t) ::  R             !//nm, target range
	   type(YMM8r4_t) ::  ht            !//m,  target height above surface
	   type(YMM8r4_t) ::  ha            !//m,  antenna height above surface
           type(YMM8r4_t) ::  psi0          !//    Fresnel reflection coefficient
	   type(YMM8r4_t) ::  psis          !//    specular scattering coefficient
	   type(YMM8r4_t) ::  psiv          !//    vegetation coefficient
	   type(YMM8r4_t) ::  beta          !//deg,surface slope
	   type(YMM8r4_t) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   type(YMM8r4_t) ::  thmax         !//deg,beam axis elevation angle
	   type(YMM8r4_t) ::  kme           !//    tracking error slope in elevation channel
	   type(YMM8r4_t) ::  Ns            !//    refractivity at the radar site
	   !integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
     end type PropagationErrSIMD_R4_8
     

     type, public :: PropagationErrSIMD_R8_4

           type(YMM4r8_t) ::  R             !//nm, target range
	   type(YMM4r8_t) ::  ht            !//m,  target height above surface
	   type(YMM4r8_t) ::  ha            !//m,  antenna height above surface
           type(YMM4r8_t) ::  psi0          !//    Fresnel reflection coefficient
	   type(YMM4r8_t) ::  psis          !//    specular scattering coefficient
	   type(YMM4r8_t) ::  psiv          !//    vegetation coefficient
	   type(YMM4r8_t) ::  beta          !//deg,surface slope
	   type(YMM4r8_t) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   type(YMM4r8_t) ::  thmax         !//deg,beam axis elevation angle
	   type(YMM4r8_t) ::  kme           !//    tracking error slope in elevation channel
	   type(YMM4r8_t) ::  Ns            !//    refractivity at the radar site
	   !integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
     end type PropagationErrSIMD_R8_4


     type, public :: PropagationErrSIMD_R4_16

           type(ZMM16r4_t) ::  R             !//nm, target range
	   type(ZMM16r4_t) ::  ht            !//m,  target height above surface
	   type(ZMM16r4_t) ::  ha            !//m,  antenna height above surface
           type(ZMM16r4_t) ::  psi0          !//    Fresnel reflection coefficient
	   type(ZMM16r4_t) ::  psis          !//    specular scattering coefficient
	   type(ZMM16r4_t) ::  psiv          !//    vegetation coefficient
	   type(ZMM16r4_t) ::  beta          !//deg,surface slope
	   type(ZMM16r4_t) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   type(ZMM16r4_t) ::  thmax         !//deg,beam axis elevation angle
	   type(ZMM16r4_t) ::  kme           !//    tracking error slope in elevation channel
	   type(ZMM16r4_t) ::  Ns            !//    refractivity at the radar site
	   !integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
     end type PropagationErrSIMD_R4_16


     type, public :: PropagationErrSIMD_R8_8

           type(ZMM8r8_t) ::  R             !//nm, target range
	   type(ZMM8r8_t) ::  ht            !//m,  target height above surface
	   type(ZMM8r8_t) ::  ha            !//m,  antenna height above surface
           type(ZMM8r8_t) ::  psi0          !//    Fresnel reflection coefficient
	   type(ZMM8r8_t) ::  psis          !//    specular scattering coefficient
	   type(ZMM8r8_t) ::  psiv          !//    vegetation coefficient
	   type(ZMM8r8_t) ::  beta          !//deg,surface slope
	   type(ZMM8r8_t) ::  th3           !//deg,elevation beamwidth (half-power, i.e. 3db)
	   type(ZMM8r8_t) ::  thmax         !//deg,beam axis elevation angle
	   type(ZMM8r8_t) ::  kme           !//    tracking error slope in elevation channel
	   type(ZMM8r8_t) ::  Ns            !//    refractivity at the radar site
	   !integer(kind=i4) :: flucts      !//    fluctuations switch, 1==low fluctuations,2==average fluctuations,3==high fluctuations in the -
		                           !//    - refractive index
     end type PropagationErrSIMD_R8_8


     
     


end module radar_types
