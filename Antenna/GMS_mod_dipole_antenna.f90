
#include "GMS_config.fpp"

module mod_dipole_antenna


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_dipole_antenna
 !          
 !          Purpose:
 !                        Module data for dipole antenna implementation.
 !                        Various characteristics of different dipole antenna formulae and computational characteristics.
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 26-11-2023
 !                        Time: 04:55 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                      Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б      
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
    use mod_kinds,    only : i4,sp,dp

    public
    implicit none

    ! Major version
    integer(kind=i4),  parameter :: MOD_DIPOLE_ANTENNA_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: MOD_DIPOLE_ANTENNA_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: MOD_DIPOLE_ANTENNA_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: MOD_DIPOLE_ANTENNA_FULLVER =   &
            1000*MOD_DIPOLE_ANTENNA_MAJOR+100*MOD_DIPOLE_ANTENNA_MINOR+10*MOD_DIPOLE_ANTENNA_MICRO
    ! Module creation date
    character(*),        parameter :: MOD_DIPOLE_ANTENNA_CREATE_DATE = "26-11-2023 04:55 +00200 (SUN 26 NOV 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: MOD_DIPOLE_ANTENNA_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: MOD_DIPOLE_ANTENNA_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: MOD_DIPOLE_ANTENNA_SYNOPSIS    = "Data describing various dipole antenna characteristics -- module based."

    
    integer(kind=i4) :: nIzf31 ! number of 'z' values of current distribution of symmetric
                               ! dipole (3.1)
    integer(kind=i4) :: ndf31  ! number of dipoles in dipole array (3.1)
    integer(kind=i4) :: nIzf34 ! number of 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
    integer(kind=i4) :: ndf34  ! number of dipoles in dipole array (3.4)
    integer(kind=i4) :: nFtf35 ! number of 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)
    integer(kind=i4) :: ndf35  ! number of dipoles in dipole array (3.5)
    integer(kind=i4) :: nDf36  ! number of 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
    integer(kind=i4) :: ndf36  ! number of dipoles in dipole array (3.6)
    integer(kind=i4) :: nRf38  ! number of 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)
    integer(kind=i4) :: ndf38  ! number of dipoles in dipole array (3.8)                         
    integer(kind=i4) :: nXf39  ! number of 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
    integer(kind=i4) :: ndf39  ! number of dipoles in dipole array (3.9)
    integer(kind=i4) :: nRf311 ! number of values for dipole impedance 'R' (function of length) (3.11)
    integer(kind=i4) :: ndf311 ! number of dipoles in dipole array (3.11)
    integer(kind=i4) :: nXf311 ! number of values for dipole impedance 'X' (function of length) (3.11)
    integer(kind=i4) :: nZf312 ! number of values for short dipole impedance 'Z' (total value) (3.12)    
    integer(kind=i4) :: ndf313 ! number of dipoles in dipole array (3.12)                      
    integer(kind=i4) :: nrf313 ! number of values for 'wave' dipole impedance (function of length) (3.13)
    integer(kind=i4) :: ndf313 ! number of dipoles in dipole array (3.13)
    integer(kind=i4) :: nRf314 ! number of values for 'R' length-invariant dipole impedance (3.14)
    integer(kind=i4) :: ndf314 ! number of dipoles in dipole array (3.14)
    integer(kind=i4) :: nXf315 ! number of values for 'X' length-invariant dipole impedance (3.15)
    integer(kind=i4) :: ndf315 ! number of dipoles in dipole array (3.15)
    integer(kind=i4) :: nbf316 ! number of beta ratio values (part of 3.15,3.14 formulae) (3.16)
    integer(kind=i4) :: ndf316 ! number of dipoles in dipole array (3.16)
    integer(kind=i4) :: nGf321 ! number of values of scattering coefficient 'Gamma' (3.21)
    integer(kind=i4) :: ndf321 ! number of dipoles in dipole array (3.21)
    integer(kind=i4) :: nFt327 ! number of values of radiation pattern of charged dipole (3.27)
    integer(kind=i4) :: ndf327 ! number of dipoles in dipole array (3.27)
    integer(kind=i4) :: nRf328 ! number of values for 'R' active impedance of charged vibrator (3.28)
    integer(kind=i4) :: ndf328 ! number of dipoles in dipole array (3.28)
    integer(kind=i4) :: nrf331 ! number of values of ingress impedance of thin biconic dipole (3.31)
    integer(kind=i4) :: ndf331 ! number of dipoles in dipole array (3.31)
    integer(kind=i4) :: nZf333 ! number of values of total impedance of dipole (active,reactive) (3.33)
    integer(kind=i4) :: ndf333 ! number of dipoles in dipole array (3.31)
    integer(kind=i4) :: nRf334 ! number of values of active impedance component of dipole (3.34, a part of 3.33)
    integer(kind=i4) :: nXf334 ! number of values of reactive impedance component of dipole (3.34, a part of 3.33)
    integer(kind=i4) :: nZf335 ! number of values of input impedance of the biconic dipole (3.35)
    integer(kind=i4) :: ndf335 ! number of dipoles in dipole array (3.35)
    integer(kind=i4) :: npf345 ! number of phi values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
    integer(kind=i4) :: ntf345 ! number of delta values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
    integer(kind=i4) :: ndf345 ! number of dipoles in dipole array (3.45)
    integer(kind=i4) :: npf346 ! number of phi values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
    integer(kind=i4) :: ntf346 ! number of delta values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
    integer(kind=i4) :: ndf346 ! number of dipoles in dipole array (3.46)
    integer(kind=i4) :: nFf349 ! number of values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
    integer(kind=i4) :: ndf349 ! number of dipoles in dipole array (3.4,3.50)
    integer(kind=i4) :: nFf350 ! number of values of horizontal-component of radiation pattern [ideally conducting earth] (3.50)
    integer(kind=i4) :: nFf352 ! number of values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
    integer(kind=i4) :: ndf352 ! number of dipoles in dipole array (3.52)
    integer(kind=i4) :: nFf366 ! number of values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
    integer(kind=i4) :: ndf366 ! number of dipoles in dipole array (3.66)
    integer(kind=i4) :: npf397 ! number of azimuthal 'phi' coordinate values of radiation pattern for V-antenna (3.97)
    integer(kind=i4) :: ntf397 ! number of azimuthal 'theta' coordinate values of radiation pattern for V-antenna (3.97)
    integer(kind=i4) :: ndf397 ! number of dipoles in dipole array (3.97)
    integer(kind=i4) :: npf398 ! number of meridional 'phi' coordinate values of radiation pattern for V-antenna (3.98)
    integer(kind=i4) :: ntf398 ! number of meridional 'theta' coordinate values of radiation pattern for V-antenna (3.98)
    integer(kind=i4) :: ndf398 ! number of dipoles in dipole array (3.98)
    integer(kind=i4) :: nRf399 ! number of impedance values of V-antenna located in the free space (3.99)
    integer(kind=i4) :: ndf399 ! number of dipoles in dipole array (3.99)
   ! First dimension -- number of dipole radiating elements in the dipole array!!
   
   ! 'z' values of current distribution of symmetric
                                      ! dipole (3.1)
    real(kind=sp), dimension(:,:), allocatable :: Izf31r4
    !dir$ attributes align : 64 :: Izf31r4
    real(kind=dp), dimension(:,:), allocatable :: Izf31r8
    !dir$ attributes align : 64 :: Izf31r8
    
   ! 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
    real(kind=sp), dimension(:,:), allocatable :: Izf34r4
    !dir$ attributes align : 64 :: Izf34r4
    real(kind=dp), dimension(:,:), allocatable :: Izf34r8
    !dir$ attributes align : 64 :: Izf34r8    
    
   ! 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)                        
    real(kind=sp), dimension(:,:), allocatable :: Ftf35r4
    !dir$ attributes align : 64 :: Ftf35r4
    real(kind=dp), dimension(:,:), allocatable :: Ftf35r8
    !dir$ attributes align : 64 :: Ftf35r8     

   ! 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
    real(kind=sp), dimension(:,:), allocatable :: Df36r4
    !dir$ attributes align : 64 :: Df36r4
    real(kind=dp), dimension(:,:), allocatable :: Df36r8
    !dir$ attributes align : 64 :: Df36r8    
    
   ! 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8) 
    real(kind=sp), dimension(:,:), allocatable :: Rf38r4
    !dir$ attributes align : 64 :: Rf38r4
    real(kind=dp), dimension(:,:), allocatable :: Rf38r8
    !dir$ attributes align : 64 :: Rf38r8    
    
   !  'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
    real(kind=sp), dimension(:,:), allocatable :: Xf39r4
    !dir$ attributes align : 64 :: Xf39r4
    real(kind=dp), dimension(:,:), allocatable :: Xf39r8
    !dir$ attributes align : 64 :: Xf39r8 
    
  ! Values for dipole impedance 'R' (function of length) (3.11)
    real(kind=sp), dimension(:,:), allocatable :: Rf311r4
    !dir$ attributes align : 64 :: Rf311r4
    real(kind=dp), dimension(:,:), allocatable :: Rf311r8
    !dir$ attributes align : 64 :: Rf311r8 
    
  ! Values for dipole impedance 'X' (function of length) (3.11)
    real(kind=sp), dimension(:,:), allocatable :: Xf311r4
    !dir$ attributes align : 64 :: Xf311r4
    real(kind=dp), dimension(:,:), allocatable :: Xf311r8
    !dir$ attributes align : 64 :: Xf311r8 

  ! values for short dipole impedance 'Z' (total value) (3.12)  
    real(kind=sp), dimension(:,:), allocatable :: Zf312r4
    !dir$ attributes align : 64 :: Zf312r4
    real(kind=dp), dimension(:,:), allocatable :: Zf312r8
    !dir$ attributes align : 64 :: Zf312r8 
  
  ! values for 'wave' dipole impedance (function of length) (3.13)
    real(kind=sp), dimension(:,:), allocatable :: rf313r4
    !dir$ attributes align : 64 :: rf313r4
    real(kind=dp), dimension(:,:), allocatable :: rf313r8
    !dir$ attributes align : 64 :: rf313r8

  ! values for 'R' length-invariant dipole impedance (3.14)
    real(kind=sp), dimension(:,:), allocatable :: Rf314r4
    !dir$ attributes align : 64 :: Rf314r4
    real(kind=dp), dimension(:,:), allocatable :: Rf314r8
    !dir$ attributes align : 64 :: Rf314r8
    
  ! values for 'X' length-invariant dipole impedance (3.15)
    real(kind=sp), dimension(:,:), allocatable :: Xf315r4
    !dir$ attributes align : 64 :: Xf315r4
    real(kind=dp), dimension(:,:), allocatable :: Xf315r8
    !dir$ attributes align : 64 :: Xf315r8
 
  ! Beta ratio values (part of 3.15,3.14 formulae) (3.16)
    real(kind=sp), dimension(:,:), allocatable :: Bf316r4
    !dir$ attributes align : 64 :: Bf316r4
    real(kind=dp), dimension(:,:), allocatable :: Bf316r8
    !dir$ attributes align : 64 :: Bf316r8

  ! Values of scattering coefficient 'Gamma' (3.21)
    real(kind=sp), dimension(:,:), allocatable :: Gf321r4
    !dir$ attributes align : 64 :: Gf321r4
    real(kind=dp), dimension(:,:), allocatable :: Gf321r8
    !dir$ attributes align : 64 :: Gf321r8

  ! Values of radiation pattern of charged dipole (3.27)
    real(kind=sp), dimension(:,:), allocatable :: Ftf327r4
    !dir$ attributes align : 64 :: Ftf327r4
    real(kind=dp), dimension(:,:), allocatable :: Ftf327r8
    !dir$ attributes align : 64 :: Ftf327r8

  ! Values for 'R' active impedance of charged vibrator (3.28)
    real(kind=sp), dimension(:,:), allocatable :: Rf328r4
    !dir$ attributes align : 64 :: Rf328r4
    real(kind=dp), dimension(:,:), allocatable :: Rf328r8
    !dir$ attributes align : 64 :: Rf328r8
    
  ! Values of ingress impedance of thin biconic dipole (3.31) 
    real(kind=sp), dimension(:,:), allocatable :: rf331r4
    !dir$ attributes align : 64 :: rf331r4
    real(kind=dp), dimension(:,:), allocatable :: rf331r8
    !dir$ attributes align : 64 :: rf331r8
    
  ! Values of total impedance of dipole (i.e. active,reactive) (3.33)
    real(kind=sp), dimension(:,:), allocatable :: Zf333r4
    !dir$ attributes align : 64 :: Zf333r4
    real(kind=dp), dimension(:,:), allocatable :: Zf333r8
    !dir$ attributes align : 64 :: Zf333r8
    
  ! Values of active impedance component of dipole (3.34, a part of 3.33)
    real(kind=sp), dimension(:,:), allocatable :: Rf334r4
    !dir$ attributes align : 64 :: Rf334r4
    real(kind=dp), dimension(:,:), allocatable :: Rf334r8
    !dir$ attributes align : 64 :: Rf334r8
    
  ! Values of reactive impedance component of dipole (3.34, a part of 3.33)
    real(kind=sp), dimension(:,:), allocatable :: Xf334r4
    !dir$ attributes align : 64 :: Xf334r4
    real(kind=dp), dimension(:,:), allocatable :: Xf334r8
    !dir$ attributes align : 64 :: Xf334r8
    
  ! Values of input impedance of the biconic dipole (3.35)
    real(kind=sp), dimension(:,:), allocatable :: Zf335r4
    !dir$ attributes align : 64 :: Zf335r4
    real(kind=dp), dimension(:,:), allocatable :: Zf335r8
    !dir$ attributes align : 64 :: Zf335r8
    
  ! Values horizontal-component [phi,delta, radiation pattern] 
  ! of symmetric horizontal dipole (plane-parallel) (3.45)
    real(kind=sp), dimension(:,:,:), allocatable :: Ff345r4
    !dir$ attributes align : 64 :: Ff345r4
    real(kind=dp), dimension(:,:,:), allocatable :: Ff345r8
    !dir$ attributes align : 64 :: Ff345r8
    
  ! Values vertical-component [phi,delta, radiation pattern] 
  ! of symmetric horizontal dipole (plane-parallel) (3.46)
    real(kind=sp), dimension(:,:,:), allocatable :: Ff346r4
    !dir$ attributes align : 64 :: Ff346r4
    real(kind=dp), dimension(:,:,:), allocatable :: Ff346r8
    !dir$ attributes align : 64 :: Ff346r8
    
  ! Values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
    real(kind=sp), dimension(:,:), allocatable :: Ff349r4
    !dir$ attributes align : 64 :: Ff349r4
    real(kind=dp), dimension(:,:), allocatable :: Ff349r8
    !dir$ attributes align : 64 :: Zf349r8
    
  ! Values of horizontal-component of radiation pattern [ideally conducting earth] (3.50)
    real(kind=sp), dimension(:,:), allocatable :: Ff350r4
    !dir$ attributes align : 64 :: Ff350r4
    real(kind=dp), dimension(:,:), allocatable :: Ff350r8
    !dir$ attributes align : 64 :: Zf350r8
    
  ! Values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
    real(kind=sp), dimension(:,:), allocatable :: Ff352r4
    !dir$ attributes align : 64 :: Ff352r4
    real(kind=dp), dimension(:,:), allocatable :: Ff352r8
    !dir$ attributes align : 64 :: Ff352r8
    
  ! Values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
    real(kind=sp), dimension(:,:), allocatable :: Ff366r4
    !dir$ attributes align : 64 :: Ff366r4
    real(kind=dp), dimension(:,:), allocatable :: Ff366r8
    !dir$ attributes align : 64 :: Ff366r8
    
  ! Azimuthal 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.97)
    real(kind=sp), dimension(:,:,:), allocatable :: Ff397r4
    !dir$ attributes align : 64 :: Ff397r4
    real(kind=dp), dimension(:,:,:), allocatable :: Ff397r8
    !dir$ attributes align : 64 :: Ff397r8
    
   ! Meridional 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.98)
    real(kind=sp), dimension(:,:,:), allocatable :: Ff398r4
    !dir$ attributes align : 64 :: Ff398r4
    real(kind=dp), dimension(:,:,:), allocatable :: Ff398r8
    !dir$ attributes align : 64 :: Ff398r8
    
   ! Impedance values of V-antenna located in the free space (3.99)
    real(kind=sp), dimension(:,:), allocatable :: Rf399r4
    !dir$ attributes align : 64 :: Rf399r4
    real(kind=dp), dimension(:,:), allocatable :: Rf399r8
    !dir$ attributes align : 64 :: Rf399r8

end module mod_dipole_antenna
