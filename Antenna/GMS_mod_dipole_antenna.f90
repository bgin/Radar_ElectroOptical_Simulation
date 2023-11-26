
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
    integer(kind=i4) :: nIzf34 ! number of 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
    integer(kind=i4) :: nFtf35 ! number of 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)
    integer(kind=i4) :: nDf36  ! number of 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
    integer(kind=i4) :: nRf38  ! number of 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)
    integer(kind=i4) :: nXf39  ! number of 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)


   ! 'z' values of current distribution of symmetric
                                      ! dipole (3.1)
    real(kind=sp), dimension(:), allocatable :: Izf31r4
    !dir$ attributes align : 64 :: Izf31r4
    real(kind=dp), dimension(:), allocatable :: Izf31r8
    !dir$ attributes align : 64 :: Izf31r8
    
   ! 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
    real(kind=sp), dimension(:), allocatable :: Izf34r4
    !dir$ attributes align : 64 :: Izf34r4
    real(kind=dp), dimension(:), allocatable :: Izf34r8
    !dir$ attributes align : 64 :: Izf34r8    
    
   ! 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)                        
    real(kind=sp), dimension(:), allocatable :: Ftf35r4
    !dir$ attributes align : 64 :: Ftf35r4
    real(kind=dp), dimension(:), allocatable :: Ftf35r8
    !dir$ attributes align : 64 :: Ftf35r8     

   ! 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
    real(kind=sp), dimension(:), allocatable :: Df36r4
    !dir$ attributes align : 64 :: Df36r4
    real(kind=dp), dimension(:), allocatable :: Df36r8
    !dir$ attributes align : 64 :: Df36r8    
    
   ! 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8) 
    real(kind=sp), dimension(:), allocatable :: Rf38r4
    !dir$ attributes align : 64 :: Rf38r4
    real(kind=dp), dimension(:), allocatable :: Rf38r8
    !dir$ attributes align : 64 :: Rf38r8    
    
   ! number of 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
    real(kind=sp), dimension(:), allocatable :: Xf39r4
    !dir$ attributes align : 64 :: Xf39r4
    real(kind=dp), dimension(:), allocatable :: Xf39r8
    !dir$ attributes align : 64 :: Xf39r8 






















end module mod_dipole_antenna
