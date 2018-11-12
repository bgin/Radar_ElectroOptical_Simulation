
module mod_constants

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_constants'
 !          
 !          Purpose:
 !                    Collection of various program constants
 !                   
 !                     
 !          History:
 !                        Date: 08-10-2018
 !                        Time: 11:31 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use mod_kinds, only : int4,int8,sp,dp
    implicit none
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_CONSTANTS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_CONSTANTS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_CONSTANTS_MICRO = 0_int4
    
    ! Module/file version
    integer(kind=int4), parameter, public :: MOD_CONSTANTS_FULLVER = 1000_int4*MOD_CONSTANTS_MAJOR+100_int4* &
                                             MOD_CONSTANTS_MINOR+10_int4*MOD_CONSTANTS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_CONSTANTS_CREATE_DATE = "08-10-2018 11:31 +00200 (MON 08 OCT 2018 GMT+2)"
    
    ! Module build date ( should be set after every succesfful build)
    character(*),  parameter, public :: MOD_CONSTANTS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_CONSTANTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),  parameter, public :: MOD_CONSTANTS_SYNOPSIS = "Collection of various mathematical and physical constants."
    
    
    !!=========================================================!!
    !!     Definitions of various constant values              !!
    !!=========================================================!!
    
    
    ! Default loop-blocking size
    integer(kind=int4), parameter, public :: DEFAULT_LOOP_BLOCKING_SIZE = 8_int4
    
    ! One nano-second
    real(kind=dp),    parameter, public :: one_ns = 0.000000001E+00_dp
    
    ! Ohm resistenace for propagating wave
    real(kind=dp),    parameter, public :: ohm_res = 377.0E+00_dp
    
     ! Speed of light
    real(kind=dp),    parameter, public :: clight_const = 299792458.0E+00_dp
    
     ! Gravitational constant
    real(kind=dp),    parameter, public :: grav_const = 9.81_dp
    
      ! Gas constant dry air i.e 287.04 m^2*s^-2*K^-1
    real(kind=dp),    parameter, public :: rgas_const = 287.0_dp
    
     ! Surface refractivity
    real(kind=dp),    parameter, public :: sref_const = 313.0_dp
    
      ! Refractivity constant C'd  i.e. 77.6 K mbar^-1
    real(kind=dp),    parameter, public :: Cd_const = 77.6_dp
    
      ! Refractivity constant C'w1 i.e. 71.6 K mbar^-1
    real(kind=dp),    parameter, public :: Cw1_const = 71.6_dp
    
      ! Refractivity constant C'w2 i.e. 3.7x10^5 K^2 mbar^-1
    real(kind=dp),    parameter, public :: CW2_const = 37000.0_dp
    
      ! Inverse SRTQ of 2 i.e 1/SQRT(2) 
    real(kind=dp),    parameter, public :: isqrt2  =  0.7071067811865475488016887242097_dp
    
     !  Zero-valued complex constant
    complex(kind=dp), parameter, public :: czero_const = CMPLX(0.0_dp,0.0_dp)
    
      ! SQRT(2)
    real(kind=dp),    parameter, public :: sqrt2 =  1.414213562373095_dp
    
      ! Earth mean radius i.e. 6371.0 km
    real(kind=dp),    parameter, public :: emr_const =  6371.0_dp
    
       ! Value of PI
    real(kind=dp),    parameter, public :: pi_const =   3.1415926535897932384626433832795_dp
    
       ! Value of 1/PI
    real(kind=dp),    parameter, public :: v1_over_pi =  0.31830988618379067153776752674503_dp
    
       ! Value of 2PI
    real(kind=dp),    parameter, public :: pi2_const =  6.283185307179586476925286766559_dp
    
       ! Value of 4PI
    real(kind=dp),    parameter, public :: pi4_const =  12.566370614359172953850573533118_dp
    
       ! Value of 8PI
    real(kind=dp),    parameter, public :: pi8_const =   25.132741228718345907701147066236_dp
    
       ! Value o 8/PI*PI
    real(kind=dp),    parameter, public :: pi_over2  =    1.5707963267948966192313216916398_dp
    
       ! Value of 4/PI
    real(kind=dp),    parameter, public :: v4_over_pi =   1.2732395447351626861510701069801_dp
    
        ! Value of quarter PI
    real(kind=dp),    parameter, public :: pi_over4   =   0.78539816339744830961566084581988_dp
    
        ! Value of PI**2
    real(kind=dp),    parameter, public :: pi_2pow    =   9.8696044010893586188344909998762_dp
    
        ! Value of PI**3
    real(dp),    parameter, public :: pi_3pow    =   31.006276680299820175476315067101_dp
    
        ! Value of PI**4
    real(dp),    parameter, public :: pi_4pow    =   97.409091034002437236440332688705_dp
    
        ! Value of SQRT(PI)
    real(dp),    parameter, public :: sqrt_pi    =    1.7724538509055160272981674833411_dp
    
         ! Value of SQRT(2PI)
    real(dp),    parameter, public :: sqrt_2pi   =    2.506628274631000502415765284811_dp
    
          ! Value of SQRT(4PI)
    real(dp),    parameter, public :: sqrt_4pi   =     3.5449077018110320545963349666823_dp
    
          ! Value of 1/SQRT(2PI)
    real(dp),    parameter, public :: isqrt_2pi  =     0.39894228040143267793994605993439_dp
    
          ! Value of 1/SQRT(4PI)
    real(dp),    parameter, public :: isqrt_4pi  =     0.07957747154594766788444188168626_dp
    
          ! Value of Ln(2)
    real(dp),    parameter, public :: ln2_const  =     0.69314718055994530941723212145818_dp
    
          ! Value of Ln(4)
    real(dp),    parameter, public :: ln4_const  =     1.3862943611198906188344642429164_dp
    
          ! Very small constant (1.0E-15)
    real(dp),    parameter, public :: v1em15     =     0.000000000000001_dp
    
          ! Shift by 1000
    real(dp),    parameter, public :: shift1e3   =     1000.0_dp
    
          ! Very large constant
    real(dp),    parameter, public :: v1e35      =     1.0E+35_dp
    
           ! Machine epsilon (64-bit)
    real(dp),    parameter, public :: meps64     =     0.00000000000000022204460492503131_dp
    
           ! Standard tolerance (for statistical calculations) 
    real(dp),    parameter, public :: statol     =     0.0000000000001_dp
    
    real(kind=dp),    parameter, public :: v1n        = 1.0_dp
    
    real(kind=dp),    parameter, public :: v0n        = 0.0_dp
             
    real(kind=dp),    parameter, public   :: v1over4    = 0.25_dp
    
    real(kind=dp), parameter, public :: v1over3 = 0.3333333333333333333333333_dp
             
    real(kind=dp),    parameter, public :: v1over2    = 0.5_dp
             
    real(kind=dp),    parameter, public :: vposinf    = IEEE_VALUE(v1n,IEEE_POSITIVE_INF)
    
    real(kind=dp), parameter, private :: INITVAL = HUGE(1.0E+00_dp)
    
    ! Array of increment coefficients which are of value 1/2^-n
    ! where n = 1,2...32
    real(dp64),   dimension(16), parameter, public :: ipow2_coeff = [ 0.5_dp64,0.25_dp64,                            &
                                                                        0.125_dp64,0.0625_dp64,0.03125_dp64,         &
                                                                        0.015625_dp64,0.0078125_dp64,                &
                                                                        0.00390625_dp64,0.001953125_dp64,            &
                                                                        0.0009765625_dp64,0.00048828125_dp64,        &
                                                                        0.000244140625_dp64,0.0001220703125_dp64,    &
                                                                        6.1169562025935894298996819182775e-5_dp64,   &
                                                                        0.000030517578125_dp64,0.0000152587890625_dp64 ]

end module mod_constants