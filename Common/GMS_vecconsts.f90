
module mod_vecconsts

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_vecconsts'
 !          
 !          Purpose:
 !                     Vector constants based on simd-friendly derived types.
 !                     
 !          History:
 !                        Date: 14-10-2017
 !                        Time: 10:57 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                    Bernard Gingold
 !                 
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
    use mod_kinds,     only :  int4,dp
    use mod_vectypes,  only :  YMM4r8_t, ZMM8r8_t
    use mod_constants, only :  v1_over_pi, pi_const
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_VECCONSTS_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_VECCONSTS_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_VECCONSTS_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_VECCONSTS_FULLVER = 1000_int4*MOD_VECCONSTS_MAJOR+100_int4*MOD_VECCONSTS_MINOR+ &
                                                                     10_int4*MOD_VECCONSTS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VECCONSTS_CREATE_DATE = "14-10-2018 10:57 PM GMT+2 (SUN 14 OCT 2018 10:57 -00200)"
    
    ! Module build creation
    character(*),       parameter, public :: MOD_VECCONSTS_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_VECCONSTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_VECCONSTS_SYNOPSIS = " SIMD-like derived types constants."
    
    
    type(YMM4r8_t), parameter, public  :: v4_invpi      = YMM4r8_t(v1_over_pi)
    
    type(YMM4r8_t), parameter, public  :: v4_pi         = YMM4r8_t(pi_const)
    
    type(YMM4r8_t), parameter, public  :: v4_n0         = YMM4r8_t(0.0_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_airyc1     = YMM4r8_t(0.355028053887817_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_airyc2     = YMM4r8_t( 0.258819403792807_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_airysr3    = YMM4r8_t( 1.732050807568877_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1em100     = YMM4r8_t(1.0E-100_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1over1_5   = YMM4r8_t(0.66666666666666666666666666666667E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1over2     = YMM4r8_t(0.5_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_n1over2    = YMM4r8_t(-0.5_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1over3     = YMM4r8_t(0.333333333333333333333333333E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1over4     = YMM4r8_t(0.25E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_n1over4    = YMM4r8_t(-0.25E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_1over8     = YMM4r8_t(0.125E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_n2         = YMM4r8_t(2.0E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_n1         = YMM4r8_t(1.0E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_hugep      = YMM4r8_t(1.0E+300_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_hugen      = YMM4r8_t(-1.0E+300_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_sqrtpi     = YMM4r8_t( 1.7724538509055160272981674833411E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_sqrt2pi    = YMM4r8_t( 2.506628274631000502415765284811E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_sqrt4pi    = YMM4r8_t( 3.5449077018110320545963349666823E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_isqrt2pi   = YMM4r8_t( 0.39894228040143267793994605993439E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_isqrt4pi   = YMM4r8_t( 0.07957747154594766788444188168626E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_ln2        = YMM4r8_t( 0.69314718055994530941723212145818E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_ln4        = YMM4r8_t( 1.3862943611198906188344642429164E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4_pinf       = YMM4r8_t( IEEE_VALUE(1.0E+00_dp,IEEE_POSITIVE_INF))
    
    type(ZMM8r8_t), parameter, public  :: v8_invpi      = ZMM8r8_t(v1_over_pi)
    
    type(ZMM8r8_t), parameter, public  :: v8_pi         = ZMM8r8_t(pi_const)
    
    type(ZMM8r8_t), parameter, public  :: v8_n0         = ZMM8r8_t(0.0E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_n1         = ZMM8r8_t(1.0E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_hugep      = ZMM8r8_t(1.0E+300_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_hugen      = ZMM8r8_t(-1.0E+300_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_airyc1     = ZMM8r8_t( 0.355028053887817_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_airyc2     = ZMM8r8_t( 0.258819403792807_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_airysr3    = ZMM8r8_t( 1.732050807568877_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1em100     = ZMM8r8_t(1.0E-100_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over1_5   = ZMM8r8_t(0.66666666666666666666666666666667_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over2     = ZMM8r8_t(0.5_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_n1over2    =  ZMM8r8_t(-0.5_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over4     =  ZMM8r8_t(0.25E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_n1over4    =  ZMM8r8_t(-0.25E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over3     = ZMM8r8_t(0.33333333333333333333333333333333E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_n2         = ZMM8r8_t(2.0E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over8     = ZMM8r8_t(0.125E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrtpi     = ZMM8r8_t(1.7724538509055160272981674833411E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrt2pi    = ZMM8r8_t(2.506628274631000502415765284811E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrt4pi    = ZMM8r8_t( 3.5449077018110320545963349666823E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_isqrt2pi   = ZMM8r8_t( 0.39894228040143267793994605993439E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_isqrt4pi   = ZMM8r8_t( 0.07957747154594766788444188168626E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_ln2        = ZMM8r8_t( 0.69314718055994530941723212145818E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_ln4        = ZMM8r8_t( 1.3862943611198906188344642429164E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_pinf       = ZMM8r8_t( IEEE_VALUE(1.0E+00_dp,IEEE_POSITIVE_INF))
    
    

end module mod_vecconsts