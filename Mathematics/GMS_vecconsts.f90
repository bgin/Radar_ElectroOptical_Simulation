
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
 !                      Minor: 1
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
    use mod_kinds,     only :  i4, sp, dp
    use mod_vectypes   
    use mod_constants, only :  v1_over_pi, pi_const,pir4_const,twopir4_const
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4), parameter, public :: MOD_VECCONSTS_MAJOR = 1_i4
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_VECCONSTS_MINOR = 0_i4
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_VECCONSTS_MICRO = 0_i4
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_VECCONSTS_FULLVER = 1000_i4*MOD_VECCONSTS_MAJOR+100_i4*MOD_VECCONSTS_MINOR+ &
                                                                     10_i4*MOD_VECCONSTS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VECCONSTS_CREATE_DATE = "14-10-2018 10:57 PM GMT+2 (SUN 14 OCT 2018 10:57 -00200)"
    
    ! Module build creation
    character(*),       parameter, public :: MOD_VECCONSTS_BUILD_DATE = __DATE__ 

    character(*),       parameter, public :: MOD_VECCONSTS_BUILD_TIME = __TIME__ 
    
    ! Module author info
    character(*),       parameter, public :: MOD_VECCONSTS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),       parameter, public :: MOD_VECCONSTS_SYNOPSIS = " SIMD-like derived types constants."
    
    
    type(XMM2r8_t), parameter, public  :: v2r8_invpi      = XMM2r8_t(0.31830988618379067153777_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_pi         = XMM2r8_t(3.14159265358979323846264_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_0          = XMM2r8_t(0.0_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_airyc1     = XMM2r8_t(0.355028053887817_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_airyc2     = XMM2r8_t( 0.258819403792807_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_airysr3    = XMM2r8_t( 1.732050807568877_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1em100     = XMM2r8_t(1.0E-100_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_eps1e15    = XMM2r8_t(1.0E-15_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1over1_5   = XMM2r8_t(0.66666666666666666666666666666667E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1over2     = XMM2r8_t(0.5_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_n1over2    = XMM2r8_t(-0.5_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1over3     = XMM2r8_t(0.333333333333333333333333333E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1over4     = XMM2r8_t(0.25E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_n1over4    = XMM2r8_t(-0.25E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1over8     = XMM2r8_t(0.125E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_2         = XMM2r8_t(2.0E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_1         = XMM2r8_t(1.0E+00_dp)
    
    type(XMM2r8_t), parameter, public  :: v2r8_n1         = XMM2r8_t(-1.0E+00_dp)
    
    ! //////////////////////////////////////////////////////////////////////////////// ! 
    
    type(XMM4r4_t), parameter, public  :: v4r4_invpi      = XMM4r4_t(0.31830988618379067153777_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_pi        = XMM4r4_t(3.14159265358979323846264_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_0          = XMM4r4_t(0.0_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_airyc1     = XMM4r4_t(0.355028053887817_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_airyc2     = XMM4r4_t( 0.258819403792807_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_airysr3    = XMM4r4_t( 1.732050807568877_sp)
    
        
    type(XMM4r4_t), parameter, public  :: v4r4_eps1e15    = XMM4r4_t(1.0E-15_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1over1_5   = XMM4r4_t(0.66666666666666666666666666666667E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1over2     = XMM4r4_t(0.5_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_n1over2    = XMM4r4_t(-0.5_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1over3     = XMM4r4_t(0.333333333333333333333333333E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1over4     = XMM4r4_t(0.25E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_n1over4    = XMM4r4_t(-0.25E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1over8     = XMM4r4_t(0.125E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_24         = XMM4r4_t(24.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_20         = XMM4r4_t(20.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_19         = XMM4r4_t(19.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_12         = XMM4r4_t(12.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_10         = XMM4r4_t(10.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_8          = XMM4r4_t(8.0E+00_sp)

     type(XMM4r4_t), parameter, public  :: v4r4_6         = XMM4r4_t(6.0E+00_sp)

     type(XMM4r4_t), parameter, public  :: v4r4_5         = XMM4r4_t(5.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_4         = XMM4r4_t(4.0E+00_sp)

     type(XMM4r4_t), parameter, public  :: v4r4_3         = XMM4r4_t(3.0E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_2         = XMM4r4_t(2.0E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_1         = XMM4r4_t(1.0E+00_sp)
    
    type(XMM4r4_t), parameter, public  :: v4r4_n1         = XMM4r4_t(-1.0E+00_sp)

    type(XMM4r4_t), parameter, public  :: v4r4_n2         = XMM4r4_t(-2.0E+00_sp)
    
    !/////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    type(YMM4r8_t), parameter, public  :: v4r8_invpi      = YMM4r8_t(v1_over_pi)
    
    type(YMM4r8_t), parameter, public  :: v4r8_pi         = YMM4r8_t(pi_const)
    
    type(YMM4r8_t), parameter, public  :: v4r8_0         = YMM4r8_t(0.0_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_airyc1     = YMM4r8_t(0.355028053887817_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_airyc2     = YMM4r8_t( 0.258819403792807_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_airysr3    = YMM4r8_t( 1.732050807568877_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1em100     = YMM4r8_t(1.0E-100_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_eps1e15    = YMM4r8_t(1.0E-15_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1over1_5   = YMM4r8_t(0.66666666666666666666666666666667E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1over2     = YMM4r8_t(0.5_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_n1over2    = YMM4r8_t(-0.5_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1over3     = YMM4r8_t(0.333333333333333333333333333E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1over4     = YMM4r8_t(0.25E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_n1over4    = YMM4r8_t(-0.25E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1over8     = YMM4r8_t(0.125E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_2         = YMM4r8_t(2.0E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_1         = YMM4r8_t(1.0E+00_dp)
    
    
    
    type(YMM4r8_t), parameter, public  :: v4r8_hugep      = YMM4r8_t(1.0E+300_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_hugen      = YMM4r8_t(-1.0E+300_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_sqrtpi     = YMM4r8_t( 1.7724538509055160272981674833411E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_sqrt2pi    = YMM4r8_t( 2.506628274631000502415765284811E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_sqrt4pi    = YMM4r8_t( 3.5449077018110320545963349666823E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_isqrt2pi   = YMM4r8_t( 0.39894228040143267793994605993439E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_isqrt4pi   = YMM4r8_t( 0.07957747154594766788444188168626E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_ln2        = YMM4r8_t( 0.69314718055994530941723212145818E+00_dp)
    
    type(YMM4r8_t), parameter, public  :: v4r8_ln4        = YMM4r8_t( 1.3862943611198906188344642429164E+00_dp)

    type(YMM8r4_t), parameter, public  :: v8r4_0       = YMM8r4_t(0.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_quart    = YMM8r4_t(0.25_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_half     = YMM8r4_t(0.5_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_1       = YMM8r4_t(1.0_sp)
    
    type(YMM8r4_t), parameter, public  :: v8r4_n1       = YMM8r4_t(-1.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n2       = YMM8r4_t(2.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n3       = YMM8r4_t(3.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n4       = YMM8r4_t(4.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n5       = YMM8r4_t(5.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_neg4     = YMM8r4_t(-4.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n9       = YMM8r4_t(9.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_n16      = YMM8r4_t(16.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_1over3   = YMM8r4_t(0.3333333333333333333333_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_pi       = YMM8r4_t(3.14159265358979323846264_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_2pi      = YMM8r4_t(twopir4_const)

    type(YMM8r4_t), parameter, public  :: v8r4_neg1     = YMM8r4_t(-1.0_sp)

    type(YMM8r4_t), parameter, public  :: v8r4_tiny     = YMM8r4_t(TINY(1.0_sp))

    type(YMM8r4_t), parameter, public  :: v8r4_huge     = YMM8r4_t(HUGE(1.0_sp))

    
    
    

    
    
    type(ZMM8r8_t), parameter, public  :: v8_invpi      = ZMM8r8_t(v1_over_pi)
    
    type(ZMM8r8_t), parameter, public  :: v8_pi         = ZMM8r8_t(3.1415926535897932384626_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_neg1       = ZMM8r8_t(-1.0_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_0          = ZMM8r8_t(0.0E+00_dp)
    
      
    type(ZMM8r8_t), parameter, public  :: v8_hugep      = ZMM8r8_t(1.0E+300_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_hugen      = ZMM8r8_t(-1.0E+300_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_eps1e15    = ZMM8r8_t(1.0E-15_dp)
    
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
    
    type(ZMM8r8_t), parameter, public  :: v8_2            = ZMM8r8_t(2.0E+00_dp)

    type(ZMM8r8_t), parameter, public  :: v8_n2           = ZMM8r8_t(-2.0E+00_dp)

    type(ZMM8r8_t), parameter, public  :: v8_1            = ZMM8r8_t(1.0_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_1over8     = ZMM8r8_t(0.125E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrtpi     = ZMM8r8_t(1.7724538509055160272981674833411E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrt2pi    = ZMM8r8_t(2.506628274631000502415765284811E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8_sqrt4pi    = ZMM8r8_t( 3.5449077018110320545963349666823E+00_dp)

    type(ZMM8r8_t), parameter, public  :: v8_2pi          = ZMM8r8_t(6.2831853071795864769253_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8r8_isqrt2pi   = ZMM8r8_t( 0.39894228040143267793994605993439E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8r8_isqrt4pi   = ZMM8r8_t( 0.07957747154594766788444188168626E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8r8_ln2        = ZMM8r8_t( 0.69314718055994530941723212145818E+00_dp)
    
    type(ZMM8r8_t), parameter, public  :: v8r8_ln4        = ZMM8r8_t( 1.3862943611198906188344642429164E+00_dp)

   
    
   

    type(ZMM16r4_t), parameter, public  :: v16_0        = ZMM16r4_t(0.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_quart     = ZMM16r4_t(0.25_sp)

    type(ZMM16r4_t), parameter, public  :: v16_half      = ZMM16r4_t(0.5_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n3        = ZMM16r4_t(3.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n4        = ZMM16r4_t(4.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_2           = ZMM16r4_t(2.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n2          = ZMM16r4_t(-2.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_1           = ZMM16r4_t(1.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n5        = ZMM16r4_t(5.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_neg4      = ZMM16r4_t(-4.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_neg1      = ZMM16r4_t(-1.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_1over3    = ZMM16r4_t(0.333333333333333333333_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n9        = ZMM16r4_t(9.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_n16       = ZMM16r4_t(16.0_sp)

    type(ZMM16r4_t), parameter, public  :: v16_2pi         = ZMM16r4_t(6.2831853071795864769253_sp)

    type(ZMM16r4_t), parameter, public  :: v16_pi          = ZMM16r4_t(3.1415926535897932384626_sp)

    type(ZMM16r4_t), parameter, public  :: v16_sqrtpi     = ZMM16r4_t(1.7724538509055160272981674833411E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_sqrt2pi    = ZMM16r4_t(2.506628274631000502415765284811E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_sqrt4pi    = ZMM16r4_t( 3.5449077018110320545963349666823E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_isqrt2pi   = ZMM16r4_t( 0.39894228040143267793994605993439E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_isqrt4pi   = ZMM16r4_t( 0.07957747154594766788444188168626E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_ln2        = ZMM16r4_t( 0.69314718055994530941723212145818E+00_sp)
    
    type(ZMM16r4_t), parameter, public  :: v16_ln4        = ZMM16r4_t( 1.3862943611198906188344642429164E+00_sp)

    type(ZMM16r4_t), parameter, public  :: v16_tiny       = ZMM16r4_t(TINY(1.0_sp))

    type(ZMM16r4_t), parameter, public  :: v16_huge       = ZMM16r4_t(HUGE(1.0_sp))

    
    
end module mod_vecconsts
