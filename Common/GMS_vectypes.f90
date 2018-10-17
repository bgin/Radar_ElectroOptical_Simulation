
#inlcude "Config.fpp"


module mod_vectypes

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_vectypes'
 !          
 !          Purpose:
 !                     SIMD-friendly derived types, which mimicks
 !                     __m256,__m256d,__m512,__m512d data types.
 !                     
 !
 !          History:
 !                        
 !                        Date: 10-10-2018
 !                        Time: 16:31 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                      Bernard Gingold
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
     
     use mod_kinds, only : int4, sp, dp
     implicit none
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=int4), parameter, public :: MOD_VECTYPES_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_VECTYPES_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_VECTYPES_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_VECTYPES_FULLVER = 1000_int4*MOD_VECTYPES_MAJOR+100_int4*MOD_VECTYPES_MINOR+ &
                                             10_int4*MOD_VECTYPES_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_VECTYPES_CREATE_DATE = "10-10-2018 16:31 +00200 (WED 10 OCT 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_VECTYPES_BUILD_DATE = " "
    
    ! Module author info
    character(*),       parameter, public :: MOD_VECTYPES_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_VECTYPES_SYNOPSIS = " SIMD-friendly derived types."
    
   
    !!===========================================!!
    !!  2-wide (real(kind=8)) XMM derived type   !!
    !!===========================================!!
     
     type, public :: XMM2r8_t
           SEQUENCE
           real(kind=dp), dimension(0:1) :: v
     end type XMM2r8_t
     
    !!===========================================!!
    !!  2-wide (int(kind=4)) XMM derived type    !!
    !!===========================================!!
     
     type, public :: XMM2i4_t
           SEQUENCE
           integer(kind=int4), dimension(0:1) :: v
     end type XMM2i4_t
     
     !!===========================================!!
     !! 3-wide (real(kind=8)) YMM derived type    !!
     !!===========================================!!
     
     type, public :: YMM3r8_t
           SEQUENCE
           real(kind=dp), dimension(0:2) :: v
     end type YMM3r8_t
     
     !!===========================================!!
     !! 3-wide (int(kind=4)) XMM derived type     !!
     !!===========================================!!
     
     type, public :: XMM3i4_t
           SEQUENCE
           integer(kind=int4), dimension(0:2) :: v
     end type YMM3i4_t
     
     !!===========================================!!
     !! 4-wide (real(kind=8)) YMM derived type    !!
     !!===========================================!!
     
     type, public :: YMM4r8_t
           SEQUENCE
           real(kind=dp), dimension(0:3) :: v
     end type YMM4r8_t
     
     !!===========================================!!
     !! 4-wide (int(kind=4)) XMM derived type    !!
     !!===========================================!!
     
     type, public :: XMM4i4_t
           SEQUENCE
           integer(kind=int4), dimension(0:3) :: v
     end type XMM4i4_t
     
     !!===========================================!!
     !! 8-wide (int(kind=4)) YMM derived type    !!
     !!===========================================!!
     
     type, public :: YMM8i4_t
           SEQUENCE
           integer(kind=int4), dimension(0:7) :: v
     end type YMM8i4_t
     
     !!===========================================!!
     !! 16-wide (int(kind=4)) ZMM derived type    !!
     !!===========================================!!
     
     type, public :: ZMM16i4_t
           SEQUENCE
           integer(kind=int4), dimension(0:15) :: v
     end type ZMM16i4_t
     
     !!===========================================!!
     !! 8-wide (real(kind=8)) ZMM derived type    !!
     !!===========================================!!
     
     

     
     
     type, public :: ZMM8r8_t
           SEQUENCE
           real(kind=dp), dimension(0:7) :: v
     end type ZMM8r8_t
     
    !!===========================================!!
    !!  2-wide (real(kind=4)) XMM derived type   !!
    !!===========================================!!
     
     type, public :: XMM2r4_t
           SEQUENCE
           real(kind=sp), dimension(0:1) :: v
     end type XMM2r4_t
     
    !!===========================================!!
    !!  3-wide (real(kind=4)) XMM derived type   !!
    !!===========================================!!
     
     type, public :: XMM3r4_t
           SEQUENCE
           real(kind=sp), dimension(0:2) :: v
     end type XMM3r4_t
     
    !!===========================================!!
    !!  4-wide (real(kind=4)) XMM derived type   !!
    !!===========================================!!
     
     type, public :: XMM4r4_t
           SEQUENCE
           real(kind=sp), dimension(0:3) :: v
     end type XMM4r4_t
     
    !!===========================================!!
    !!  8-wide (real(kind=4)) YMM derived type   !!
    !!===========================================!!
     
     type, public :: YMM8r4_t
           SEQUENCE
           real(kind=sp), dimension(0:7) :: v
     end type YMM8r4_t
     
    !!===========================================!!
    !!  16-wide (real(kind=4)) ZMM derived type  !!
    !!===========================================!!
     
     type, public :: ZMM16r4_t
           SEQUENCE
           real(kind=sp), dimension(0:15) :: v
     end type ZMM16r4_t
     
     !!===============================================!!
     !! 4-wide real(kind=4) XMM derived type          !!
     !! 2 complex number representation by parts.     !!
     !!===============================================!!
     
     type, public :: XMM2c4_t
           SEQUENCE
           real(kind=sp), dimension(0:1) :: re
           real(kind=sp), dimension(0:1) :: im
     end type XMM2c4_t
     
     !!===============================================!!
     !! 4-wide real(kind=8) YMM derived type          !!
     !! 4 complex number representation by parts.     !!
     !!===============================================!!
     
     type, public :: YMM4c8_t
           SEQUENCE
           real(kind=dp), dimension(0:3) :: re
           real(kind=dp), dimension(0:3) :: im
     end type YMM4c8_t
     !!===============================================!!
     !! 8-wide real(kind=8) ZMM derived type          !!
     !! 8 complex number representation by parts.     !!
     !!===============================================!!
     
     type, public :: ZMM8c8_t
           SEQUENCE
           real(kind=dp), dimension(0:7) :: re
           real(kind=dp), dimension(0:7) :: im
     end type ZMM8c8_t
     
     !!===============================================!!
     !! 16-wide real(kind=4) ZMM derived type         !!
     !! 16 complex number representation by parts.    !!
     !!===============================================!!
     
     type, public ZMM16c4_t
           SEQUENCE
           real(kind=sp), dimension(0:15) :: re
           real(kind=sp), dimension(0:15) :: im
     end type ZMM16c4_t
     
     !!===============================================!!
     !! 2-wide logical(kind=int4)) Mask derived type  !!
     !!===============================================!!
     
     type, public :: Mask2_t
           SEQUENCE
           logical(kind=int4), dimension(0:1) :: m
     end type Mask2_t
     
     !!===============================================!!
     !! 2-wide logical(kind=int4)) Mask derived type  !!
     !!===============================================!!
     
     type, public :: Mask3_t
           SEQUENCE
           logical(kind=int4), dimension(0:2) :: m
     end type Mask3_t
         
     !!===============================================!!
     !! 4-wide logical(kind=int4)) Mask derived type  !!
     !!===============================================!!
     
     type, public :: Mask4_t
           SEQUENCE
           logical(kind=int4), dimension(0:3) :: m
     end type Mask4_t
     
     !!===============================================!!
     !! 8-wide logical(kind=int4)) Mask derived type  !!
     !!===============================================!!
     
     type, public :: Mask8_t
           SEQUENCE
           logical(kind=int4), dimension(0:7) :: m
     end type Mask8_t
     
     !!===============================================!!
     !! 16-wide logical(kind=int4)) Mask derived type  !!
     !!===============================================!!
     
     type, public :: Mask16_t
           SEQUENCE
           logical(kind=int4),  dimension(0:15) :: m
     end type Mask16_t
     
end module mod_vectypes