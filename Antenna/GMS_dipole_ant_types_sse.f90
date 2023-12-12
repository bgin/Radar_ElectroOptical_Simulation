
#include "GMS_config.fpp"

module dipole_ant_types_sse


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         dipole_ant_types_sse
 !          
 !          Purpose:
 !                        Abstract data types (derived) SIMD SSE sized for dipole antenna implementation.
 !                        Various characteristics of different dipole antenna formulae and computational characteristics.
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 12-12-2023
 !                        Time: 09:48 GMT+2
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
   
    use mod_kinds,    only : i4
    use mod_vectypes, only : XMM2r8_t,XMM4r4_t
    public
    implicit none

    ! Major version
    integer(kind=i4),  parameter :: DIPOLE_ANT_TYPES_SSE_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: DIPOLE_ANT_TYPES_SSE_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: DIPOLE_ANT_TYPES_SSE_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: DIPOLE_ANT_TYPES_SSE_FULLVER =   &
            1000*DIPOLE_ANT_TYPES_SSE_MAJOR+100*DIPOLE_ANT_TYPES_SSE_MINOR+10*DIPOLE_ANT_TYPES_SSE_MICRO
    ! Module creation date
    character(*),        parameter :: DIPOLE_ANT_TYPES_CREATE_DATE = "12-12-2023 09:47 +00200 (TUE 12 DEC 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: DIPOLE_ANT_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: DIPOLE_ANT_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: DIPOLE_ANT_TYPES_SYNOPSIS    = "Data describing various dipole antenna characteristics -- type based, SSE(128-bit)-sized."
    
! default floating-point precision is 32-bit.
#if !defined(DIPOLE_ANT_TYPES_USE_PRECISION_REAL8)
#define DIPOLE_ANT_TYPES_USE_PRECISION_REAL8 0
#endif

   ! First dimension -- number of dipole radiating elements in the dipole array!!
   
   ! 'z' values of current distribution of symmetric
                                      ! dipole (3.1)
    type, public :: Izf31RV2x4x_t
          integer(kind=i4) :: nIzf31 ! number of 'z' values of current distribution of symmetric
                               ! dipole (3.1)
          integer(kind=i4) :: ndf31  ! number of dipoles in dipole array (3.1)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz  
#endif   
    end type Izf31RV2x4x_t
    
     ! 'z' values of current distribution of symmetric
                                      ! dipole (3.1)
     ! Flat-arrays
    type, public :: Izf31FRV2x4x_t
          integer(kind=i4) :: nIzf31 ! number of 'z' values of current distribution of symmetric
                               ! dipole (3.1)
          integer(kind=i4) :: ndf31  ! number of dipoles in dipole array (3.1)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz
#else
          type(XMM4r4_t), dimension(:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz  
#endif   
    end type Izf31FRV2x4x_t
    
    
    
 ! 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)   
    type, public :: Izf34RV2x4x_t
          integer(kind=i4) :: nIzf34 ! number of 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
          integer(kind=i4) :: ndf34  ! number of dipoles in dipole array (3.4)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz 
#endif
    end type Izf34RV2x4x_t
    
    ! 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)   
    ! Flat-array layout
    type, public :: Izf34FRV2x4x_t
          integer(kind=i4) :: nIzf34 ! number of 'z' values of current distribution of symmetric
                               ! very thin dipole (3.4)
          integer(kind=i4) :: ndf34  ! number of dipoles in dipole array (3.4)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz
#else
          type(XMM4r4_t), dimension(:), allocatable :: Iz
          !dir$ attributes align : 64 :: Iz 
#endif
    end type Izf34FRV2x4x_t
    
    
    
  ! 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)     
    type, public :: Ftf35RV2x4x_t
          integer(kind=i4) :: nFtf35 ! number of 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)
          integer(kind=i4) :: ndf35  ! number of dipoles in dipole array (3.5)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: Ft
         !dir$ attributes align : 64 :: Ft
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: Ft
          !dir$ attributes align : 64 :: Ft
#endif  
    end type Ftf35RV2x4x_t
    
    ! 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5) 
    ! Flat-array layout    
    type, public :: Ftf35FRV2x4x_t
          integer(kind=i4) :: nFtf35 ! number of 'theta' values of sinusoidal current distribution
                               ! for dipole radiation pattern (3.5)
          integer(kind=i4) :: ndf35  ! number of dipoles in dipole array (3.5)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: Ft
         !dir$ attributes align : 64 :: Ft
#else
          type(XMM4r4_t), dimension(:), allocatable :: Ft
          !dir$ attributes align : 64 :: Ft
#endif  
    end type Ftf35FRV2x4x_t
    
    
    
   ! 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
    type, public :: Df36RV2x4x_t
          integer(kind=i4) :: nDf36  ! number of 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
          integer(kind=i4) :: ndf36  ! number of dipoles in dipole array (3.6)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: D
         !dir$ attributes align : 64 :: D
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: D
          !dir$ attributes align : 64 :: D
#endif            
    end type Df36RV2x4x_t
    
    
      ! 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
      ! Flat-array layout
    type, public :: Df36FRV2x4x_t
          integer(kind=i4) :: nDf36  ! number of 'D' values of directed coefficients (KND, rus.)
                               ! for symmetric dipole (3.6)
          integer(kind=i4) :: ndf36  ! number of dipoles in dipole array (3.6)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: D
         !dir$ attributes align : 64 :: D
#else
          type(XMM4r4_t), dimension(:), allocatable :: D
          !dir$ attributes align : 64 :: D
#endif            
    end type Df36FRV2x4x_t
    
    
   ! 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)  
    type, public :: Rf38RV2x4x_t
          integer(kind=i4) :: nRf38  ! number of 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)
          integer(kind=i4) :: ndf38  ! number of dipoles in dipole array (3.8) 
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif              
    end type Rf38RV2x4x_t
    
    
    ! 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)  
    ! Flay-array layout
    type, public :: Rf38FRV2x4x_t
          integer(kind=i4) :: nRf38  ! number of 'R' values of symmetric dipole impedance (function of length)
                               ! (3.8)
          integer(kind=i4) :: ndf38  ! number of dipoles in dipole array (3.8) 
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif              
    end type Rf38FRV2x4x_t
    
    
    ! 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
    type, public :: Xf39RV2x4x_t
          integer(kind=i4) :: nXf39  ! number of 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
          integer(kind=i4) :: ndf39  ! number of dipoles in dipole array (3.9)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif            
    end type Xf39RV2x4x_t
    
    
     ! 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
     ! Flat-array layout
    type, public :: Xf39FRV2x4x_t
          integer(kind=i4) :: nXf39  ! number of 'X' values of symmetric dipole reactive 
                               ! impedance (function of length) (3.9)
          integer(kind=i4) :: ndf39  ! number of dipoles in dipole array (3.9)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif            
    end type Xf39FRV2x4x_t
    
    
    
    ! Values for dipole impedance 'R' (function of length) (3.11)
    type, public :: Rf311RV2x4x_t
          integer(kind=i4) :: nRf311 ! number of values for dipole impedance 'R' (function of length) (3.11)
          integer(kind=i4) :: ndf311 ! number of dipoles in dipole array (3.11)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif            
    end type Rf311RV2x4x_t
    
    
    ! Values for dipole impedance 'R' (function of length) (3.11)
    ! Flat-array layout
    type, public :: Rf311FRV2x4x_t
          integer(kind=i4) :: nRf311 ! number of values for dipole impedance 'R' (function of length) (3.11)
          integer(kind=i4) :: ndf311 ! number of dipoles in dipole array (3.11)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif            
    end type Rf311FRV2x4x_t
    
    
    
    ! Values for dipole impedance 'X' (function of length) (3.11)
    type, public :: Xf311RV2x4x_t
          integer(kind=i4) :: nXf311 ! number of values for dipole impedance 'R' (function of length) (3.11)
          integer(kind=i4) :: ndf311 ! number of dipoles in dipole array (3.11)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif             
    end type Xf311RV2x4x_t  
    
    
     ! Values for dipole impedance 'X' (function of length) (3.11)
    type, public :: Xf311FRV2x4x_t
          integer(kind=i4) :: nXf311 ! number of values for dipole impedance 'R' (function of length) (3.11)
          integer(kind=i4) :: ndf311 ! number of dipoles in dipole array (3.11)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif             
    end type Xf311FRV2x4x_t  
    
    
    
    ! Values for short dipole impedance 'Z' (total value) (3.12)    
    type, public :: Zf312RV2x4x_t
          integer(kind=i4) :: nZf312 ! number of values for short dipole impedance 'Z' (total value) (3.12)    
          integer(kind=i4) :: ndf312 ! number of dipoles in dipole array (3.12)  
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: Z
         !dir$ attributes align : 64 :: Z
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: Z
          !dir$ attributes align : 64 :: Z
#endif              
    end type Zf312RV2x4x_t  
    
    
     ! Values for short dipole impedance 'Z' (total value) (3.12)
     ! Flat-array layout    
    type, public :: Zf312FRV2x4x_t
          integer(kind=i4) :: nZf312 ! number of values for short dipole impedance 'Z' (total value) (3.12)    
          integer(kind=i4) :: ndf312 ! number of dipoles in dipole array (3.12)  
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: Z
         !dir$ attributes align : 64 :: Z
#else
          type(XMM4r4_t), dimension(:), allocatable :: Z
          !dir$ attributes align : 64 :: Z
#endif              
    end type Zf312FRV2x4x_t  
    
    
    
    ! Values for 'wave' dipole impedance (function of length) (3.13)             
    type, public :: rf313RV2x4x_t
          integer(kind=i4) :: nrf313 ! number of values for 'wave' dipole impedance (function of length) (3.13)
          integer(kind=i4) :: ndf313 ! number of dipoles in dipole array (3.13)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: r
         !dir$ attributes align : 64 :: r
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: r
          !dir$ attributes align : 64 :: r
#endif               
    end type rf313RV2x4x_t
    
    
     ! Values for 'wave' dipole impedance (function of length) (3.13)
     ! Flat-array layout             
    type, public :: rf313FRV2x4x_t
          integer(kind=i4) :: nrf313 ! number of values for 'wave' dipole impedance (function of length) (3.13)
          integer(kind=i4) :: ndf313 ! number of dipoles in dipole array (3.13)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: r
         !dir$ attributes align : 64 :: r
#else
          type(XMM4r4_t), dimension(:), allocatable :: r
          !dir$ attributes align : 64 :: r
#endif               
    end type rf313FRV2x4x_t
    
    
    
    
    ! Values for 'R' length-invariant dipole impedance (3.14)
    type, public :: Rf314RV2x4x_t
          integer(kind=i4) :: nRf314 ! number of values for 'R' length-invariant dipole impedance (3.14)
          integer(kind=i4) :: ndf314 ! number of dipoles in dipole array (3.14)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif              
    end type Rf314RV2x4x_t
    
    
     ! Values for 'R' length-invariant dipole impedance (3.14)
     ! Flat-array layout
    type, public :: Rf314FRV2x4x_t
          integer(kind=i4) :: nRf314 ! number of values for 'R' length-invariant dipole impedance (3.14)
          integer(kind=i4) :: ndf314 ! number of dipoles in dipole array (3.14)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: R
         !dir$ attributes align : 64 :: R
#else
          type(XMM4r4_t), dimension(:), allocatable :: R
          !dir$ attributes align : 64 :: R
#endif              
    end type Rf314FRV2x4x_t
    
    
    
    
   ! Values for 'X' length-invariant dipole impedance (3.15)
    type, public :: Xf315RV2x4x_t
          integer(kind=i4) :: nXf315 ! number of values for 'X' length-invariant dipole impedance (3.15)
          integer(kind=i4) :: ndf315 ! number of dipoles in dipole array (3.15)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:,:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:,:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif            
    end type Xf315RV2x4x_t
    
    
     ! Values for 'X' length-invariant dipole impedance (3.15)
     ! Flat-array layout
    type, public :: Xf315RV2x4x_t
          integer(kind=i4) :: nXf315 ! number of values for 'X' length-invariant dipole impedance (3.15)
          integer(kind=i4) :: ndf315 ! number of dipoles in dipole array (3.15)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
          type(XMM2r8_t), dimension(:), allocatable :: X
         !dir$ attributes align : 64 :: X
#else
          type(XMM4r4_t), dimension(:), allocatable :: X
          !dir$ attributes align : 64 :: X
#endif            
    end type Xf315FRV2x4x_t
    
    
   ! Beta ratio values (part of 3.15,3.14 formulae) (3.16)
     type, public :: Bf316RV2x4x_t
           integer(kind=i4) :: nbf316 ! number of beta ratio values (part of 3.15,3.14 formulae) (3.16)
           integer(kind=i4) :: ndf316 ! number of dipoles in dipole array (3.16)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
           type(XMM2r8_t), dimension(:,:), allocatable :: B
          !dir$ attributes align : 64 :: B
#else
           type(XMM4r4_t), dimension(:,:), allocatable :: B
           !dir$ attributes align : 64 :: B
#endif             
     end type Bf316RV2x4x_t
     
     
     ! Beta ratio values (part of 3.15,3.14 formulae) (3.16)
     ! Flat array-layout
     type, public :: Bf316RV2x4x_t
           integer(kind=i4) :: nbf316 ! number of beta ratio values (part of 3.15,3.14 formulae) (3.16)
           integer(kind=i4) :: ndf316 ! number of dipoles in dipole array (3.16)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
           type(XMM2r8_t), dimension(:), allocatable :: B
          !dir$ attributes align : 64 :: B
#else
           type(XMM4r4_t), dimension(:), allocatable :: B
           !dir$ attributes align : 64 :: B
#endif             
     end type Bf316RV2x4x_t
     
     
    ! Values of scattering coefficient 'Gamma' (3.21)                 
      type, public :: Gf321RV2x4x_t
            integer(kind=i4) :: nGf321 ! number of values of scattering coefficient 'Gamma' (3.21)
            integer(kind=i4) :: ndf321 ! number of dipoles in dipole array (3.21)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: G
           !dir$ attributes align : 64 :: G
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: G
            !dir$ attributes align : 64 :: G
#endif              
      end type Gf321RV2x4x_t
      
      
      ! Values of scattering coefficient 'Gamma' (3.21)  
      ! Flat-array layout               
      type, public :: Gf321FRV2x4x_t
            integer(kind=i4) :: nGf321 ! number of values of scattering coefficient 'Gamma' (3.21)
            integer(kind=i4) :: ndf321 ! number of dipoles in dipole array (3.21)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: G
           !dir$ attributes align : 64 :: G
#else
            type(XMM4r4_t), dimension(:), allocatable :: G
            !dir$ attributes align : 64 :: G
#endif              
      end type Gf321FRV2x4x_t 
      
      
      
    ! Values of radiation pattern of charged dipole (3.27)
      type, public :: Ftf327RV2x4x_t
            integer(kind=i4) :: nFt327 ! number of values of radiation pattern of charged dipole (3.27)
            integer(kind=i4) :: ndf327 ! number of dipoles in dipole array (3.27)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: Ft
           !dir$ attributes align : 64 :: Ft
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: Ft
            !dir$ attributes align : 64 :: Ft
#endif                
      end type Ftf327RV2x4x_t
      
      
      ! Values of radiation pattern of charged dipole (3.27)
      ! Flat-array layout
      type, public :: Ftf327FRV2x4x_t
            integer(kind=i4) :: nFt327 ! number of values of radiation pattern of charged dipole (3.27)
            integer(kind=i4) :: ndf327 ! number of dipoles in dipole array (3.27)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: Ft
           !dir$ attributes align : 64 :: Ft
#else
            type(XMM4r4_t), dimension(:), allocatable :: Ft
            !dir$ attributes align : 64 :: Ft
#endif                
      end type Ftf327FRV2x4x_t
      
   
    ! Values for 'R' active impedance of charged vibrator (3.28)
      type, public :: Rf328RV2x4x_t
            integer(kind=i4) :: nRf328 ! number of values for 'R' active impedance of charged vibrator (3.28)
            integer(kind=i4) :: ndf328 ! number of dipoles in dipole array (3.28)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: R
           !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif             
      end type Rf328RV2x4x_t
      
      
      ! Values for 'R' active impedance of charged vibrator (3.28)
      ! Flat-array layout
      type, public :: Rf328FRV2x4x_t
            integer(kind=i4) :: nRf328 ! number of values for 'R' active impedance of charged vibrator (3.28)
            integer(kind=i4) :: ndf328 ! number of dipoles in dipole array (3.28)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: R
           !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif             
      end type Rf328FRV2x4x_t
      
      
    ! Values of ingress impedance of thin biconic dipole (3.31) 
      type, public :: rf331RV2x4x_t
            integer(kind=i4) :: nrf331 ! number of values of ingress impedance of thin biconic dipole (3.31)
            integer(kind=i4) :: ndf331 ! number of dipoles in dipole array (3.31)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: r
            !dir$ attributes align : 64 :: r
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: r
            !dir$ attributes align : 64 :: r
#endif            
      end type rf331RV2x4x_t
      
      
      ! Values of ingress impedance of thin biconic dipole (3.31) 
      ! Flat-array layout
      type, public :: rf331FRV2x4x_t
            integer(kind=i4) :: nrf331 ! number of values of ingress impedance of thin biconic dipole (3.31)
            integer(kind=i4) :: ndf331 ! number of dipoles in dipole array (3.31)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: r
            !dir$ attributes align : 64 :: r
#else
            type(XMM4r4_t), dimension(:), allocatable :: r
            !dir$ attributes align : 64 :: r
#endif            
      end type rf331FRV2x4x_t
      
      
      
   ! Values of total impedance of dipole (i.e. active,reactive) (3.33)  
      type, public :: Zf333RV2x4x_t
            integer(kind=i4) :: nZf333 ! number of values of total impedance of dipole (active,reactive) (3.33)
            integer(kind=i4) :: ndf333 ! number of dipoles in dipole array (3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#endif     
      end type Zf333RV2x4x_t 
      
      
       ! Values of total impedance of dipole (i.e. active,reactive) (3.33)  
       ! Flat-array layout
      type, public :: Zf333FRV2x4x_t
            integer(kind=i4) :: nZf333 ! number of values of total impedance of dipole (active,reactive) (3.33)
            integer(kind=i4) :: ndf333 ! number of dipoles in dipole array (3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#else
            type(XMM4r4_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#endif     
      end type Zf333FRV2x4x_t 
      
      
      
    ! Values of active impedance component of dipole (3.34, a part of 3.33)
      type, public :: Rf334RV2x4x_t
            integer(kind=i4) :: nRf334 ! number of values of active impedance component of dipole (3.34, a part of 3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: R
            !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif                 
      end type Rf334RV2x4x_t
      
      
       ! Values of active impedance component of dipole (3.34, a part of 3.33)
       ! Flat-array layout
      type, public :: Rf334FRV2x4x_t
            integer(kind=i4) :: nRf334 ! number of values of active impedance component of dipole (3.34, a part of 3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: R
            !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif                 
      end type Rf334FRV2x4x_t
      
      
      
      
    ! Values of reactive impedance component of dipole (3.34, a part of 3.33)  
      type, public :: Xf334RV2x4x_t
            integer(kind=i4) :: nXf334 ! number of values of reactive impedance component of dipole (3.34, a part of 3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: X
            !dir$ attributes align : 64 :: X
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: X
            !dir$ attributes align : 64 :: X
#endif                  
      end type Xf334RV2x4x_t
      
      
       ! Values of reactive impedance component of dipole (3.34, a part of 3.33)  
      type, public :: Xf334FRV2x4x_t
            integer(kind=i4) :: nXf334 ! number of values of reactive impedance component of dipole (3.34, a part of 3.33)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: X
            !dir$ attributes align : 64 :: X
#else
            type(XMM4r4_t), dimension(:), allocatable :: X
            !dir$ attributes align : 64 :: X
#endif                  
      end type Xf334FRV2x4x_t 
      
      
    ! Values of input impedance of the biconic dipole (3.35)
      type, public :: Zf335RV2x4x_t
            integer(kind=i4) :: nZf335 ! number of values of input impedance of the biconic dipole (3.35)
            integer(kind=i4) :: ndf335 ! number of dipoles in dipole array (3.35)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#endif              
      end type Zf335RV2x4x_t
      
      
     ! Values of input impedance of the biconic dipole (3.35)
     ! Flat array layout
      type, public :: Zf335FRV2x4x_t
            integer(kind=i4) :: nZf335 ! number of values of input impedance of the biconic dipole (3.35)
            integer(kind=i4) :: ndf335 ! number of dipoles in dipole array (3.35)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#else
            type(XMM4r4_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#endif              
      end type Zf335FRV2x4x_t 
      
      
    !  ! Values horizontal-component [phi,delta, radiation pattern] 
    ! of symmetric horizontal dipole (plane-parallel) (3.45)
      type, public :: Ff345RV2x4x_t
            integer(kind=i4) :: npf345 ! number of phi values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
            integer(kind=i4) :: ntf345 ! number of delta values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
            integer(kind=i4) :: ndf345 ! number of dipoles in dipole array (3.45)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif    
      end type Ff345RV2x4x_t
      
      
       !  ! Values horizontal-component [phi,delta, radiation pattern] 
    ! of symmetric horizontal dipole (plane-parallel) (3.45)
    ! Flat-array layout
      type, public :: Ff345FRV2x4x_t
            integer(kind=i4) :: npf345 ! number of phi values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
            integer(kind=i4) :: ntf345 ! number of delta values [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.45)
            integer(kind=i4) :: ndf345 ! number of dipoles in dipole array (3.45)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif    
      end type Ff345FRV2x4x_t
      
    !  ! Values vertical-component [phi,delta, radiation pattern] 
    ! of symmetric horizontal dipole (plane-parallel) (3.46)
      type, public :: Ff346RV2x4x_t
            integer(kind=i4) :: npf346 ! number of phi values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
            integer(kind=i4) :: ntf346 ! number of delta values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
            integer(kind=i4) :: ndf346 ! number of dipoles in dipole array (3.46)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
      end type Ff346RV2x4x_t
      
      
       !  ! Values vertical-component [phi,delta, radiation pattern] 
    ! of symmetric horizontal dipole (plane-parallel) (3.46)
    ! Flat-array layout
      type, public :: Ff346FRV2x4x_t
            integer(kind=i4) :: npf346 ! number of phi values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
            integer(kind=i4) :: ntf346 ! number of delta values horizontal-part [radiation pattern] of symmetric horizontal dipole (plane-parallel) (3.46)
            integer(kind=i4) :: ndf346 ! number of dipoles in dipole array (3.46)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
      end type Ff346FRV2x4x_t
      
      
      
   ! Values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
      type, public :: Ff34950RV2x4x_t
            integer(kind=i4) :: nFf349 ! number of values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
            integer(kind=i4) :: nFf350 ! number of values of horizontal-component of radiation pattern [ideally conducting earth] (3.50)
            integer(kind=i4) :: ndf349 ! number of dipoles in dipole array (3.49,3.50)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: Fv
            type(XMM2r8_t), dimension(:,:), allocatable :: Fh
            !dir$ attributes align : 64 :: Fv
            !dir$ attributes align : 64 :: Fh
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: Fv
            type(XMM4r4_t), dimension(:,:), allocatable :: Fh
            !dir$ attributes align : 64 :: Fv
            !dir$ attributes align : 64 :: Fh
#endif                
      end type Ff34950RV2x4x_t
      
      
      ! Values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
      ! Flat-array layout
      type, public :: Ff34950FRV2x4x_t
            integer(kind=i4) :: nFf349 ! number of values of vertical-component of radiation pattern [ideally conducting earth] (3.49)
            integer(kind=i4) :: nFf350 ! number of values of horizontal-component of radiation pattern [ideally conducting earth] (3.50)
            integer(kind=i4) :: ndf349 ! number of dipoles in dipole array (3.49,3.50)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: Fv
            type(XMM2r8_t), dimension(:), allocatable :: Fh
            !dir$ attributes align : 64 :: Fv
            !dir$ attributes align : 64 :: Fh
#else
            type(XMM4r4_t), dimension(:), allocatable :: Fv
            type(XMM4r4_t), dimension(:), allocatable :: Fh
            !dir$ attributes align : 64 :: Fv
            !dir$ attributes align : 64 :: Fh
#endif                
      end type Ff34950FRV2x4x_t
      
      
      
      
    ! Values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
      type, public :: Ff352RV2x4x_t
            integer(kind=i4) :: nFf352 ! number of values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
            integer(kind=i4) :: ndf352 ! number of dipoles in dipole array (3.52)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif   
      end type Ff352RV2x4x_t
      
      
      ! Values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
      ! Flat-array layout
      type, public :: Ff352FRV2x4x_t
            integer(kind=i4) :: nFf352 ! number of values of vertical-component of radiation pattern of vertical symmetric dipole (3.52)
            integer(kind=i4) :: ndf352 ! number of dipoles in dipole array (3.52)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif   
      end type Ff352FRV2x4x_t 
      
      
      
    ! Values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
      type, public :: Ff366RV2x4x_t
            integer(kind=i4) :: nFf366 ! number of values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
            integer(kind=i4) :: ndf366 ! number of dipoles in dipole array (3.66)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff366RV2x4x_t
      
      
      ! Values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
      ! Flat-array layout
      type, public :: Ff366FRV2x4x_t
            integer(kind=i4) :: nFf366 ! number of values of vertical-component of radiation pattern for assymetrical cylindrical dipole (3.66)
            integer(kind=i4) :: ndf366 ! number of dipoles in dipole array (3.66)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff366FRV2x4x_t 
      
    ! Azimuthal 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.97)
      type, public :: Ff397aRV2x4x_t
            integer(kind=i4) :: npf397 ! number of azimuthal 'phi' coordinate values of radiation pattern for V-antenna (3.97)
            integer(kind=i4) :: ntf397 ! number of azimuthal 'theta' coordinate values of radiation pattern for V-antenna (3.97)
            integer(kind=i4) :: ndf397 ! number of dipoles in dipole array (3.97)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff397aRV2x4x_t 
      
      
      ! Azimuthal 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.97)
      ! Flat-array layout
      type, public :: Ff397aFRV2x4x_t
            integer(kind=i4) :: npf397 ! number of azimuthal 'phi' coordinate values of radiation pattern for V-antenna (3.97)
            integer(kind=i4) :: ntf397 ! number of azimuthal 'theta' coordinate values of radiation pattern for V-antenna (3.97)
            integer(kind=i4) :: ndf397 ! number of dipoles in dipole array (3.97)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff397aFRV2x4x_t  
      
   ! Meridional 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.98)
      type, public :: Ff397mRV2x4x_t
            integer(kind=i4) :: npf398 ! number of meridional 'phi' coordinate values of radiation pattern for V-antenna (3.98)
            integer(kind=i4) :: ntf398 ! number of meridional 'theta' coordinate values of radiation pattern for V-antenna (3.98)
            integer(kind=i4) :: ndf398 ! number of dipoles in dipole array (3.98)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff397mRV2x4x_t 
      
      
      ! Meridional 'phi' and 'theta' coordinate values of radiation pattern for V-antenna (3.98)
      ! Flat-array layout
      type, public :: Ff397mFRV2x4x_t
            integer(kind=i4) :: npf398 ! number of meridional 'phi' coordinate values of radiation pattern for V-antenna (3.98)
            integer(kind=i4) :: ntf398 ! number of meridional 'theta' coordinate values of radiation pattern for V-antenna (3.98)
            integer(kind=i4) :: ndf398 ! number of dipoles in dipole array (3.98)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff397mFRV2x4x_t 
      
   ! Impedance values of V-antenna located in the free space (3.99)
      type, public :: Rf399RV2x4x_t
            integer(kind=i4) :: nRf399 ! number of impedance values of V-antenna located in the free space (3.99)
            integer(kind=i4) :: ndf399 ! number of dipoles in dipole array (3.99)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: R
            !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif 
      end type Rf399RV2x4x_t
      
       ! Impedance values of V-antenna located in the free space (3.99)
       ! Flat-array layout
      type, public :: Rf399FRV2x4x_t
            integer(kind=i4) :: nRf399 ! number of impedance values of V-antenna located in the free space (3.99)
            integer(kind=i4) :: ndf399 ! number of dipoles in dipole array (3.99)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: R
            !dir$ attributes align : 64 :: R
#else
            type(XMM4r4_t), dimension(:), allocatable :: R
            !dir$ attributes align : 64 :: R
#endif 
      end type Rf399FRV2x4x_t
      
      
    ! Phi and theta values of radiation pattern for V-antenna (Pistolkors antenna) (3.100) 
      type, public :: Ff3100RV2x4x_t
            integer(kind=i4) :: npf3100! number of phi values of radiation pattern for V-antenna (Pistolkors antenna) (3.100)
            integer(kind=i4) :: ntf3100! number of theta values of radiation pattern for V-antenna (Pistolkors antenna) (3.100)
            integer(kind=i4) :: ndf3100! number of dipoles in dipole array (3.100)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif               
      end type Ff3100RV2x4x_t
      
       ! Phi and theta values of radiation pattern for V-antenna (Pistolkors antenna) (3.100) 
       ! Flat-arrays layout
      type, public :: Ff3100FRV2x4x_t
            integer(kind=i4) :: npf3100! number of phi values of radiation pattern for V-antenna (Pistolkors antenna) (3.100)
            integer(kind=i4) :: ntf3100! number of theta values of radiation pattern for V-antenna (Pistolkors antenna) (3.100)
            integer(kind=i4) :: ndf3100! number of dipoles in dipole array (3.100)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif               
      end type Ff3100FRV2x4x_t
      
    ! Phi values [horizontal plane] of radiation pattern 
                               ! for V-antenna (Pistolkors antenna) (3.101)
      type, public :: Ff3101RV2x4x_t
            integer(kind=i4) :: npf3101! number of phi values [horizontal plane] of radiation pattern 
                               ! for V-antenna (Pistolkors antenna) (3.101)
            integer(kind=i4) :: ndf3101! number of dipoles in dipole array (3.101) 
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:,:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
      end type Ff3101RV2x4x_t
      
      
       ! Phi values [horizontal plane] of radiation pattern 
                               ! for V-antenna (Pistolkors antenna) (3.101)
       ! Flat-array layout
      type, public :: Ff3101FRV2x4x_t
            integer(kind=i4) :: npf3101! number of phi values [horizontal plane] of radiation pattern 
                               ! for V-antenna (Pistolkors antenna) (3.101)
            integer(kind=i4) :: ndf3101! number of dipoles in dipole array (3.101) 
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
      end type Ff3101FRV2x4x_t
      
    !  Values (W/cm) of critical voltage for dipole antenna (3.104)
      type, public :: Ef3104RV2x4x_t
            integer(kind=i4) :: nEf3104! number of values (W/cm) of critical voltage for dipole antenna (3.104)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: E
            !dir$ attributes align : 64 :: E
#else
            type(XMM4r4_t), dimension(:), allocatable :: E
            !dir$ attributes align : 64 :: E
#endif                 
      end type Ef3104RV2x4x_t
      
    ! Values for mutual impedance of two dipole antennae (3.110)
      type, public :: Zf3110RV2x4x_t
            integer(kind=i4) :: nZf3110! number of values for mutual impedance of two dipole antennae (3.110)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#else
            type(XMM4r4_t), dimension(:), allocatable :: Z
            !dir$ attributes align : 64 :: Z
#endif             
      end type Zf3110RV2x4x_t
      
    ! Phi values of radiation pattern for two-dipole antenna (horizontal-plane) (3.122)
      type, public :: Ff3122RV2x4x_t
            integer(kind=i4) :: ntf3122! number of phi values of radiation pattern for two-dipole antenna (horizontal plane)(3.122)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif             
      end type Ff3122RV2x4x_t
      
   ! Phi values of radiation pattern for two-dipole antenna 
                               ! plane-perpendicular to antennae axis(3.123)
     type, public :: Ff3123RV2x4x_t
           integer(kind=i4) :: ntf3123! number of phi values of radiation pattern for two-dipole antenna 
                               ! plane-perpendicular to antennae axis(3.123)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif               
     end type Ff3123RV2x4x_t
   
   ! Phi values of radiation pattern for two-dipole antenna (vertical-plane) (3.124)
     type, public :: Ff3124RV2x4x_t
           integer(kind=i4) :: ntf3124! number of phi values of radiation pattern for two-dipole antenna (vertical plane)(3.124)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
     end type Ff3124RV2x4x_t
     
   ! Theta values of radiation pattern for 2D array of dipoles (horizontal plane) (3.126)
     type, public :: Ff3126RV2x4x_t
           integer(kind=i4) :: ntf3126! number of theta values of radiation pattern for 2D array of dipoles (horizontal plane) (3.126)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
            type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
            type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
     end type Ff3126RV2x4x_t
     
   ! Theta values of radiation pattern for 2D array of dipoles (vertical plane) (3.127)
     type, public :: Ff3127RV2x4x_t
           integer(kind=i4) :: ntf3127! number of theta values of radiation pattern for 2D array of dipoles (vertical plane) (3.127)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
           type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
           type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif            
     end type Ff3127RV2x4x_t
     
   ! Theta values (horizontal-plane) of radiation pattern for aperiodic reflector (3.136)
     type, public :: Ff3136RV2x4x_t
           integer(kind=i4) :: ntf3136! number of theta values (horizontal-plane) of radiation pattern for aperiodic reflector (3.136)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
           type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
           type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif              
     end type Ff3136RV2x4x_t
     
   ! Delta values (vertical-plane) of radiation pattern for aperiodic reflector (3.137)
     type, public :: Ff3137RV2x4x_t
           integer(kind=i4) :: ntf3137! number of delta values (vertical-plane) of radiation pattern for aperiodic reflector (3.137)
#if (DIPOLE_ANT_TYPES_USE_PRECISION_REAL8) == 1
           type(XMM2r8_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#else
           type(XMM4r4_t), dimension(:), allocatable :: F
            !dir$ attributes align : 64 :: F
#endif  
     end type Ff3137RV2x4x_t
    
    
   
end module dipole_ant_types_sse
 
   
  
 
 



 

 
    
 
    
  
    
  
  
    
 
    
 
    
 
    
 
    
  
    
 
    
  
    
 
    
  
    
   
   
    
  
    
 
    
  
    
 
    
  
    
 
  
    
  
    
  
    
  
    
 
    
 
    
   
    

