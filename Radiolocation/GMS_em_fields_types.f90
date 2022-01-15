

module em_fields_types


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'em_fields_types'
 !          
 !          Purpose:
  !                     This module contains various implementations of
 !                      Electro-Magnetics fields both PAOS types and SoA types.
 !          History:
 !                        
 !                        Date: 15-01-2022
 !                        Time: 12:56 GMT+2
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
     use mod_avx512c16f32
     use mod_avx512c8f64
     use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
     implicit none
     public


     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: EM_FIELDS_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: EM_FIELDS_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: EM_FIELDS_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4), parameter :: EM_FIELDS_TYPES_FULLVER = &
          1000*EM_FIELDS_TYPES_MAJOR+100*EM_FIELDS_TYPES_MINOR+10*EM_FIELDS_TYPES_MICRO
     ! Module creation date
     character(*),       parameter :: EM_FIELDS_TYPES_CREATION_DATE = "15-01-2022 12:53 +00200 (SAT 15 DEC 2022 GMT+2)"
     ! Module build date
     character(*),       parameter :: EM_FIELDS_TYPES_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: EM_FIELDS_TYPES_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: EM_FIELDS_TYPES_SYNOPSIS      = " Computational ElectroMagnetics fields data types PAoS and SoA."

     !==============================================================
     !                   PAoS data types
     !==============================================================

       ! Complex Electric Field 1D (Packed-AoS) data type decomposed.
     type, public :: H_X_C16
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif   
      end type H_X_C16

     ! Complex Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_C16
          
           integer(kind=i4) :: n_cells  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
#endif    
       end type H_XY_C16

     

     ! Complex Electric Field 3D (Packed-AoS) data type decomposed.
     type, public :: H_XYZ_C16
         
           integer(kind=i4) :: n_cell  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y
           type(Zmm16c4), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           type(Zmm16c4), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_C16
        


         ! Real Electric Field 3D (Packed-AoS) data type decomposed.
      type, public :: H_XYZ_R16
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x
           type(ZMM16r4_t), allocatable, dimension(:) :: H_y
           type(Zmm16r4_t), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM16r4_t), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           type(Zmm16r4_t), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_R16


      ! Real Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_R16
           
           integer(kind=i4) :: n_cells 
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x
           type(ZMM16r4_t), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM16r4_t), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
          
#endif

      end type H_XY_R16

        
      ! Real Electric Field 1D (Packed-AoS) data type decomposed.
      type, public :: H_X_R16
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif  
     
      end type H_X_R16

     ! Complex Magnetic Field 1D (Packed-AoS) data type decomposed.  
     type, public :: B_X_C16
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif             

     end type B_X_C16


       ! Complex Magnetic  Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_C16
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: B_x
           type(ZMM16c4), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM16c4), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
#endif    
       end type B_XY_C16
     

     ! Complex Magnetic Field 3D (Packed-AoS) data type decomposed.
     type, public :: B_XYZ_C16
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: B_x
           type(ZMM16c4), allocatable, dimension(:) :: B_y
           type(Zmm16c4), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM16c4), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           type(Zmm16c4), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_C16


      ! Real Magnetic Field 3D (Packed-AoS) data type decomposed.
      type, public :: B_XYZ_R16
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x
           type(ZMM16r4_t), allocatable, dimension(:) :: B_y
           type(Zmm16r4_t), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM16r4_t), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           type(Zmm16r4_t), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_R16

      
      ! Real Magnetic Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_R16
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
            
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x
           type(ZMM16r4_t), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM16r4_t), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
          
#endif

       end type B_XY_R16


      ! Real Magnetic Field 1D (Packed-AoS) data type decomposed.
      type, public :: B_X_R16
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16r4_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif  
     
      end type B_X_R16 
      
        
      ! Complex Surface currents 3D (Packed-AoS) data type.
      type, public :: Js_XYZ_C16
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches, each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM16c4), allocatable, dimension(:) :: Js_x
           type(ZMM16c4), allocatable, dimension(:) :: Js_y
           type(ZMM16c4), allocatable, dimension(:) :: Js_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: Js_x !GCC$ ATTRIBUTES aligned(64) :: Js_x
           type(ZMM16c4), allocatable, dimension(:) :: Js_y !GCC$ ATTRIBUTES aligned(64) :: Js_y
           type(ZMM16c4), allocatable, dimension(:) :: Js_z !GCC$ ATTRIBUTES aligned(64) :: Js_z
#endif         
            
      end type Js_XYZ_C16

      !=======================================================================================!
      !!               Packed AoS double precision data types definitions
      !=======================================================================================!
      
         ! Complex Electric Field 1D (Packed-AoS) data type decomposed.
     type, public :: H_X_C8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches for MoM code.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif   
      end type H_X_C8

     ! Complex Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_C8
          
           integer(kind=i4) :: n_cells  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: H_x
           type(ZMM8c8), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM8c8), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
#endif    
       end type H_XY_C8

     

     ! Complex Electric Field 3D (Packed-AoS) data type decomposed.
     type, public :: H_XYZ_C8
           
           integer(kind=i4) :: n_cell  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: H_x
           type(ZMM8c8), allocatable, dimension(:) :: H_y
           type(Zmm8c8), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM8c8), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           type(Zmm8c8), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

      end type H_XYZ_C8

      ! Real Electric Field 3D (Packed-AoS) data type decomposed.
      type, public :: H_XYZ_R8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x
           type(ZMM8r8_t), allocatable, dimension(:) :: H_y
           type(Zmm8r8_t), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM8r8_t), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           type(Zmm8r8_t), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_R8


      ! Real Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_R8
            
           integer(kind=i4) :: n_cells 
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x
           type(ZMM8r8_t), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM8r8_t), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
          
#endif

      end type H_XY_R8

        
      ! Real Electric Field 1D (Packed-AoS) data type decomposed.
      type, public :: H_X_R8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif  
     
      end type H_X_R8

     ! Complex Magnetic Field 1D (Packed-AoS) data type decomposed.  
     type, public :: B_X_C8
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif             

     end type B_X_C8


       ! Complex Magnetic  Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_C8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: B_x
           type(ZMM8c8), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM8c8), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
#endif    
       end type B_XY_C8
     

     ! Complex Magnetic Field 3D (Packed-AoS) data type decomposed.
     type, public :: B_XYZ_C8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: B_x
           type(ZMM8c8), allocatable, dimension(:) :: B_y
           type(Zmm8c8), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM8c8), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           type(Zmm8c8), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_C8


      ! Real Magnetic Field 3D (Packed-AoS) data type decomposed.
      type, public :: B_XYZ_R8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x
           type(ZMM8r8_t), allocatable, dimension(:) :: B_y
           type(Zmm8r8_t), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM8r8_t), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           type(Zmm8r8_t), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_R8

      
      ! Real Magnetic Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_R8
            
            integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
            
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x
           type(ZMM8r8_t), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           type(ZMM8r8_t), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
          
#endif

       end type B_XY_R8


      ! Real Magnetic Field 1D (Packed-AoS) data type decomposed.
      type, public :: B_X_R8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8r8_t), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif  
     
      end type B_X_R8
      
        
      ! Complex Surface currents 3D (Packed-AoS) data type.
      type, public :: Js_XYZ_C8
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches, each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           type(ZMM8c8), allocatable, dimension(:) :: Js_x
           type(ZMM8c8), allocatable, dimension(:) :: Js_y
           type(ZMM8c8), allocatable, dimension(:) :: Js_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM8c8), allocatable, dimension(:) :: Js_x !GCC$ ATTRIBUTES aligned(64) :: Js_x
           type(ZMM8c8), allocatable, dimension(:) :: Js_y !GCC$ ATTRIBUTES aligned(64) :: Js_y
           type(ZMM8c8), allocatable, dimension(:) :: Js_z !GCC$ ATTRIBUTES aligned(64) :: Js_z
#endif         
            
      end type Js_XYZ_C8


      !==============================================================        
      !                    SoA data types
      !==============================================================



        ! Complex Electric Field 1D (SoA) data type decomposed.
     type, public :: H_X_C1_4
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif   
      end type H_X_C1_4

     ! Complex Electric Field 2D (SoA) data type decomposed.
      type, public :: H_XY_C1_4
          
           integer(kind=i4) :: n_cells  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: H_x
           complex(kind=sp), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           complex(kind=sp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
#endif    
       end type H_XY_C1_4

     

     ! Complex Electric Field 3D (SoA) data type decomposed.
     type, public :: H_XYZ_C1_4
         
           integer(kind=i4) :: n_cell  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y
           type(Zmm16c4), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           type(ZMM16c4), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           type(ZMM16c4), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           type(Zmm16c4), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_C1_4
        


         ! Real Electric Field 3D (Packed-AoS) data type decomposed.
      type, public :: H_XYZ_R1_4
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: H_x
           real(kind=sp), allocatable, dimension(:) :: H_y
           real(kind=sp), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           real(kind=sp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           real(kind=sp), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_R1_4


      ! Real Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_R1_4
           
           integer(kind=i4) :: n_cells 
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: H_x
           real(kind=sp), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           real(kind=sp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
          
#endif

      end type H_XY_R1_4

        
      ! Real Electric Field 1D (Packed-AoS) data type decomposed.
      type, public :: H_X_R1_4
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: H_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif  
     
      end type H_X_R1_4

     ! Complex Magnetic Field 1D (Packed-AoS) data type decomposed.  
     type, public :: B_X_C1_4
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif             

     end type B_X_C1_4


       ! Complex Magnetic  Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_C1_4
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: B_x
           complex(kind=sp), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           complex(kind=sp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
#endif    
       end type B_XY_C1_4
     

     ! Complex Magnetic Field 3D (Packed-AoS) data type decomposed.
     type, public :: B_XYZ_C1_4
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: B_x
           complex(kind=sp), allocatable, dimension(:) :: B_y
           complex(kind=sp), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           complex(kind=sp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           complex(kind=sp), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_C1_4


      ! Real Magnetic Field 3D (Packed-AoS) data type decomposed.
      type, public :: B_XYZ_R1_4
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: B_x
           real(kind=sp), allocatable, dimension(:) :: B_y
           real(kind=sp), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           real(kind=sp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           real(kind=sp), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_R1_4

      
      ! Real Magnetic Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_R1_4
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
            
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: B_x
           real(kind=sp), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           real(kind=sp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
          
#endif

       end type B_XY_R1_4


      ! Real Magnetic Field 1D (Packed-AoS) data type decomposed.
      type, public :: B_X_R1_4
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=sp), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=sp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif  
     
      end type B_X_R1_4 
      
        
      ! Complex Surface currents 3D (Packed-AoS) data type.
      type, public :: Js_XYZ_C1_4
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches, each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=sp), allocatable, dimension(:) :: Js_x
           complex(kind=sp), allocatable, dimension(:) :: Js_y
           complex(kind=sp), allocatable, dimension(:) :: Js_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: Js_x !GCC$ ATTRIBUTES aligned(64) :: Js_x
           complex(kind=sp), allocatable, dimension(:) :: Js_y !GCC$ ATTRIBUTES aligned(64) :: Js_y
           complex(kind=sp), allocatable, dimension(:) :: Js_z !GCC$ ATTRIBUTES aligned(64) :: Js_z
#endif         
            
      end type Js_XYZ_C1_4

      !=======================================================================================!
      !!               Packed AoS double precision data types definitions
      !=======================================================================================!
      
         ! Complex Electric Field 1D (Packed-AoS) data type decomposed.
     type, public :: H_X_C1_8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches for MoM code.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif   
      end type H_X_C1_8

     ! Complex Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_C1_8
          
           integer(kind=i4) :: n_cells  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: H_x
           complex(kind=dp), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           complex(kind=dp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
#endif    
       end type H_XY_C1_8

     

     ! Complex Electric Field 3D (Packed-AoS) data type decomposed.
     type, public :: H_XYZ_C1_8
           
           integer(kind=i4) :: n_cell  ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: H_x
           complex(kind=dp), allocatable, dimension(:) :: H_y
           complex(kind=dp), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           complex(kind=dp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           complex(kind=dp), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

      end type H_XYZ_C1_8

      ! Real Electric Field 3D (Packed-AoS) data type decomposed.
      type, public :: H_XYZ_R1_8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: H_x
           real(kind=dp), allocatable, dimension(:) :: H_y
           real(kind=dp), allocatable, dimension(:) :: H_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           real(kind=dp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
           real(kind=dp), allocatable, dimension(:) :: H_z !GCC$ ATTRIBUTES aligned(64) :: H_z
#endif

       end type H_XYZ_R1_8


      ! Real Electric Field 2D (Packed-AoS) data type decomposed.
      type, public :: H_XY_R1_8
            
           integer(kind=i4) :: n_cells 
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: H_x
           real(kind=dp), allocatable, dimension(:) :: H_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: H_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
           real(kind=dp), allocatable, dimension(:) :: H_y !GCC$ ATTRIBUTES aligned(64) :: H_y
          
#endif

      end type H_XY_R1_8

        
      ! Real Electric Field 1D (Packed-AoS) data type decomposed.
      type, public :: H_X_R1_8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: H_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: H_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: H_x !GCC$ ATTRIBUTES aligned(64) :: H_x
#endif  
     
      end type H_X_R1_8

     ! Complex Magnetic Field 1D (Packed-AoS) data type decomposed.  
     type, public :: B_X_C1_8
           
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif             

     end type B_X_C1_8


       ! Complex Magnetic  Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_C1_8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: B_x
           complex(kind=dp), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           complex(kind=dp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
#endif    
       end type B_XY_C1_8
     

     ! Complex Magnetic Field 3D (Packed-AoS) data type decomposed.
     type, public :: B_XYZ_C1_8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: B_x
           complex(kind=dp), allocatable, dimension(:) :: B_y
           complex(kind=dp), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           complex(kind=dp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           complex(kind=dp), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_C1_8


      ! Real Magnetic Field 3D (Packed-AoS) data type decomposed.
      type, public :: B_XYZ_R1_8
            
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: B_x
           real(kind=dp), allocatable, dimension(:) :: B_y
           real(kind=dp), allocatable, dimension(:) :: B_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           real(kind=dp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
           real(kind=dp), allocatable, dimension(:) :: B_z !GCC$ ATTRIBUTES aligned(64) :: B_z
#endif

      end type B_XYZ_R1_8

      
      ! Real Magnetic Field 2D (Packed-AoS) data type decomposed.
      type, public :: B_XY_R1_8
            
            integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
            
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: B_x
           real(kind=dp), allocatable, dimension(:) :: B_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: B_y
        
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
           real(kind=dp), allocatable, dimension(:) :: B_y !GCC$ ATTRIBUTES aligned(64) :: B_y
          
#endif

       end type B_XY_R1_8


      ! Real Magnetic Field 1D (Packed-AoS) data type decomposed.
      type, public :: B_X_R1_8
          
           integer(kind=i4) :: n_cells ! number of cells i.e. patches each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           real(kind=dp), allocatable, dimension(:) :: B_x
            !DIR$ ATTRIBUTES ALIGN : 64 :: B_x
         
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           real(kind=dp), allocatable, dimension(:) :: B_x !GCC$ ATTRIBUTES aligned(64) :: B_x
#endif  
     
      end type B_X_R1_8
      
        
      ! Complex Surface currents 3D (Packed-AoS) data type.
      type, public :: Js_XYZ_C1_8
           
            integer(kind=i4) :: n_cells ! number of cells i.e. patches, each Packed-AoS operates on 16 cells
                                        ! simultaneously.
#if defined(__INTEL_COMPILER) || defined(__ICC)        
           complex(kind=dp), allocatable, dimension(:) :: Js_x
           complex(kind=dp), allocatable, dimension(:) :: Js_y
           complex(kind=dp), allocatable, dimension(:) :: Js_z
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_x
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_y
           !DIR$ ATTRIBUTES ALIGN : 64 :: Js_z
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: Js_x !GCC$ ATTRIBUTES aligned(64) :: Js_x
           complex(kind=dp), allocatable, dimension(:) :: Js_y !GCC$ ATTRIBUTES aligned(64) :: Js_y
           complex(kind=dp), allocatable, dimension(:) :: Js_z !GCC$ ATTRIBUTES aligned(64) :: Js_z
#endif         
            
      end type Js_XYZ_C1_8














end module em_fields_types
