
#include "GMS_config.fpp"


module antenna_common_adt_avx512



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         antenna_common_adt_avx512
 !          
 !          Purpose:
 !                        
 !                        Various characteristics of different antenna common formulae and computational characteristics.
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 10-12-2023
 !                        Time: 15:37 GMT+2
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
   
    use mod_kinds,        only : i4
    use use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
    use avx512_cvec8,     only : ZMM8c8
    use avx512_cvec16,    only : ZMM16c4
    public
    implicit none

    ! Major version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_AVX512_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_AVX512_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_AVX512_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_AVX512_FULLVER =   &
            1000*ANTENNA_COMMON_ADT_AVX512_MAJOR+100*ANTENNA_COMMON_ADT_AVX512_MINOR+10*ANTENNA_COMMON_ADT_AVX512_MICRO
    ! Module creation date
    character(*),        parameter :: ANTENNA_COMMON_ADT_AVX512_CREATE_DATE = "10-12-2023 15:37 +00200 (SUN 10 DEC 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: ANTENNA_COMMON_ADT_AVX512_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: ANTENNA_COMMON_ADT_AVX512_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: ANTENNA_COMMON_ADT_AVX512_SYNOPSIS    = "Data describing various common antenna characteristics -- derived types AVX512512-based."

  ! Default setting is 0. 
#if !defined(ANTENNA_COMMON_ADT_AVX512_USE_PRECISION_REAL8)
#define ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8 0
#endif  


  
   
    type, public :: Data_size_t
          sequence
          integer(kind=i4) :: nith    ! number of theta integration points
          integer(kind=i4) :: niph    ! number of phi integration points
          integer(kind=i4) :: noth    ! number of theta observation points
          integer(kind=i4) :: noph    ! number of phi obsertvation points
    
          integer(kind=i4) :: nsur    ! number of R components of spherical coordinate unit vector
          integer(kind=i4) :: nsup    ! number of Phi components of spherical coordinate unit vector
          integer(kind=i4) :: nsut    ! number of Theta components of spherical coordinate unit vector
    
          integer(kind=i4) :: nrx    ! number of x components of normal vectors
          integer(kind=i4) :: nry    ! number of y components of normal vectors
          integer(kind=i4) :: nrz    ! number of z components of normal vectors
    
          integer(kind=i4) :: nirx    ! number of Rx elements of integration points.
          integer(kind=i4) :: niry    ! number of Ry elements of integration points.
          integer(kind=i4) :: nirz    ! number of Rz elements of integration points.
    
          integer(kind=i4) :: norx    ! number of Rx elements of observation points.
          integer(kind=i4) :: nory    ! number of Ry elements of observation points.
          integer(kind=i4) :: norz    ! number of Rz elements of observation points.
    end type Data_size_t
    
   
         ! Dynamic derived type for EMF modeling (complex-single) AoS-layout
    type, public :: DC3D_aos_cv8x16x_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
        type(ZMM8c8), dimension(:,:), allocatable :: fx
        type(ZMM8c8), dimension(:,:), allocatable :: fy
        type(ZMM8c8), dimension(:,:), allocatable :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#else       
        type(ZMM16c4), dimension(:,:), allocatable :: fx
        type(ZMM16c4), dimension(:,:), allocatable :: fy
        type(ZMM16c4), dimension(:,:), allocatable :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#endif
    end type DC3D_aos_cv8x16x_t
    
    ! Dynamic derived type for EMF modeling (complex-single) flat-layout
    type, public :: DC3D_flat_cv8x16x_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
        type(ZMM8c8), dimension(:), allocatable :: fx ! length is: ne*nx
        type(ZMM8c8), dimension(:), allocatable :: fy ! length is: ne*ny
        type(ZMM8c8), dimension(:), allocatable :: fz ! length is: ne*nz
#else        
        type(ZMM16c4), dimension(:), allocatable :: fx ! length is: ne*nx
        type(ZMM16c4), dimension(:), allocatable :: fy ! length is: ne*ny
        type(ZMM16c4), dimension(:), allocatable :: fz ! length is: ne*nz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#endif
    end type DC3D_flat_cv8x16x_t
   
    
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) AoS-layout
    type, public :: DC3D_aos_rv8x16x_t
    ! First dimension nth field, second dimension number of sample points
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
        type(ZMM8r8_t), dimension(:,:), allocatable :: fxr
        type(ZMM8r8_t), dimension(:,:), allocatable :: fxi
        type(ZMM8r8_t), dimension(:,:), allocatable :: fyr
        type(ZMM8r8_t), dimension(:,:), allocatable :: fyi
        type(ZMM8r8_t), dimension(:,:), allocatable :: fzr
        type(ZMM8r8_t), dimension(:,:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        type(ZMM16r4_t), dimension(:,:), allocatable :: fxr
        type(ZMM16r4_t), dimension(:,:), allocatable :: fxi
        type(ZMM16r4_t), dimension(:,:), allocatable :: fyr
        type(ZMM16r4_t), dimension(:,:), allocatable :: fyi
        type(ZMM16r4_t), dimension(:,:), allocatable :: fzr
        type(ZMM16r4_t), dimension(:,:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type DC3D_aos_rv2xrx_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) flat-layout
    type, public :: DC3D_flat_rv8x16x_t
    ! First dimension nth field, second dimension number of sample points
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
        type(ZMM8r8_t), dimension(:), allocatable :: fxr
        type(ZMM8r8_t), dimension(:), allocatable :: fxi
        type(ZMM8r8_t), dimension(:), allocatable :: fyr
        type(ZMM8r8_t), dimension(:), allocatable :: fyi
        type(ZMM8r8_t), dimension(:), allocatable :: fzr
        type(ZMM8r8_t), dimension(:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        type(ZMM16r4_t), dimension(:), allocatable :: fxr
        type(ZMM16r4_t), dimension(:), allocatable :: fxi
        type(ZMM16r4_t), dimension(:), allocatable :: fyr
        type(ZMM16r4_t), dimension(:), allocatable :: fyi
        type(ZMM16r4_t), dimension(:), allocatable :: fzr
        type(ZMM16r4_t), dimension(:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type DC3D_flat_rv8x16x_t
    
         
      
    ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) AoS-layout
    type, public :: DC2D_aos_cv8x16x_t
    ! First dimension nth field, second dimension number of sample points 
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
       type(ZMM8c8), dimension(:,:), allocatable :: fth
       type(ZMM8c8), dimension(:,:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else       
       type(ZMM16c4), dimension(:,:), allocatable :: fth
       type(ZMM16c4), dimension(:,:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type DC2D_aos_cv8x16x_t
    
    
     ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) flat-layout
    type, public :: DC2D_flat_cv8x16x_t
    ! First dimension nth field, second dimension number of sample points 
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
       type(ZMM8c8), dimension(:), allocatable :: fth
       type(ZMM8c8), dimension(:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else
       type(ZMM16c4), dimension(:), allocatable :: fth
       type(ZMM16c4), dimension(:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type DC2D_flat_cv8x16x_t
    
    
  
  
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) AoS-layout
    type, public :: DC2D_aos_rv8x16x_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
       type(ZMM8r8_t), dimension(:,:), allocatable :: fthr
       type(ZMM8r8_t), dimension(:,:), allocatable :: fthi
       type(ZMM8r8_t), dimension(:,:), allocatable :: fphr
       type(ZMM8r8_t), dimension(:,:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#else       
       type(ZMM16r4_t), dimension(:,:), allocatable :: fthr
       type(ZMM16r4_t), dimension(:,:), allocatable :: fthi
       type(ZMM16r4_t), dimension(:,:), allocatable :: fphr
       type(ZMM16r4_t), dimension(:,:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type DC2D_aos_rv8x16x_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) flat-layout
    type, public :: DC2D_flat_rv8x16x_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1
       type(ZMM8r8_t), dimension(:), allocatable :: fthr
       type(ZMM8r8_t), dimension(:), allocatable :: fthi
       type(ZMM8r8_t), dimension(:), allocatable :: fphr
       type(ZMM8r8_t), dimension(:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi 
#else       
       type(ZMM16r4_t), dimension(:), allocatable :: fthr
       type(ZMM16r4_t), dimension(:), allocatable :: fthi
       type(ZMM16r4_t), dimension(:), allocatable :: fphr
       type(ZMM16r4_t), dimension(:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type DC2D_flat_rv8x16x_t
    
    
       
    
    
    ! Normalized function Psi (power of radiated field)
    type, public :: Pf239RV8x16x_t
       integer(kind=i4) :: nth  !theta-dim
       integer(kind=i4) :: nph  !phi-dim
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1      
       type(ZMM8r8_t), dimension(:,:), allocatable :: P
       !dir$ attributes align : 64 :: P
#else      
      type(ZMM16r4_t), dimension(:,:), allocatable :: P
       !dir$ attributes align : 64 :: P
#endif
    end type Pf239RV8x16x_t
  
    
    ! Elementary electric dipoles (radiation patterns)
    type, public :: RPf240RV8x16x_t 
       integer(kind=i4) :: nft240  ! number of values of elementary electric dipole (2.40)
       integer(kind=i4) :: ndf240  ! number of elementary dipoles (2.40)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1          
       type(ZMM8r8_t), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf240RV8x16x_t
    
    ! Sinusoidal current distribution (2.43)
    type, public :: Izf243CV8x16x_t
       integer(kind=i4) :: nzf243  ! number of current distribution 'z' values (2.43)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1         
       type(ZMM8c8), dimension(:), allocatable :: Iz
       !dir$ attributes align : 64 :: Iz
#else
       type(ZMM16c4), dimension(:), allocatable :: Iz
       !dir$ attributes align : 64 :: f243r8
#endif
    end type Izf243CV8x16x_t
    
    ! Radiation pattern of similiar EM radiators (2.96)
    type, public :: RPf296RV8x16x_t
       integer(kind=i4) :: ntf296  ! number of theta values (2.96)
       integer(kind=i4) :: npf296  ! number of phi values (2.96)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1           
       type(ZMM8r8_t), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf296RV8x16x_t
    
    !Linear phase error values apperture edge (2.98)
    type, public :: PEf298RV8x16x_t
       integer(kind=i4) :: npf298  ! number of linear phase error values apperture edge (2.98)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1          
       type(ZMM8r8_t), dimension(:),   allocatable :: f298r4
       !dir$ attributes align : 64 :: pe
#else
       type(ZMM16r4_t), dimension(:),   allocatable :: f298r8
       !dir$ attributes align : 64 :: pe
#endif
    end type PEf298RV8x16x_t
    
    !Radiation pattern including linear phase error term (2.100)
    type, public :: RPf2100RV8x16x_t
       integer(kind=i4) :: nftf2100 ! number of values for radiation pattern linear phase error (2.100)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1    
       type(ZMM8r8_t), dimension(:),   allocatable :: rp
#else
       !dir$ attributes align : 64 :: rp
       type(ZMM16r4_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    type, public :: RPf2100RV8x16x_t

    !Radiation pattern including quadratic phase error term (2.102)
    type, public :: RPf2102RV8x16x_t
       integer(kind=i4) :: nftf2102 ! number of values for radiation pattern quadratic phase error (2.102)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1   
       type(ZMM8r8_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf2102RV8x16x_t

    ! Radiation pattern cubic phase error and cosinusoidal amplitude distribution (2.107)
    type, public :: RPf2107RV8x16x_t
       integer(kind=i4) :: nfuf2107 ! number of values for radiation pattern cubic phase error
                                 ! and cosinusoidal amplitude distribution (2.107)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1                                    
       type(ZMM8r8_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif
    end type RPf2107RV8x16x_t
    
    
   ! Average of radiation pattern of 2D (single) array (2.110)
    type, public :: RPf2100RV8x16x_t
          integer(kind=i4) :: ntf2110  ! number of theta values 2D array (2.110)
          integer(kind=i4) :: npf2110  ! number of phi values 2D array (2.110)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          type(ZMM16r4_t), dimension(:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif
    end type RPf2100RV8x16x_t
    
  ! Average of radiation pattern of 1D (single) array (2.110a)
    type, public :: RPf2100aRV8x16x_t
          integer(kind=i4) :: ntf2110a ! number of theta values 1D array (2.110a)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1                                    
       type(ZMM8r8_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif          
    end type RPf2100aRV8x16x_t
    
    
  ! Power-averaged of radiation pattern of 2D (single) array (2.111)
    type, public :: RPf2111RV8x16x_t
          integer(kind=i4) :: ntf2111  ! number of theta values 2D array (2.111)
          integer(kind=i4) :: npf2111  ! number of phi values 2D array (2.111) 
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          type(ZMM16r4_t), dimension(:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif          
    end type RPf2111RV8x16x_t
    
   
  ! Power-average of radiation pattern of 1D (single) array (2.111a)
    type, public :: RPf2111aRV8x16x_t
          integer(kind=i4) :: ntf211a  ! number of theta values 1D array (2.111a)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1                                    
       type(ZMM8r8_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       type(ZMM16r4_t), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif            
    end type RPf2111aRV8x16x_t 
    
   
  !  Phi values of coefficient of directional pattern (2.127)
    type, public :: DPf2127RV8x16x_t
          integer(kind=i4) :: npf2127  ! number of phi values of coefficient of directional pattern (2.127)
          integer(kind=i4) :: ntf2127  ! number of theta values of coefficient of directional pattern (2.127)
          integer(kind=i4) :: naf2127  ! number of radiating elements or discrete antennas (2.127)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:,:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          type(ZMM16r4_t), dimension(:,:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif                  
    end type DPf2127RV8x16x_t
   
  ! Values of real parts of m-th and n-th antenna impedance (2.143)
    type, public :: RMNf2143RV8x16x_t
          integer(kind=i4) :: nrmnf2143 ! number of real parts of m-th and n-th antenna impedance (2.143)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: rmn
           !dir$ attributes align : 64 :: rmn
#else
          type(ZMM16r4_t), dimension(:), allocatable :: rmn
          !dir$ attributes align : 64 :: rmn
#endif  
    end type RMNf2143RV8x16x_t
    
    
  ! Values of imaginary parts of m-th and n-th antenna impedance (2.144)
    type, public :: RMNf2144RV8x16x_t
          integer(kind=i4) :: nxmnf2144 ! number of imaginary parts of m-th and n-th antenna impedance (2.144)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: rmn
           !dir$ attributes align : 64 :: rmn
#else
          type(ZMM16r4_t), dimension(:), allocatable :: rmn
          !dir$ attributes align : 64 :: rmn
#endif            
    end type RMNf2144RV8x16x_t
    
    
  ! Values of mutual impedance of two antennas as a function of their distance (2.145)
    type, public :: R12f2145RV8x16x_t
          integer(kind=i4) :: nrf2145   ! number of values of mutual impedance of two antennas as a function of their distance (2.145)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: r12
           !dir$ attributes align : 64 :: r12
#else
          type(ZMM16r4_t), dimension(:), allocatable :: r12
          !dir$ attributes align : 64 :: r12
#endif           
    end type R12f2145RV8x16x_t                     

    
  ! Values of real parts of m-th and n-th antenna impedance (2.148)
    type, public :: XMNf2148RV8x16x_t
          integer(kind=i4) :: nxmnf2148 ! number of real parts of m-th and n-th antenna impedance (2.148)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: xmn
           !dir$ attributes align : 64 :: xmn
#else
          type(ZMM16r4_t), dimension(:), allocatable :: xmn
          !dir$ attributes align : 64 :: xmn
#endif            
    end type XMNf2148RV8x16x_t
    
    
  ! Theta values for complex radiating pattern (2.149)
    type, public :: RPf2149CV8x16x_t
          integer(kind=i4) :: nft2149   ! number of theta values for complex radiating pattern (2.149)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          type(ZMM16c4), dimension(:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif          
    end type RPf2149CV8x16x_t
    
    
  ! Values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    type, public :: R12f2150RV8x16x_t
          integer(kind=i4) :: nrf2150   ! number of values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: r12
           !dir$ attributes align : 64 :: r12
#else
          type(ZMM16r4_t), dimension(:), allocatable :: r12
          !dir$ attributes align : 64 :: r12
#endif                                   
    end type R12f2150RV8x16x_t 
    
  
  ! Values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    type, public :: X12f2150RV8x16x_t
          integer(kind=i4) :: nxf2150   !number of values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: x12
           !dir$ attributes align : 64 :: x12
#else
          type(ZMM16r4_t), dimension(:), allocatable :: x12
          !dir$ attributes align : 64 :: x12
#endif             
    end type X12f2150RV8x16x_t
    
    
  ! The values 'height' of an antenna (EM-meaning) (2.153)
    type, public :: HGf2153RV8x16x_t
          integer(kind=i4) :: nh2153    ! number of 'height' values of antenna (EM-meaning)  (2.154)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: hg
           !dir$ attributes align : 64 :: hg
#else
          type(ZMM16r4_t), dimension(:), allocatable :: hg
          !dir$ attributes align : 64 :: hg
#endif                 
    end type HGf2153RV8x16x_t
     
   
  ! The values 'height' of an antenna (EM-meaning) symmetric vibrator (2.154)
    type, public :: HSf2154RV8x16x_t
          integer(kind=i4) :: nh2154    ! number of 'height' values of antenna (EM-meaning) symmetric vibrator (2.154)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: hs
           !dir$ attributes align : 64 :: hs
#else
          type(ZMM16r4_t), dimension(:), allocatable :: hs
          !dir$ attributes align : 64 :: hs
#endif              
    end type HSf2154RV8x16x_t 
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
    type, public :: Af2159RV8x16x_t
          integer(kind=i4) :: naf2159   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          type(ZMM16r4_t), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                   
    end type Af2159RV8x16x_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
    type, public :: Af2160RV8x16x_t
          integer(kind=i4) :: naf2160   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          type(ZMM16r4_t), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                  
    end type Af2160RV8x16x_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric apperture (2.161)
    type, public :: Af2161RV8x16x_t
          integer(kind=i4) :: naf2161   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric aperture (2.161)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          type(ZMM16r4_t), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                    
    end type Af2161RV8x16x_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial to Electric field tangent to apperture (2.162)
    type, public :: Af2162RV8x16x_t
          integer(kind=i4) :: naf2162   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial orientation of E-field tangent to apperture (2.162)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          type(ZMM16r4_t), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif  
    end type Af2162RV8x16x_t
    
    
  ! The values of complex Fresnel coefficients, vertical polarization (2.169)
    type, public :: Rvf2169CV8x16x_t
          integer(kind=i4) :: nRvf2169  ! number of complex Fresnel coefficients, vertical polarization (2.169)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:), allocatable :: rv
           !dir$ attributes align : 64 :: rv
#else
          type(ZMM16c4), dimension(:), allocatable :: rv
          !dir$ attributes align : 64 :: rv
#endif            
    end type Rvf2169CV8x16x_t
     
  
  ! The values of complex Fresnel coefficients, horizontal polarization (2.170)
    type, public :: Rvf2170CV8x16x_t
          integer(kind=i4) :: nRhf2170  ! number of complex Fresnel coefficients, horizontal polarization (2.170)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:), allocatable :: rv
           !dir$ attributes align : 64 :: rv
#else
          type(ZMM16c4), dimension(:), allocatable :: rv
          !dir$ attributes align : 64 :: rv
#endif             
    end type Rvf2170CV8x16x_t
    
    
  ! The values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
    type, public :: Evf2172CV8x16x_t
          integer(kind=i4) :: nEvf2172  ! number of values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:), allocatable :: ev
           !dir$ attributes align : 64 :: ev
#else
          type(ZMM16c4), dimension(:), allocatable :: ev
          !dir$ attributes align : 64 :: ev
#endif                                   
    end type Evf2172CV8x16x_t
    
    
  ! The values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
    type, public :: Ehf2173CV8x16x_t
          integer(kind=i4) :: nEhf2172  ! number of values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:), allocatable :: eh
           !dir$ attributes align : 64 :: eh
#else
          type(ZMM16c4), dimension(:), allocatable :: eh
          !dir$ attributes align : 64 :: eh
#endif               
    end type Ehf2173CV8x16x_t
    
    
  ! Internal antenna noise temperature values (2.179)
    type, public :: Tf2179RV8x16x_t
          integer(kind=i4) :: nTf2179   ! number of internal antenna noise temperature values (2.179)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          type(ZMM16r4_t), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif            
    end type Tf2179RV8x16x_t
     
   
  ! External antenna noise temperature values (2.180) 
    type, public :: Tf2180RV8x16x_t
          integer(kind=i4) :: nTf2180   ! number of external to antenna noise temperature values (2.180)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          type(ZMM16r4_t), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif            
    end type Tf2180RV8x16x_t
    
    
  ! The values of noise atmosphere temperature (2.181)
    type, public :: Tf2181RV8x16x_t
          integer(kind=i4) :: nTf2181   ! number of values of noise atmosphere temperature (2.181)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          type(ZMM16r4_t), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif           
    end type Tf2181RV8x16x_t 
    
    
  ! The values of total antenna noise temperature (2.182)
    type, public :: Tf2182RV8x16x_t
          integer(kind=i4) :: nTf2182   ! number of values of total noise antenna temperature (2.182)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          type(ZMM16r4_t), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif           
    end type Tf2182RV8x16x_t
  
    
  ! The values of optical curve length for 'n' curves (2.186)
    type, public :: Qf2186RV8x16x_t
          integer(kind=i4) :: nQf2186   ! number of values of optical curve length (2.186)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8r8_t), dimension(:), allocatable :: q
           !dir$ attributes align : 64 :: q
#else
          type(ZMM16r4_t), dimension(:), allocatable :: q
          !dir$ attributes align : 64 :: q
#endif             
    end type Qf2186RV8x16x_t
   
    
  ! The values of 'x,y,z' (eikonal) coordinate (2.187)
    type, public :: Lf2187CV8x16x_t
          integer(kind=i4) :: nLxf2187  ! number of values of 'x' (eikonal) coordinate (2.187)
          integer(kind=i4) :: nLyf2187  ! number of values of 'y' (eikonal) coordinate (2.187)
          integer(kind=i4) :: nLzf2187  ! number of values of 'z' (eikonal) coordinate (2.187)
#if (ANTENNA_COMMON_ADT_USE_AVX512_PRECISION_REAL8) == 1              
          type(ZMM8c8), dimension(:,:,:), allocatable :: lxyz
           !dir$ attributes align : 64 :: lxyz
#else
          type(ZMM16c4), dimension(:,:,:), allocatable :: lxyz
          !dir$ attributes align : 64 :: lxyz
#endif             
    end type Lf2187CV8x16x_t
    
   
    
   
     
   
     
   
     
    
     
    
    
     
     
end module antenna_common_adt_avx512
