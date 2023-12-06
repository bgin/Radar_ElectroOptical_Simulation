
#include "GMS_config.fpp"


module antenna_common_adt



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         antenna_common_adt
 !          
 !          Purpose:
 !                        
 !                        Various characteristics of different antenna common formulae and computational characteristics.
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 18-11-2023
 !                        Time: 09:53 GMT+2
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
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_ADT_FULLVER =   &
            1000*ANTENNA_COMMON_ADT_MAJOR+100*ANTENNA_COMMON_ADT_MINOR+10*ANTENNA_COMMON_ADT_MICRO
    ! Module creation date
    character(*),        parameter :: ANTENNA_COMMON_ADT_CREATE_DATE = "18-11-2023 09:53 +00200 (SAT 18 NOV 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: ANTENNA_COMMON_ADT_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: ANTENNA_COMMON_ADT_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: ANTENNA_COMMON_ADT_SYNOPSIS    = "Data describing various common antenna characteristics -- derived types based."

    
    ! Array holding a number of points for various antenna characteristics computation.
    integer(kind=i4) :: ne      ! number of  Electric fields (for phased array radiating element)
    integer(kind=i4) :: nepts1  !x-dim
    integer(kind=i4) :: nepts2  !y-dim
    integer(kind=i4) :: nepts3  !z-dim
    
    integer(kind=i4) :: nm      ! number of  Magnetic fields (for phased array radiating element)
    integer(kind=i4) :: nmpts1  !x-dim
    integer(kind=i4) :: nmpts2  !y-dim
    integer(kind=i4) :: nmpts3  !z-dim
    
    integer(kind=i4) :: nesp    ! number of Electric fields spherical-coordinates (for phased array radiating element)
    integer(kind=i4) :: neptsph ! phi-coordinate
    integer(kind=i4) :: neptsth ! theta-coordinate
    
    integer(kind=i4) :: nmsp    ! number of Magnetic fields spherical-coordinates (for phased array radiating element)
    integer(kind=i4) :: nmptsph ! phi-coordinate
    integer(kind=i4) :: nmptsth ! theta-coordinate
    
    integer(kind=i4) :: nje     ! number of surface currents (electric) (for phased array radiating element)
    integer(kind=i4) :: njepts1 !x-dim
    integer(kind=i4) :: njepts2 !y-dim
    integer(kind=i4) :: njepts3 !z-dim 
    
    integer(kind=i4) :: njm     ! number of surface currents (magnetic) (for phased array radiating element)
    integer(kind=i4) :: njmpts1 !x-dim
    integer(kind=i4) :: njmpts2 !y-dim
    integer(kind=i4) :: njmpts3 !z-dim
    
    integer(kind=i4) :: nhe     ! number of Hertz (electric) vectors
    integer(kind=i4) :: nhepts1 !x-dim
    integer(kind=i4) :: nhepts2 !y-dim
    integer(kind=i4) :: nhepts3 !z-dim
    
    integer(kind=i4) :: nhm     ! number of Hertz (magnetic) vectors
    integer(kind=i4) :: nhmpts1 !x-dim
    integer(kind=i4) :: nhmpts2 !y-dim
    integer(kind=i4) :: nhmpts3 !z-dim
    
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
    
    integer(kind=i4) :: npsip   ! number of Psi function's phi values
    integer(kind=i4) :: npsit   ! number of Psi function's theta values
    integer(kind=i4) :: npsi    ! number of Psi functions for phased arrays.
    
    integer(kind=i4) :: nswapp  ! numbers of terms for spherical wave approximation
    integer(kind=i4) :: ncost   ! number of values of cos(theta) argument (formulae: 2.52,2.53,...etc)
    integer(kind=i4) :: nrho    ! distances from the antenna apperture to the coordinate system origin (2.53)
    integer(kind=i4) :: nsint   ! number of sin(theta) values (2.62)
    integer(kind=i4) :: nsinp   ! number of sin(phi) values (2.62)
    integer(kind=i4) :: cosp    ! number of cos(phi) values (2.62)
    integer(kind=i4) :: nx      ! number of 'x' values antenna apperture (2.62)
    integer(kind=i4) :: ny      ! number of 'y' values antenna apperture (2.62)
    integer(kind=i4) :: nL      ! number of 'L' sizes of apperture (2.65)
    integer(kind=i4) :: ngam    ! number of wavelengths (2.65)
    integer(kind=i4) :: nftf265 ! number of values i.e. F(theta) for radiation pattern (2.65)
    integer(kind=i4) :: nftf268 ! number of values i.e. F(theta) for radiation pattern (2.68) 
    integer(kind=i4) :: nu      ! number of 'u' values (2.70)
    integer(kind=i4) :: mf271   ! number of 'm' terms (2.71)
    integer(kind=i4) :: nf271   ! value of 'n' (2.71)
    integer(kind=i4) :: nftf271 ! number of values i.e. F(theta) for radiation pattern (2.71)
    integer(kind=i4) :: nftf269 ! number of values i.e. f(2x/L) for radiation pattern (2.69)
    integer(kind=i4) :: nftf274 ! number of values i.e. f(2x/L) for radiation pattern (2.74)
    integer(kind=i4) :: nftf275 ! number of values i.e. F(theta) for radiation pattern (2.75)
    integer(kind=i4) :: nftf278 ! number of values i.e. f(x) for radiation pattern (2.78)
    integer(kind=i4) :: nNxf283 ! number of Nx function component (2.83)
    integer(kind=i4) :: nrf283  ! number of 'r' coordinate of apperture (2.83)
    integer(kind=i4) :: npf283  ! number of 'phi' coordinate of apperture (2.83)
    integer(kind=i4) :: nuf286  ! number of 'u' values (2.86)
    integer(kind=i4) :: nfuf285 ! number of values i.e. F(u)vfor radiation pattern (2.85)
    integer(kind=i4) :: nauf286 ! number of values of function (2.86)
    integer(kind=i4) :: nrhf286 ! number of values 'rho' argument (2.86)
    integer(kind=i4) :: nrf287  ! number of values rho coordinate f(rho,phi) (2.87)
    integer(kind=i4) :: npf287  ! number of values phi coordinate f(rho,phi) (2.87)
    integer(kind=i4) :: nftf291 ! number of values i.e. F(theta) for radiation pattern (2.91)
    integer(kind=i4) :: nmf291  ! number of m-1 values (2.91)
    integer(kind=i4) :: nfrf293 ! number of values i.e. F(rho) for radiation pattern (2.93)
    integer(kind=i4) :: nmf293  ! number of terms (2.93)
    integer(kind=i4) :: nrf293  ! number of 'r' coordinate values (2.93)
    integer(kind=i4) :: nfxf294 ! number of values i.e. f(x,y) for amplitude field of aperture (2.94) 
    
    integer(kind=i4) :: nft241  ! number of values of elementary electric frame (2.41)
    integer(kind=i4) :: nff241  ! number of elementary frames (2.41)
    
    
    
   
    
    integer(kind=i4) :: nftf2105 ! number of values for radiation pattern quadratic phase error
                                 ! and cosinusoidal amplitude distribution (2.105)
    
    integer(kind=i4) :: ntf2107  ! number of approximation terms (2.107)
    integer(kind=i4) :: ntf2110  ! number of theta values 2D array (2.110)
    integer(kind=i4) :: npf2110  ! number of phi values 2D array (2.110)
    integer(kind=i4) :: ntf2110a ! number of theta values 1D array (2.110a)
    integer(kind=i4) :: ntf2111  ! number of theta values 2D array (2.111)
    integer(kind=i4) :: npf2111  ! number of phi values 2D array (2.111) 
    integer(kind=i4) :: ntf211a  ! number of theta values 1D array (2.111a)
    integer(kind=i4) :: nuf2125  ! number of values sinusoidal phase error (2.125)
    integer(kind=i4) :: nuf2126  ! number of values cosinusoidal phase error (2.126)
    integer(kind=i4) :: npf2127  ! number of phi values of coefficient of directional pattern (2.127)
    integer(kind=i4) :: ntf2127  ! number of theta values of coefficient of directional pattern (2.127)
    integer(kind=i4) :: naf2127  ! number of radiating elements or discrete antennas (2.127)
    integer(kind=i4) :: nrmnf2143 ! number of real parts of m-th and n-th antenna impedance (2.143)
    integer(kind=i4) :: nxmnf2144 ! number of imaginary parts of m-th and n-th antenna impedance (2.144)
    integer(kind=i4) :: nrf2145   ! number of values of mutual impedance of two antennas as a function of their distance (2.145)
    integer(kind=i4) :: nxmnf2148 ! number of real parts of m-th and n-th antenna impedance (2.148)
    integer(kind=i4) :: nft2149   ! number of theta values for complex radiating pattern (2.149)
    integer(kind=i4) :: nrf2150   ! number of values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    integer(kind=i4) :: nxf2150   !number of values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    integer(kind=i4) :: nh2153    ! number of 'height' values of an antenna (EM-meaning) a general formula (2.153)
    integer(kind=i4) :: nh2154    ! number of 'height' values of antenna (EM-meaning) symmetric vibrator (2.154)
    integer(kind=i4) :: naf2159   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
    integer(kind=i4) :: naf2160   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
    integer(kind=i4) :: naf2161   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric aperture (2.161)
    integer(kind=i4) :: naf2162   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial orientation of E-field tangent to apperture (2.162)
    integer(kind=i4) :: nRvf2169  ! number of complex Fresnel coefficients, vertical polarization (2.169)
    integer(kind=i4) :: nRhf2170  ! number of complex Fresnel coefficients, horizontal polarization (2.170)
    integer(kind=i4) :: nEvf2172  ! number of values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
    integer(kind=i4) :: nEhf2172  ! number of values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
    integer(kind=i4) :: nTf2179   ! number of internal antenna noise temperature values (2.179)
    integer(kind=i4) :: nTf2180   ! number of external to antenna noise temperature values (2.180)
    integer(kind=i4) :: nTf2181   ! number of values of noise atmosphere temperature (2.181)
    integer(kind=i4) :: nTf2182   ! number of values of total noise antenna temperature (2.182)
    integer(kind=i4) :: nQf2186   ! number of values of optical curve length (2.186)
    integer(kind=i4) :: nLxf2187  ! number of values of 'x' (eikonal) coordinate (2.187)
    integer(kind=i4) :: nLyf2187  ! number of values of 'y' (eikonal) coordinate (2.187)
    integer(kind=i4) :: nLzf2187  ! number of values of 'z' (eikonal) coordinate (2.187)
   
! Default setting is 0. 
#if !defined(ANTENNA_COMMON_ADT_USE_PRECISION_REAL8)
#define ANTENNA_COMMON_ADT_USE_PRECISION_REAL8 0
#endif    
    
    ! Dynamic derived type for EMF modeling (complex-single) AoS-layout
    type, public :: DC3D_aos_cmplx_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
        complex(kind=dp), dimension(:,:), allocatable :: fx
        complex(kind=dp), dimension(:,:), allocatable :: fy
        complex(kind=dp), dimension(:,:), allocatable :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#else       
        complex(kind=sp), dimension(:,:), allocatable :: fx
        complex(kind=sp), dimension(:,:), allocatable :: fy
        complex(kind=sp), dimension(:,:), allocatable :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#endif
    end type DC3D_aos_cmplx_t
    
    ! Dynamic derived type for EMF modeling (complex-single) flat-layout
    type, public :: DC3D_flat_cmplx_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
        complex(kind=dp), dimension(:), allocatable :: fx ! length is: ne*nx
        complex(kind=dp), dimension(:), allocatable :: fy ! length is: ne*ny
        complex(kind=dp), dimension(:), allocatable :: fz ! length is: ne*nz
#else        
        complex(kind=sp), dimension(:), allocatable :: fx ! length is: ne*nx
        complex(kind=sp), dimension(:), allocatable :: fy ! length is: ne*ny
        complex(kind=sp), dimension(:), allocatable :: fz ! length is: ne*nz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#endif
    end type DC3D_flat_cmplx_t
   
    
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) AoS-layout
    type, public :: DC3D_aos_real_t
    ! First dimension nth field, second dimension number of sample points
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
        real(kind=dp), dimension(:,:), allocatable :: fxr
        real(kind=dp), dimension(:,:), allocatable :: fxi
        real(kind=dp), dimension(:,:), allocatable :: fyr
        real(kind=dp), dimension(:,:), allocatable :: fyi
        real(kind=dp), dimension(:,:), allocatable :: fzr
        real(kind=dp), dimension(:,:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        real(kind=sp), dimension(:,:), allocatable :: fxr
        real(kind=sp), dimension(:,:), allocatable :: fxi
        real(kind=sp), dimension(:,:), allocatable :: fyr
        real(kind=sp), dimension(:,:), allocatable :: fyi
        real(kind=sp), dimension(:,:), allocatable :: fzr
        real(kind=sp), dimension(:,:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type DC3D_aos_real_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) flat-layout
    type, public :: DC3D_flat_real_t
    ! First dimension nth field, second dimension number of sample points
        integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
        integer(kind=i4) :: nx  !x-dim
        integer(kind=i4) :: ny  !y-dim
        integer(kind=i4) :: nz  !z-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
        real(kind=dp), dimension(:), allocatable :: fxr
        real(kind=dp), dimension(:), allocatable :: fxi
        real(kind=dp), dimension(:), allocatable :: fyr
        real(kind=dp), dimension(:), allocatable :: fyi
        real(kind=dp), dimension(:), allocatable :: fzr
        real(kind=dp), dimension(:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        real(kind=sp), dimension(:), allocatable :: fxr
        real(kind=sp), dimension(:), allocatable :: fxi
        real(kind=sp), dimension(:), allocatable :: fyr
        real(kind=sp), dimension(:), allocatable :: fyi
        real(kind=sp), dimension(:), allocatable :: fzr
        real(kind=sp), dimension(:), allocatable :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type DC3D_flat_real_t
    
         
      
    ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) AoS-layout
    type, public :: DC2D_aos_cmplx_t
    ! First dimension nth field, second dimension number of sample points 
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
       complex(kind=dp), dimension(:,:), allocatable :: fth
       complex(kind=dp), dimension(:,:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else       
       complex(kind=sp), dimension(:,:), allocatable :: fth
       complex(kind=sp), dimension(:,:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type DC2D_aos_cmplx_t
    
    
     ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) flat-layout
    type, public :: DC2D_flat_cmplx_t
    ! First dimension nth field, second dimension number of sample points 
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
       complex(kind=dp), dimension(:), allocatable :: fth
       complex(kind=dp), dimension(:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else
       complex(kind=sp), dimension(:), allocatable :: fth
       complex(kind=sp), dimension(:), allocatable :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type DC2D_flat_cmplx_t
    
    
  
  
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) AoS-layout
    type, public :: DC2D_aos_real_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
       real(kind=dp), dimension(:,:), allocatable :: fthr
       real(kind=dp), dimension(:,:), allocatable :: fthi
       real(kind=dp), dimension(:,:), allocatable :: fphr
       real(kind=dp), dimension(:,:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#else       
       real(kind=sp), dimension(:,:), allocatable :: fthr
       real(kind=sp), dimension(:,:), allocatable :: fthi
       real(kind=sp), dimension(:,:), allocatable :: fphr
       real(kind=sp), dimension(:,:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type DC2D_aos_real_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) flat-layout
    type, public :: DC2D_flat_real_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1
       real(kind=dp), dimension(:), allocatable :: fthr
       real(kind=dp), dimension(:), allocatable :: fthi
       real(kind=dp), dimension(:), allocatable :: fphr
       real(kind=dp), dimension(:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi 
#else       
       real(kind=sp), dimension(:), allocatable :: fthr
       real(kind=sp), dimension(:), allocatable :: fthi
       real(kind=sp), dimension(:), allocatable :: fphr
       real(kind=sp), dimension(:), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type DC2D_flat_real_t
    
    
       
    
    
    ! Normalized function Psi (power of radiated field)
    type, public :: Pf239_t
       integer(kind=i4) :: nth  !theta-dim
       integer(kind=i4) :: nph  !phi-dim
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1      
       real(kind=dp), dimension(:,:), allocatable :: P
       !dir$ attributes align : 64 :: P
#else      
       real(kind=dp), dimension(:,:), allocatable :: P
       !dir$ attributes align : 64 :: P
#endif
    end type Pf239_t
  
    
    ! Elementary electric dipoles (radiation patterns)
    type, public :: RPf240_t
       integer(kind=i4) :: nft240  ! number of values of elementary electric dipole (2.40)
       integer(kind=i4) :: ndf240  ! number of elementary dipoles (2.40)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1          
       real(kind=dp), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf240_t
    
    ! Sinusoidal current distribution (2.43)
    type, public :: Izf243_t
       integer(kind=i4) :: nzf243  ! number of current distribution 'z' values (2.43)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1         
       complex(kind=dp), dimension(:), allocatable :: Iz
       !dir$ attributes align : 64 :: Iz
#else
       complex(kind=sp), dimension(:), allocatable :: Iz
       !dir$ attributes align : 64 :: f243r8
#endif
    end type Izf243_t
    
    ! Radiation pattern of similiar EM radiators (2.96)
    type, public :: RPf296_t
       integer(kind=i4) :: ntf296  ! number of theta values (2.96)
       integer(kind=i4) :: npf296  ! number of phi values (2.96)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1           
       real(kind=dp), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:,:), allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf296_t
    
    !Linear phase error values apperture edge (2.98)
    type, public :: PEf298_t
       integer(kind=i4) :: npf298  ! number of linear phase error values apperture edge (2.98)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1          
       real(kind=dp), dimension(:),   allocatable :: f298r4
       !dir$ attributes align : 64 :: pe
#else
       real(kind=sp), dimension(:),   allocatable :: f298r8
       !dir$ attributes align : 64 :: pe
#endif
    end type PEf298_t
    
    !Radiation pattern including linear phase error term (2.100)
    type, public :: RPf2100_t
       integer(kind=i4) :: nftf2100 ! number of values for radiation pattern linear phase error (2.100)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1    
       real(kind=dp), dimension(:),   allocatable :: rp
#else
       !dir$ attributes align : 64 :: rp
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    type, public :: RPf2100_t

    !Radiation pattern including quadratic phase error term (2.102)
    type, public :: RPf2102_t
       integer(kind=i4) :: nftf2102 ! number of values for radiation pattern quadratic phase error (2.102)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1   
       real(kind=dp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf2102_t

    ! Radiation pattern cubic phase error and cosinusoidal amplitude distribution (2.107)
    type, public :: RPf2107_t
       integer(kind=i4) :: nfuf2107 ! number of values for radiation pattern cubic phase error
                                 ! and cosinusoidal amplitude distribution (2.107)
#if (ANTENNA_COMMON_ADT_USE_PRECISION_REAL8) == 1                                    
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=dp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif
    end type RPf2107_t
    
    ! Average of radiation pattern of 2D (single) array (2.110)
    real(kind=sp), dimension(:,:), allocatable :: f2110r4
    !dir$ attributes align : 64 :: f2110r4
    real(kind=dp), dimension(:,:), allocatable :: f2110r8
    !dir$ attributes align : 64 :: f2110r8
    
    
    
    ! Average of radiation pattern of 1D (single) array (2.110a)
    real(kind=sp), dimension(:), allocatable :: f2110ar4
    !dir$ attributes align : 64 :: f2110ar4
    real(kind=dp), dimension(:), allocatable :: f2110ar8
    !dir$ attributes align : 64 :: f2110ar8
    
    ! Power-averaged of radiation pattern of 2D (single) array (2.111)
    real(kind=sp), dimension(:,:), allocatable :: f2111r4
    !dir$ attributes align : 64 :: f2111r4
    real(kind=dp), dimension(:,:), allocatable :: f2111r8
    !dir$ attributes align : 64 :: f2111r8
    
    ! Power-average of radiation pattern of 1D (single) array (2.111a)
    real(kind=sp), dimension(:), allocatable :: f2111ar4
    !dir$ attributes align : 64 :: f2111ar4
    real(kind=dp), dimension(:), allocatable :: f2111ar8
    !dir$ attributes align : 64 :: f2111ar8                          

    !Phi values of coefficient of directional pattern (2.127)
    real(kind=sp), dimension(:,:,:), allocatable :: f2127r4
    !dir$ attributes align : 64 :: f2127r4
    real(kind=dp), dimension(:,:,:), allocatable :: f2127r8
    !dir$ attributes align : 64 :: f2127r8
    
    ! Values of real parts of m-th and n-th antenna impedance (2.143)
    real(kind=sp), dimension(:), allocatable :: rmnf2143r4
     !dir$ attributes align : 64 :: rmnf2143r4
    real(kind=dp), dimension(:), allocatable :: rmnf2143r8
     !dir$ attributes align : 64 :: rmnf2143r8
     
    ! Values of imaginary parts of m-th and n-th antenna impedance (2.144)
    real(kind=sp), dimension(:), allocatable :: rmnf2144r4
     !dir$ attributes align : 64 :: rmnf2144
    real(kind=dp), dimension(:), allocatable :: rmnf2144r8
     !dir$ attributes align : 64 :: rmnf2144r8

    ! Values of mutual impedance of two antennas as a function of their distance (2.145)
    real(kind=sp), dimension(:), allocatable :: rf2145r4
    !dir$ attributes align : 64 :: rf2145r4
    real(kind=dp), dimension(:), allocatable :: rf2145r8
    !dir$ attributes align : 64 :: rf2145r8
    
    ! Values of real parts of m-th and n-th antenna impedance (2.143)
    real(kind=sp), dimension(:), allocatable :: xmnf2148r4
     !dir$ attributes align : 64 :: xmnf2148r4
    real(kind=dp), dimension(:), allocatable :: xmnf2148r8
     !dir$ attributes align : 64 :: xmnf2148r8

    ! Theta values for complex radiating pattern (2.149)
    complex(kind=sp), dimension(:), allocatable :: f2149r4
     !dir$ attributes align : 64 :: f2149r4
    complex(kind=dp), dimension(:), allocatable :: f2149r8
     !dir$ attributes align : 64 :: f2149r8
     
    ! Values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    real(kind=sp), dimension(:), allocatable :: r12f2150r4
     !dir$ attributes align : 64 :: r12f2150r4
    real(kind=dp), dimension(:), allocatable :: r12f2150r8
     !dir$ attributes align : 64 :: r12f2150r8 
     
    ! Values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    real(kind=sp), dimension(:), allocatable :: x12f2150r4
     !dir$ attributes align : 64 :: x12f2150r4
    real(kind=dp), dimension(:), allocatable :: x12f2150r8
     !dir$ attributes align : 64 :: x12f2150r8 
     
    ! The values 'height' of an antenna (EM-meaning) (2.153)
    real(kind=sp), dimension(:), allocatable :: hgf2153r4
     !dir$ attributes align : 64 :: hgf2153r4
    real(kind=dp), dimension(:), allocatable :: hgf2153r8
     !dir$ attributes align : 64 :: hgf2153r8
     
     ! The values 'height' of an antenna (EM-meaning) symmetric vibrator (2.154)
    real(kind=sp), dimension(:), allocatable :: hsf2154r4
     !dir$ attributes align : 64 :: hsf2154r4
    real(kind=dp), dimension(:), allocatable :: hsf2154r8
     !dir$ attributes align : 64 :: hsf2154r8
     
    ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
    real(kind=sp), dimension(:), allocatable :: af2159r4
     !dir$ attributes align : 64 :: af2159r4
    real(kind=dp), dimension(:), allocatable :: af2159r8
     !dir$ attributes align : 64 :: af2159r8
     
    ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
    real(kind=sp), dimension(:), allocatable :: af2160r4
     !dir$ attributes align : 64 :: af2160r4
    real(kind=dp), dimension(:), allocatable :: af2160r8
     !dir$ attributes align : 64 :: af2160r8
     
     ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric apperture (2.161)
    real(kind=sp), dimension(:), allocatable :: af2161r4
     !dir$ attributes align : 64 :: af2161r4
    real(kind=dp), dimension(:), allocatable :: af2161r8
     !dir$ attributes align : 64 :: af2161r8
     
    ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial to Electric field tangent to apperture (2.162)
    real(kind=sp), dimension(:), allocatable :: af2162r4
     !dir$ attributes align : 64 :: af2162r4
    real(kind=dp), dimension(:), allocatable :: af2162r8
     !dir$ attributes align : 64 :: af2162r8
     
    !The values of complex Fresnel coefficients, vertical polarization (2.169)
    complex(kind=sp), dimension(:), allocatable :: Rvf2169r4
    !dir$ attributes align : 64 :: Rvf2169r4
    complex(kind=dp), dimension(:), allocatable :: Rvf2169r8
    !dir$ attributes align : 64 :: Rvf2169r8
    
    !The values of complex Fresnel coefficients, horizontal polarization (2.170)
    complex(kind=sp), dimension(:), allocatable :: Rvf2170r4
    !dir$ attributes align : 64 :: Rvf2170r4
    complex(kind=dp), dimension(:), allocatable :: Rvf2170r8
    !dir$ attributes align : 64 :: Rvf2170r8
    
    ! The values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
    complex(kind=sp), dimension(:), allocatable :: Evf2172r4
    !dir$ attributes align : 64 :: Evf2172r4
    complex(kind=dp), dimension(:), allocatable :: Evf2172r8
    !dir$ attributes align : 64 :: Evf2172r8
    
    ! The values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
    complex(kind=sp), dimension(:), allocatable :: Ehf2173r4
    !dir$ attributes align : 64 :: Ehf2173r4
    complex(kind=dp), dimension(:), allocatable :: Ehf2173r8
    !dir$ attributes align : 64 :: Ehf2173r8
    
    !Internal antenna noise temperature values (2.179)
    real(kind=sp), dimension(:), allocatable :: Tf2179r4
     !dir$ attributes align : 64 :: Tf2179r4
    real(kind=dp), dimension(:), allocatable :: Tf2179r8
     !dir$ attributes align : 64 :: Tf2179r8
    
    !External antenna noise temperature values (2.180)
    real(kind=sp), dimension(:), allocatable :: Tf2180r4
     !dir$ attributes align : 64 :: Tf2180r4
    real(kind=dp), dimension(:), allocatable :: Tf2180r8
     !dir$ attributes align : 64 :: Tf2180r8
     
    !The values of noise atmosphere temperature (2.181)
    real(kind=sp), dimension(:), allocatable :: Tf2181r4
     !dir$ attributes align : 64 :: Tf2181r4
    real(kind=dp), dimension(:), allocatable :: Tf2181r8
     !dir$ attributes align : 64 :: Tf2181r8
     
    !The values of total antenna noise temperature (2.182)
    real(kind=sp), dimension(:), allocatable :: Tf2182r4
     !dir$ attributes align : 64 :: Tf2182r4
    real(kind=dp), dimension(:), allocatable :: Tf2182r8
     !dir$ attributes align : 64 :: Tf2182r8
     
    !The values of optical curve length for 'n' curves (2.186)
    real(kind=sp), dimension(:), allocatable :: Qf2186r4
     !dir$ attributes align : 64 :: Qf2186r4
    real(kind=dp), dimension(:), allocatable :: Qf2186r8
     !dir$ attributes align : 64 :: Qf2186r8
     
    !The values of 'x,y,z' (eikonal) coordinate (2.187)
    real(kind=sp), dimension(:,:,:), allocatable :: Lf2187r4
     !dir$ attributes align : 64 :: Lf2187r4
    real(kind=dp), dimension(:,:,:), allocatable :: Lf2187r8
    !dir$ attributes align : 64 :: Lf2187r8
    
     
     
end module antenna_common_adt
