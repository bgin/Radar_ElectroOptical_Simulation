
#include "GMS_config.fpp"


module antenna_common_sadt



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         antenna_common_sadt
 !          
 !          Purpose:
 !                        
 !                        Various characteristics of different antenna common formulae and computational characteristics.
 !                        Static memory allocation!!
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 14-12-2023
 !                        Time: 08:51 GMT+2
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
    integer(kind=i4),  parameter :: ANTENNA_COMMON_SADT_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_SADT_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_SADT_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: ANTENNA_COMMON_SADT_FULLVER =   &
            1000*ANTENNA_COMMON_SADT_MAJOR+100*ANTENNA_COMMON_SADT_MINOR+10*ANTENNA_COMMON_SADT_MICRO
    ! Module creation date
    character(*),        parameter :: ANTENNA_COMMON_ASDT_CREATE_DATE = "14-12-2023 08:53 +00200 (THR 12 DEC 2023 GMT+2)"
    ! Module build date
    character(*),        parameter :: ANTENNA_COMMON_SADT_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: ANTENNA_COMMON_SADT_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: ANTENNA_COMMON_SADT_SYNOPSIS    = "Data describing various common antenna characteristics -- derived types based statically allocated."

  ! Default setting is 0. 
#if !defined(ANTENNA_COMMON_SADT_USE_PRECISION_REAL8)
#define ANTENNA_COMMON_SADT_USE_PRECISION_REAL8 0
#endif  

    
 
        ! Elementary electric dipoles (radiation patterns)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1   
       integer(kind=i4) :: nft240  ! number of values of elementary electric dipole (2.40)
       integer(kind=i4) :: ndf240  ! number of elementary dipoles (2.40)

       ! Dynamic derived type for EMF modeling (real-single) (decomposed) AoS-layout
       ! DC3D_aos_real_t
       ! First dimension nth field, second dimension number of sample points      
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
    
#include "GMS_antenna_common_sadt.inc"    
    
   
     ! Dynamic derived type for EMF modeling (complex-single) AoS-layout
     type, public :: SC3D_aos_cmplx_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
     
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        complex(kind=dp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fx
        complex(kind=dp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fy
        complex(kind=dp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#else       
        complex(kind=sp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fx
        complex(kind=sp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fy
        complex(kind=sp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz 
#endif
    end type SC3D_aos_cmplx_t
    
    ! Dynamic derived type for EMF modeling (complex-single) flat-layout
    type, public :: SC3D_flat_cmplx_t
    ! First dimension nth field, second dimension number of sample points
         ! Array holding a number of points for various antenna characteristics computation.
      
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        complex(kind=dp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fx ! length is: ne*nx
        complex(kind=dp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fy ! length is: ne*ny
        complex(kind=dp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fz ! length is: ne*nz
#else        
        complex(kind=sp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fx ! length is: ne*nx
        complex(kind=sp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fy ! length is: ne*ny
        complex(kind=sp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fz ! length is: ne*nz
        !dir$ attributes align : 64 :: fx
        !dir$ attributes align : 64 :: fy
        !dir$ attributes align : 64 :: fz
#endif
    end type SC3D_flat_cmplx_t
   
    
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) AoS-layout
    type, public :: SC3D_aos_real_t
    ! First dimension nth field, second dimension number of sample points
       
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        real(kind=dp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fxr
        real(kind=dp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fxi
        real(kind=dp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fyr
        real(kind=dp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fyi
        real(kind=dp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fzr
        real(kind=dp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        real(kind=sp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fxr
        real(kind=sp), dimension(SC3D_caos_nx,SC3D_caos_ne) :: fxi
        real(kind=sp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fyr
        real(kind=sp), dimension(SC3D_caos_ny,SC3D_caos_ne) :: fyi
        real(kind=sp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fzr
        real(kind=sp), dimension(SC3D_caos_nz,SC3D_caos_ne) :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type SC3D_aos_real_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) flat-layout
    type, public :: SC3D_flat_real_t
    ! First dimension nth field, second dimension number of sample points
        
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
        real(kind=dp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fxr
        real(kind=dp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fxi
        real(kind=dp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fyr
        real(kind=dp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fyi
        real(kind=dp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fzr
        real(kind=dp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#else        
        real(kind=sp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fxr
        real(kind=sp), dimension(SC3D_cflat_nx*SC3D_cflat_ne) :: fxi
        real(kind=sp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fyr
        real(kind=sp), dimension(SC3D_cflat_ny*SC3D_cflat_ne) :: fyi
        real(kind=sp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fzr
        real(kind=sp), dimension(SC3D_cflat_nz*SC3D_cflat_ne) :: fzi
        !dir$ attributes align : 64 :: fxr
        !dir$ attributes align : 64 :: fxi
        !dir$ attributes align : 64 :: fyr
        !dir$ attributes align : 64 :: fyi
        !dir$ attributes align : 64 :: fzr
        !dir$ attributes align : 64 :: fzi
#endif
    end type SC3D_flat_real_t
    
         
      
    ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) AoS-layout
    type, public :: SC2D_aos_cmplx_t
    ! First dimension nth field, second dimension number of sample points 
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
       complex(kind=dp), dimension(SC2D_caos_nth,SC2D_caos_ne) :: fth
       complex(kind=dp), dimension(SC2D_caos_nph,SC2D_caos_ne) :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else       
       complex(kind=sp), dimension(SC2D_caos_nth,SC2D_caos_ne) :: fth
       complex(kind=sp), dimension(SC2D_caos_nph,SC2D_caos_ne) :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type SC2D_aos_cmplx_t
    
    
     ! Dynamic derived type for EMF modeling (complex-single) (theta,phi coordinates) flat-layout
    type, public :: SC2D_flat_cmplx_t
    ! First dimension nth field, second dimension number of sample points 
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
       complex(kind=dp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fth
       complex(kind=dp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#else
       complex(kind=sp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fth
       complex(kind=sp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fph
       !dir$ attributes align : 64 :: fth
       !dir$ attributes align : 64 :: fph
#endif
    end type SC2D_flat_cmplx_t
    
    
  
  
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) AoS-layout
    type, public :: SC2D_aos_real_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
       real(kind=dp), dimension(SC2D_caos_nth,SC2D_caos_ne) :: fthr
       real(kind=dp), dimension(SC2D_caos_nth,SC2D_caos_ne) :: fthi
       real(kind=dp), dimension(SC2D_caos_nph,SC2D_caos_ne) :: fphr
       real(kind=dp), dimension(SC2D_caos_nph,SC2D_caos_ne) :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#else       
       real(kind=sp), dimension(SC2D_caos_nth,SC2D_caos_ne), allocatable :: fthr
       real(kind=sp), dimension(SC2D_caos_nth,SC2D_caos_ne), allocatable :: fthi
       real(kind=sp), dimension(SC2D_caos_nph,SC2D_caos_ne), allocatable :: fphr
       real(kind=sp), dimension(SC2D_caos_nph,SC2D_caos_ne), allocatable :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type SC2D_aos_real_t
    
    
    ! Dynamic derived type for EMF modeling (real-single) (decomposed) (theta,phi coordinates) flat-layout
    type, public :: SC2D_flat_real_t
    ! First dimension nth field, second dimension number of sample points
       integer(kind=i4) :: ne      ! number of  EM fields (for phased array radiating element)
       integer(kind=i4) :: nth  !th-dim
       integer(kind=i4) :: nph  !ph-dim
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1
       real(kind=dp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fthr
       real(kind=dp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fthi
       real(kind=dp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fphr
       real(kind=dp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi 
#else       
       real(kind=sp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fthr
       real(kind=sp), dimension(SC2D_cflat_nth*SC2D_cflat_ne) :: fthi
       real(kind=sp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fphr
       real(kind=sp), dimension(SC2D_cflat_nph*SC2D_cflat_ne) :: fphi
       !dir$ attributes align : 64 :: fthr
       !dir$ attributes align : 64 :: fthi
       !dir$ attributes align : 64 :: fphr
       !dir$ attributes align : 64 :: fphi
#endif
    end type SC2D_flat_real_t
    
    
       
    
    
    ! Normalized function Psi (power of radiated field)
    type, public :: Pf239_t
       
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1      
       real(kind=dp), dimension(nph,nth) :: P
       !dir$ attributes align : 64 :: P
#else      
       real(kind=dp), dimension(nph,nth) :: P
       !dir$ attributes align : 64 :: P
#endif
    end type Pf239_t
  
    
    ! Elementary electric dipoles (radiation patterns)
    type, public :: RPf240_t
     
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1          
       real(kind=dp), dimension(nft240,ndf240) :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(nft240,ndf240) :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf240_t
    
    ! Sinusoidal current distribution (2.43)
    type, public :: Izf243_t
      
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1         
       complex(kind=dp), dimension(nzf243) :: Iz
       !dir$ attributes align : 64 :: Iz
#else
       complex(kind=sp), dimension(nzf243) :: Iz
       !dir$ attributes align : 64 :: f243r8
#endif
    end type Izf243_t
    
    ! Radiation pattern of similiar EM radiators (2.96)
    type, public :: RPf296_t
      
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1           
       real(kind=dp), dimension(npf296,ntf296) :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(npf296,ntf296) :: rp
       !dir$ attributes align : 64 :: rp
#endif
    end type RPf296_t
    
    !Linear phase error values apperture edge (2.98)
    type, public :: PEf298_t
      
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1          
       real(kind=dp), dimension(npf298) :: pe
       !dir$ attributes align : 64 :: pe
#else
       real(kind=sp), dimension(npf298) :: pe
       !dir$ attributes align : 64 :: pe
#endif
    end type PEf298_t
    
    !Radiation pattern including linear phase error term (2.100)
    type, public :: RPf2100_t
       integer(kind=i4) :: nftf2100 ! number of values for radiation pattern linear phase error (2.100)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1    
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
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1   
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
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1                                    
       real(kind=dp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif
    end type RPf2107_t
    
    
   ! Average of radiation pattern of 2D (single) array (2.110)
    type, public :: RPf2100_t
          integer(kind=i4) :: ntf2110  ! number of theta values 2D array (2.110)
          integer(kind=i4) :: npf2110  ! number of phi values 2D array (2.110)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          real(kind=sp), dimension(:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif
    end type RPf2100_t
    
  ! Average of radiation pattern of 1D (single) array (2.110a)
    type, public :: RPf2100a_t
          integer(kind=i4) :: ntf2110a ! number of theta values 1D array (2.110a)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1                                    
       real(kind=dp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif          
    end type RPf2100a_t
    
    
  ! Power-averaged of radiation pattern of 2D (single) array (2.111)
    type, public :: RPf2111_t
          integer(kind=i4) :: ntf2111  ! number of theta values 2D array (2.111)
          integer(kind=i4) :: npf2111  ! number of phi values 2D array (2.111) 
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          real(kind=sp), dimension(:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif          
    end type RPf2111_t
    
   
  ! Power-average of radiation pattern of 1D (single) array (2.111a)
    type, public :: RPf2111a_t
          integer(kind=i4) :: ntf211a  ! number of theta values 1D array (2.111a)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1                                    
       real(kind=dp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp
#else
       real(kind=sp), dimension(:),   allocatable :: rp
       !dir$ attributes align : 64 :: rp  
#endif            
    end type RPf2111a_t 
    
   
  !  Phi values of coefficient of directional pattern (2.127)
    type, public :: DPf2127_t
          integer(kind=i4) :: npf2127  ! number of phi values of coefficient of directional pattern (2.127)
          integer(kind=i4) :: ntf2127  ! number of theta values of coefficient of directional pattern (2.127)
          integer(kind=i4) :: naf2127  ! number of radiating elements or discrete antennas (2.127)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:,:,:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          real(kind=sp), dimension(:,:,:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif                  
    end type DPf2127_t
   
  ! Values of real parts of m-th and n-th antenna impedance (2.143)
    type, public :: RMNf2143_t
          integer(kind=i4) :: nrmnf2143 ! number of real parts of m-th and n-th antenna impedance (2.143)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: rmn
           !dir$ attributes align : 64 :: rmn
#else
          real(kind=sp), dimension(:), allocatable :: rmn
          !dir$ attributes align : 64 :: rmn
#endif  
    end type RMNf2143_t
    
    
  ! Values of imaginary parts of m-th and n-th antenna impedance (2.144)
    type, public :: RMNf2144_t
          integer(kind=i4) :: nxmnf2144 ! number of imaginary parts of m-th and n-th antenna impedance (2.144)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: rmn
           !dir$ attributes align : 64 :: rmn
#else
          real(kind=sp), dimension(:), allocatable :: rmn
          !dir$ attributes align : 64 :: rmn
#endif            
    end type RMNf2144_t
    
    
  ! Values of mutual impedance of two antennas as a function of their distance (2.145)
    type, public :: R12f2145_t
          integer(kind=i4) :: nrf2145   ! number of values of mutual impedance of two antennas as a function of their distance (2.145)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: r12
           !dir$ attributes align : 64 :: r12
#else
          real(kind=sp), dimension(:), allocatable :: r12
          !dir$ attributes align : 64 :: r12
#endif           
    end type R12f2145_t                     

    
  ! Values of real parts of m-th and n-th antenna impedance (2.148)
    type, public :: XMNf2148_t
          integer(kind=i4) :: nxmnf2148 ! number of real parts of m-th and n-th antenna impedance (2.148)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: xmn
           !dir$ attributes align : 64 :: xmn
#else
          real(kind=sp), dimension(:), allocatable :: xmn
          !dir$ attributes align : 64 :: xmn
#endif            
    end type XMNf2148_t
    
    
  ! Theta values for complex radiating pattern (2.149)
    type, public :: RPf2149_t
          integer(kind=i4) :: nft2149   ! number of theta values for complex radiating pattern (2.149)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:), allocatable :: rp
           !dir$ attributes align : 64 :: rp
#else
          complex(kind=sp), dimension(:), allocatable :: rp
          !dir$ attributes align : 64 :: rp
#endif          
    end type RPf2149_t
    
    
  ! Values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    type, public :: R12f2150_t
          integer(kind=i4) :: nrf2150   ! number of values of mutual impedance (real part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: r12
           !dir$ attributes align : 64 :: r12
#else
          real(kind=sp), dimension(:), allocatable :: r12
          !dir$ attributes align : 64 :: r12
#endif                                   
    end type R12f2150_t 
    
  
  ! Values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
    type, public :: X12f2150_t
          integer(kind=i4) :: nxf2150   !number of values of mutual impedance (imaginary part) of two antennas as an 
                                  ! function of complex radiation pattern (2.150)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: x12
           !dir$ attributes align : 64 :: x12
#else
          real(kind=sp), dimension(:), allocatable :: x12
          !dir$ attributes align : 64 :: x12
#endif             
    end type X12f2150_t
    
    
  ! The values 'height' of an antenna (EM-meaning) (2.153)
    type, public :: HGf2153_t
          integer(kind=i4) :: nh2153    ! number of 'height' values of antenna (EM-meaning)  (2.154)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: hg
           !dir$ attributes align : 64 :: hg
#else
          real(kind=sp), dimension(:), allocatable :: hg
          !dir$ attributes align : 64 :: hg
#endif                 
    end type HGf2153_t
     
   
  ! The values 'height' of an antenna (EM-meaning) symmetric vibrator (2.154)
    type, public :: HSf2154_t
          integer(kind=i4) :: nh2154    ! number of 'height' values of antenna (EM-meaning) symmetric vibrator (2.154)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: hs
           !dir$ attributes align : 64 :: hs
#else
          real(kind=sp), dimension(:), allocatable :: hs
          !dir$ attributes align : 64 :: hs
#endif              
    end type HSf2154_t 
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
    type, public :: Af2159_t
          integer(kind=i4) :: naf2159   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) general case (2.159)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          real(kind=sp), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                   
    end type Af2159_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
    type, public :: Af2160_t
          integer(kind=i4) :: naf2160   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a very narrow beam (2.160)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          real(kind=sp), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                  
    end type Af2160_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric apperture (2.161)
    type, public :: Af2161_t
          integer(kind=i4) :: naf2161   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) a sine-symmetric aperture (2.161)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          real(kind=sp), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif                                    
    end type Af2161_t
    
    
  ! The  area values as (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial to Electric field tangent to apperture (2.162)
    type, public :: Af2162_t
          integer(kind=i4) :: naf2162   ! number of area values (function of an area) of an 
                                  ! antenna (EM-meaning) coaxial orientation of E-field tangent to apperture (2.162)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: a
           !dir$ attributes align : 64 :: a
#else
          real(kind=sp), dimension(:), allocatable :: a
          !dir$ attributes align : 64 :: a
#endif  
    end type Af2162_t
    
    
  ! The values of complex Fresnel coefficients, vertical polarization (2.169)
    type, public :: Rvf2169_t
          integer(kind=i4) :: nRvf2169  ! number of complex Fresnel coefficients, vertical polarization (2.169)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:), allocatable :: rv
           !dir$ attributes align : 64 :: rv
#else
          complex(kind=sp), dimension(:), allocatable :: rv
          !dir$ attributes align : 64 :: rv
#endif            
    end type Rvf2169_t
     
  
  ! The values of complex Fresnel coefficients, horizontal polarization (2.170)
    type, public :: Rvf2170_t
          integer(kind=i4) :: nRhf2170  ! number of complex Fresnel coefficients, horizontal polarization (2.170)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:), allocatable :: rv
           !dir$ attributes align : 64 :: rv
#else
          complex(kind=sp), dimension(:), allocatable :: rv
          !dir$ attributes align : 64 :: rv
#endif             
    end type Rvf2170_t
    
    
  ! The values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
    type, public :: Evf2172_t
          integer(kind=i4) :: nEvf2172  ! number of values of Electrical field, 
                                  ! vertical polarization, receiption point (2.172)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:), allocatable :: ev
           !dir$ attributes align : 64 :: ev
#else
          complex(kind=sp), dimension(:), allocatable :: ev
          !dir$ attributes align : 64 :: ev
#endif                                   
    end type Evf2172_t
    
    
  ! The values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
    type, public :: Ehf2173_t
          integer(kind=i4) :: nEhf2172  ! number of values of Electrical field, 
                                  ! horizontal polarization, receiption point (2.173)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:), allocatable :: eh
           !dir$ attributes align : 64 :: eh
#else
          complex(kind=sp), dimension(:), allocatable :: eh
          !dir$ attributes align : 64 :: eh
#endif               
    end type Ehf2173_t
    
    
  ! Internal antenna noise temperature values (2.179)
    type, public :: Tf2179_t
          integer(kind=i4) :: nTf2179   ! number of internal antenna noise temperature values (2.179)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          real(kind=sp), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif            
    end type Tf2179_t
     
   
  ! External antenna noise temperature values (2.180) 
    type, public :: Tf2180_t
          integer(kind=i4) :: nTf2180   ! number of external to antenna noise temperature values (2.180)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          real(kind=sp), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif            
    end type Tf2180_t
    
    
  ! The values of noise atmosphere temperature (2.181)
    type, public :: Tf2181_t
          integer(kind=i4) :: nTf2181   ! number of values of noise atmosphere temperature (2.181)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          real(kind=sp), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif           
    end type Tf2181_t 
    
    
  ! The values of total antenna noise temperature (2.182)
    type, public :: Tf2182_t
          integer(kind=i4) :: nTf2182   ! number of values of total noise antenna temperature (2.182)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: t
           !dir$ attributes align : 64 :: t
#else
          real(kind=sp), dimension(:), allocatable :: t
          !dir$ attributes align : 64 :: t
#endif           
    end type Tf2182_t
  
    
  ! The values of optical curve length for 'n' curves (2.186)
    type, public :: Qf2186_t
          integer(kind=i4) :: nQf2186   ! number of values of optical curve length (2.186)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          real(kind=dp), dimension(:), allocatable :: q
           !dir$ attributes align : 64 :: q
#else
          real(kind=sp), dimension(:), allocatable :: q
          !dir$ attributes align : 64 :: q
#endif             
    end type Qf2186_t
   
    
  ! The values of 'x,y,z' (eikonal) coordinate (2.187)
    type, public :: Lf2187_t
          integer(kind=i4) :: nLxf2187  ! number of values of 'x' (eikonal) coordinate (2.187)
          integer(kind=i4) :: nLyf2187  ! number of values of 'y' (eikonal) coordinate (2.187)
          integer(kind=i4) :: nLzf2187  ! number of values of 'z' (eikonal) coordinate (2.187)
#if (ANTENNA_COMMON_SADT_USE_PRECISION_REAL8) == 1              
          complex(kind=dp), dimension(:,:,:), allocatable :: lxyz
           !dir$ attributes align : 64 :: lxyz
#else
          complex(kind=sp), dimension(:,:,:), allocatable :: lxyz
          !dir$ attributes align : 64 :: lxyz
#endif             
    end type Lf2187_t
    
   
    
   
     
   
     
   
     
    
     
    
    
     
     
end module antenna_common_adt
