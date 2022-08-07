

module MM_VIE


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'MM_VIE'
 !          
 !          Purpose:
 !                     This module contains implementation of Method of Moments
 !                      Volume Integral Equation (VIE).
 !          History:
 !                        
 !                        Date: 01-01-2022
 !                        Time: 09:54 GMT+2
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
     use mod_kinds,    only : i1,i4,sp,dp
     use mod_avx512c16f32
     use mod_avx512c8f64
     use mod_vectypes, only : ZMM16r4_t,ZMM8r8_t
     implicit none
     public


     !!===========================================================!!
     !         Data types for VIE                                  !
     !=============================================================!

      ! Input data for MM Direct Volume Integral code
     type, public :: VIE_In_RC4
           public
          
           complex(kind=sp) :: inc_eth  ! Complex value of E (incident theta)
           complex(kind=sp) :: inc_eph  ! Complex value of E (incident phi)
           real(kind=sp)    :: freq     ! [Mhz]
           real(kind=sp)    :: inc_th   ! [Deg], theta of incident wave angle
           real(kind=sp)    :: inc_ph   ! [Deg], phi   of incident wave angle
           real(kind=sp)    :: sc_th    ! [Deg]  initial scattered theta angle
           real(kind=sp)    :: sc_ph    ! [Deg]  initial scattered phi angle
           integer(kind=i4) :: nth      ! Number of theta angle increments
           integer(kind=i4) :: nph      ! Number of phi angle increments
           logical(kind=i1) :: accuracy !if .false.  no numerical integration, if .true. numerical integration will be
           !                                       computed
     end type VIE_In_RC4

     ! Mesh data type for MM Direct Volume Integral
     type, public :: VIE_Mesh_RC4
           public
           integer(kind=i4), dimension(3) :: ndiv ! Number of division (x,y,x) for each rectangular volume cell
           integer(kind=i4) :: n_cells  ! Total number of volume cells (discretized)
#if defined(__INTEL_COMPILER) || defined(__ICC)   
           real(kind=sp),    allocatable, dimension(:)    :: cell_cx ! X-coordinate of n-th cell centre
           real(kind=sp),    allocatable, dimension(:)    :: cell_cy ! Y-coordinate of n-th cell centre
           real(kind=sp),    allocatable, dimension(:)    :: cell_cz ! Z-coordinate of n-th cell centre
           real(kind=sp),    allocatable, dimension(:)    :: cell_vl ! Volume of n-th cell
           complex(kind=sp), allocatable, dimension(:)    :: cell_ee ! Complex relative permittivity of n-th cell
           real(kind=sp),    allocatable, dimension(:)    :: cell_dx ! X-dimension of  n-th cell
           real(kind=sp),    allocatable, dimension(:)    :: cell_dy ! Y-dimension of  n-th cell
           real(kind=sp),    allocatable, dimension(:)    :: cell_dz ! Z-dimension of  n-th cell
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cz
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cz
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_ee
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dz
           
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))

           real(kind=sp),    allocatable, dimension(:)    :: cell_cx !GCC$ ATTRIBUTES aligned(64) :: cell_cx
           real(kind=sp),    allocatable, dimension(:)    :: cell_cy !GCC$ ATTRIBUTES aligned(64) :: cell_cy
           real(kind=sp),    allocatable, dimension(:)    :: cell_cz !GCC$ ATTRIBUTES aligned(64) :: cell_cz
           real(kind=sp),    allocatable, dimension(:)    :: cell_vl !GCC$ ATTRIBUTES aligned(64) :: cell_vl
           complex(kind=sp), allocatable, dimension(:)    :: cell_ee !GCC$ ATTRIBUTES aligned(64) :: cell_ee
           real(kind=sp),    allocatable, dimension(:)    :: cell_dx !GCC$ ATTRIBUTES aligned(64) :: cell_dx
           real(kind=sp),    allocatable, dimension(:)    :: cell_dy !GCC$ ATTRIBUTES aligned(64) :: cell_dy
           real(kind=sp),    allocatable, dimension(:)    :: cell_dz !GCC$ ATTRIBUTES aligned(64) :: cell_dz
#endif
     end type VIE_Mesh_RC4

     ! Output data for MM Direct Volume Integral code.
     
     type, public :: EE_XYZ_RC4
           public
           integer(kind=i4) :: n_cells  ! Total number of volume cells (discretized)
#if defined(__INTEL_COMPILER) || defined(__ICC)             
           complex(kind=sp), allocatable, dimension(:) :: EEx !Complex magnitude x-component [V/m]
           complex(kind=sp), allocatable, dimension(:) :: EEy !Complex magnitude y-component [V/m]
           complex(kind=sp), allocatable, dimension(:) :: EEz !Complex magnitude z-component [V/m]
           real(kind=sp),    allocatable, dimension(:) :: EEt !Total Electric Field amplitude [V/m]
           real(kind=sp),    allocatable, dimension(:) :: PEd !Power dissipation [V/m^3]
           real(kind=sp),    allocatable, dimension(:) :: PEs !Energy density
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEx
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEy
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEz
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEt
           !DIR$ ATTRIBUTES ALIGN : 64 :: PEd
           !DIR$ ATTRIBUTES ALIGN : 64 :: PEs
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=sp), allocatable, dimension(:) :: EEx !GCC$ ATTRIBUTES aligned(64) :: EEx
           complex(kind=sp), allocatable, dimension(:) :: EEy !GCC$ ATTRIBUTES aligned(64) :: EEy
           complex(kind=sp), allocatable, dimension(:) :: EEz !GCC$ ATTRIBUTES aligned(64) :: EEz
           real(kind=sp),    allocatable, dimension(:) :: EEt !GCC$ ATTRIBUTES aligned(64) :: EEt
           real(kind=sp),    allocatable, dimension(:) :: PEd !GCC$ ATTRIBUTES aligned(64) :: PEd
           real(kind=sp),    allocatable, dimension(:) :: PEs !GCC$ ATTRIBUTES aligned(64) :: PEs
#endif
      end type EE_XYZ_RC4



      !!==================================================!
      !    Double precision derived types                 !
      !!==================================================!

         ! Input data for MM Direct Volume Integral code
     type, public :: VIE_In_RC8
           public
          
           complex(kind=dp) :: inc_eth  ! Complex value of E (incident theta)
           complex(kind=dp) :: inc_eph  ! Complex value of E (incident phi)
           real(kind=dp)    :: freq     ! [Mhz]
           real(kind=dp)    :: inc_th   ! [Deg], theta of incident wave angle
           real(kind=dp)    :: inc_ph   ! [Deg], phi   of incident wave angle
           real(kind=dp)    :: sc_th    ! [Deg]  initial scattered theta angle
           real(kind=dp)    :: sc_ph    ! [Deg]  initial scattered phi angle
           integer(kind=i4) :: nth      ! Number of theta angle increments
           integer(kind=i4) :: nph      ! Number of phi angle increments
           logical(kind=i1) :: accuracy !if .false.  no numerical integration, if .true. numerical integration will be
           !                                       computed
     end type VIE_In_RC8

     ! Mesh data type for MM Direct Volume Integral
     type, public :: VIE_Mesh_RC8
           public
           integer(kind=i4), dimension(3) :: ndiv ! Number of division (x,y,x) for each rectangular volume cell
           integer(kind=i4) :: n_cells  ! Total number of volume cells (discretized)
#if defined(__INTEL_COMPILER) || defined(__ICC)   
           real(kind=dp),    allocatable, dimension(:)    :: cell_cx ! X-coordinate of n-th cell centre
           real(kind=dp),    allocatable, dimension(:)    :: cell_cy ! Y-coordinate of n-th cell centre
           real(kind=dp),    allocatable, dimension(:)    :: cell_cz ! Z-coordinate of n-th cell centre
           real(kind=dp),    allocatable, dimension(:)    :: cell_vl ! Volume of n-th cell
           complex(kind=dp), allocatable, dimension(:)    :: cell_ee ! Complex relative permittivity of n-th cell
           real(kind=dp),    allocatable, dimension(:)    :: cell_dx ! X-dimension of  n-th cell
           real(kind=dp),    allocatable, dimension(:)    :: cell_dy ! Y-dimension of  n-th cell
           real(kind=dp),    allocatable, dimension(:)    :: cell_dz ! Z-dimension of  n-th cell
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cz
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_cz
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_ee
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dx
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dy
           !DIR$ ATTRIBUTES ALIGN : 64 :: cell_dz
           
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))

           real(kind=dp),    allocatable, dimension(:)    :: cell_cx !GCC$ ATTRIBUTES aligned(64) :: cell_cx
           real(kind=dp),    allocatable, dimension(:)    :: cell_cy !GCC$ ATTRIBUTES aligned(64) :: cell_cy
           real(kind=dp),    allocatable, dimension(:)    :: cell_cz !GCC$ ATTRIBUTES aligned(64) :: cell_cz
           real(kind=dp),    allocatable, dimension(:)    :: cell_vl !GCC$ ATTRIBUTES aligned(64) :: cell_vl
           complex(kind=dp), allocatable, dimension(:)    :: cell_ee !GCC$ ATTRIBUTES aligned(64) :: cell_ee
           real(kind=dp),    allocatable, dimension(:)    :: cell_dx !GCC$ ATTRIBUTES aligned(64) :: cell_dx
           real(kind=dp),    allocatable, dimension(:)    :: cell_dy !GCC$ ATTRIBUTES aligned(64) :: cell_dy
           real(kind=dp),    allocatable, dimension(:)    :: cell_dz !GCC$ ATTRIBUTES aligned(64) :: cell_dz
#endif
     end type VIE_Mesh_RC8

     ! Output data for MM Direct Volume Integral code.
     
     type, public :: EE_XYZ_RC8
           public
           integer(kind=i4) :: n_cells  ! Total number of volume cells (discretized)
#if defined(__INTEL_COMPILER) || defined(__ICC)             
           complex(kind=dp), allocatable, dimension(:) :: EEx !Complex magnitude x-component [V/m]
           complex(kind=dp), allocatable, dimension(:) :: EEy !Complex magnitude y-component [V/m]
           complex(kind=dp), allocatable, dimension(:) :: EEz !Complex magnitude z-component [V/m]
           real(kind=dp),    allocatable, dimension(:) :: EEt !Total Electric Field amplitude [V/m]
           real(kind=dp),    allocatable, dimension(:) :: PEd !Power dissipation [V/m^3]
           real(kind=dp),    allocatable, dimension(:) :: PEs !Energy density
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEx
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEy
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEz
           !DIR$ ATTRIBUTES ALIGN : 64 :: EEt
           !DIR$ ATTRIBUTES ALIGN : 64 :: PEd
           !DIR$ ATTRIBUTES ALIGN : 64 :: PEs
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           complex(kind=dp), allocatable, dimension(:) :: EEx !GCC$ ATTRIBUTES aligned(64) :: EEx
           complex(kind=dp), allocatable, dimension(:) :: EEy !GCC$ ATTRIBUTES aligned(64) :: EEy
           complex(kind=dp), allocatable, dimension(:) :: EEz !GCC$ ATTRIBUTES aligned(64) :: EEz
           real(kind=dp),    allocatable, dimension(:) :: EEt !GCC$ ATTRIBUTES aligned(64) :: EEt
           real(kind=dp),    allocatable, dimension(:) :: PEd !GCC$ ATTRIBUTES aligned(64) :: PEd
           real(kind=dp),    allocatable, dimension(:) :: PEs !GCC$ ATTRIBUTES aligned(64) :: PEs
#endif
      end type EE_XYZ_RC8


        

























end module MM_VIE
