

#include "GMS_config.fpp"

module antenna_types


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         antenna_types
 !          
 !          Purpose:
 !                        Derived data types for 'antenna_sensor' module implementation.
 !                        Various characteristics of different antenna types  
 !                        Based mainly on book titled (rus):          
 !                        Проектирование антенно фидерных устройств. Жук М.С. Молочков Ю.Б
 !          History:
 !                        Date: 10-11-2022
 !                        Time: 12:53 GMT+2
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
    integer(kind=i4),  parameter :: ANTENNA_TYPES_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: ANTENNA_TYPES_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: ANTENNA_TYPES_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: ANTENNA_TYPES_FULLVER =   &
            1000*ANTENNA_TYPES_MAJOR+100*ANTENNA_TYPES_MINOR+10*ANTENNA_TYPES_MICRO
    ! Module creation date
    character(*),        parameter :: ANTENNA_TYPES_CREATE_DATE = "10-11-2022 12:54 +00200 (THR 10 NOV 2022 GMT+2)"
    ! Module build date
    character(*),        parameter :: ANTENNA_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: ANTENNA_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: ANTENNA_TYPES_SYNOPSIS    = "ADT describing various antenna types characteristics."

    
    type, public :: E_c4_t
          ! Complex 3D Electric Field
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          complex(kind=sp), dimension(:), allocatable :: e_x
          complex(kind=sp), dimension(:), allocatable :: e_y
          complex(kind=sp), dimension(:), allocatable :: e_z
          !dir$ attributes align : 64 :: e_x
          !dir$ attributes align : 64 :: e_y
          !dir$ attributes align : 64 :: e_z
    end type E_c4_t


    type, public :: H_c4_t
          ! Complex 3D Magnetic Field
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          complex(kind=sp), dimension(:), allocatable :: h_x
          complex(kind=sp), dimension(:), allocatable :: h_y
          complex(kind=sp), dimension(:), allocatable :: h_z
          !dir$ attributes align : 64 :: h_x
          !dir$ attributes align : 64 :: h_y
          !dir$ attributes align : 64 :: h_z
    end type H_c4_t


    type, public :: E_c8_t
          ! Complex 3D Electric Field
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          complex(kind=dp), dimension(:), allocatable :: e_x
          complex(kind=dp), dimension(:), allocatable :: e_y
          complex(kind=dp), dimension(:), allocatable :: e_z
          !dir$ attributes align : 64 :: e_x
          !dir$ attributes align : 64 :: e_y
          !dir$ attributes align : 64 :: e_z
    end type E_c8_t


    type, public :: H_c8_t
          ! Complex 3D Magnetic Field
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          complex(kind=dp), dimension(:), allocatable :: h_x
          complex(kind=dp), dimension(:), allocatable :: h_y
          complex(kind=dp), dimension(:), allocatable :: h_z
          !dir$ attributes align : 64 :: h_x
          !dir$ attributes align : 64 :: h_y
          !dir$ attributes align : 64 :: h_z
    end type H_c8_t


    type, public :: E_r4_t
       ! Complex Electric  field  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)           :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=sp), dimension(:), allocatable :: e_xr
       real(kind=sp), dimension(:), allocatable :: e_xi
       real(kind=sp), dimension(:), allocatable :: e_yr
       real(kind=sp), dimension(:), allocatable :: e_yi
       real(kind=sp), dimension(:), allocatable :: e_zr
       real(kind=sp), dimension(:), allocatable :: e_zi
       !dir$ attributes align : 64 :: e_xr
       !dir$ attributes align : 64 :: e_xi
       !dir$ attributes align : 64 :: e_yr
       !dir$ attributes align : 64 :: e_yi
       !dir$ attributes align : 64 :: e_zr
       !dir$ attributes align : 64 :: e_zi
    end type E_r4_t


    type, public :: H_r4_t
       ! Complex Magnetic field  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)           :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=sp), dimension(:), allocatable :: h_xr
       real(kind=sp), dimension(:), allocatable :: h_xi
       real(kind=sp), dimension(:), allocatable :: h_yr
       real(kind=sp), dimension(:), allocatable :: h_yi
       real(kind=sp), dimension(:), allocatable :: h_zr
       real(kind=sp), dimension(:), allocatable :: h_zi
       !dir$ attributes align : 64 :: h_xr
       !dir$ attributes align : 64 :: h_xi
       !dir$ attributes align : 64 :: h_yr
       !dir$ attributes align : 64 :: h_yi
       !dir$ attributes align : 64 :: h_zr
       !dir$ attributes align : 64 :: h_zi
    end type H_r4_t


    type, public :: E_r8_t
       ! Complex Electric  field  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)           :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=dp), dimension(:), allocatable :: e_xr
       real(kind=dp), dimension(:), allocatable :: e_xi
       real(kind=dp), dimension(:), allocatable :: e_yr
       real(kind=dp), dimension(:), allocatable :: e_yi
       real(kind=dp), dimension(:), allocatable :: e_zr
       real(kind=dp), dimension(:), allocatable :: e_zi
       !dir$ attributes align : 64 :: e_xr
       !dir$ attributes align : 64 :: e_xi
       !dir$ attributes align : 64 :: e_yr
       !dir$ attributes align : 64 :: e_yi
       !dir$ attributes align : 64 :: e_zr
       !dir$ attributes align : 64 :: e_zi
    end type E_r8_t


    type, public :: H_r8_t
       ! Complex Magnetic field  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)           :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=dp), dimension(:), allocatable :: h_xr
       real(kind=dp), dimension(:), allocatable :: h_xi
       real(kind=dp), dimension(:), allocatable :: h_yr
       real(kind=dp), dimension(:), allocatable :: h_yi
       real(kind=dp), dimension(:), allocatable :: h_zr
       real(kind=dp), dimension(:), allocatable :: h_zi
       !dir$ attributes align : 64 :: h_xr
       !dir$ attributes align : 64 :: h_xi
       !dir$ attributes align : 64 :: h_yr
       !dir$ attributes align : 64 :: h_yi
       !dir$ attributes align : 64 :: h_zr
       !dir$ attributes align : 64 :: h_zi
    end type H_r8_t


    type, public :: JE_c4_t
       ! Complex Electric Current
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
        complex(kind=sp), dimension(:), allocatable :: je_x
        complex(kind=sp), dimension(:), allocatable :: je_y
        complex(kind=sp), dimension(:), allocatable :: je_z
        !dir$ attributes align : 64 :: je_x
        !dir$ attributes align : 64 :: je_y
        !dir$ attributes align : 64 :: je_z
    end type JE_c4_t


    type, public :: JM_c4_t
       ! Complex Magnetic Current
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
        complex(kind=sp), dimension(:), allocatable :: jm_x
        complex(kind=sp), dimension(:), allocatable :: jm_y
        complex(kind=sp), dimension(:), allocatable :: jm_z
        !dir$ attributes align : 64 :: jm_x
        !dir$ attributes align : 64 :: jm_y
        !dir$ attributes align : 64 :: jm_z
    end type JM_c4_t


    type, public :: JE_c8_t
       ! Complex Electric Current
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
        complex(kind=dp), dimension(:), allocatable :: je_x
        complex(kind=dp), dimension(:), allocatable :: je_y
        complex(kind=dp), dimension(:), allocatable :: je_z
        !dir$ attributes align : 64 :: je_x
        !dir$ attributes align : 64 :: je_y
        !dir$ attributes align : 64 :: je_z
    end type JE_c8_t


    type, public :: JM_c8_t
       ! Complex Magnetic Current
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
        complex(kind=dp), dimension(:), allocatable :: jm_x
        complex(kind=dp), dimension(:), allocatable :: jm_y
        complex(kind=dp), dimension(:), allocatable :: jm_z
        !dir$ attributes align : 64 :: jm_x
        !dir$ attributes align : 64 :: jm_y
        !dir$ attributes align : 64 :: jm_z
    end type JM_c8_t


    type, public :: JE_r4_t
       ! Complex Electric Current  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=sp), dimension(:), allocatable :: je_xr
       real(kind=sp), dimension(:), allocatable :: je_xi
       real(kind=sp), dimension(:), allocatable :: je_yr
       real(kind=sp), dimension(:), allocatable :: je_yi
       real(kind=sp), dimension(:), allocatable :: je_zr
       real(kind=sp), dimension(:), allocatable :: je_zi
       !dir$ attributes align : 64 :: je_xr
       !dir$ attributes align : 64 :: je_xi
       !dir$ attributes align : 64 :: je_yr
       !dir$ attributes align : 64 :: je_yi
       !dir$ attributes align : 64 :: je_zr
       !dir$ attributes align : 64 :: je_zi
    end type JE_r4_t


    type, public :: JM_r4_t
       ! Complex Magnetic Current  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=sp), dimension(:), allocatable :: jm_xr
       real(kind=sp), dimension(:), allocatable :: jm_xi
       real(kind=sp), dimension(:), allocatable :: jm_yr
       real(kind=sp), dimension(:), allocatable :: jm_yi
       real(kind=sp), dimension(:), allocatable :: jm_zr
       real(kind=sp), dimension(:), allocatable :: jm_zi
       !dir$ attributes align : 64 :: jm_xr
       !dir$ attributes align : 64 :: jm_xi
       !dir$ attributes align : 64 :: jm_yr
       !dir$ attributes align : 64 :: jm_yi
       !dir$ attributes align : 64 :: jm_zr
       !dir$ attributes align : 64 :: jm_zi
    end type JM_r4_t


    type, public :: JE_r8_t
       ! Complex Electric Current  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=dp), dimension(:), allocatable :: je_xr
       real(kind=dp), dimension(:), allocatable :: je_xi
       real(kind=dp), dimension(:), allocatable :: je_yr
       real(kind=dp), dimension(:), allocatable :: je_yi
       real(kind=dp), dimension(:), allocatable :: je_zr
       real(kind=dp), dimension(:), allocatable :: je_zi
       !dir$ attributes align : 64 :: je_xr
       !dir$ attributes align : 64 :: je_xi
       !dir$ attributes align : 64 :: je_yr
       !dir$ attributes align : 64 :: je_yi
       !dir$ attributes align : 64 :: je_zr
       !dir$ attributes align : 64 :: je_zi
    end type JE_r8_t


    type, public :: JM_r8_t
       ! Complex Magnetic Current  decomposed into real and imaginary parts 
       ! To be used mainly by the integrators.
       integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
       real(kind=dp), dimension(:), allocatable :: jm_xr
       real(kind=dp), dimension(:), allocatable :: jm_xi
       real(kind=dp), dimension(:), allocatable :: jm_yr
       real(kind=dp), dimension(:), allocatable :: jm_yi
       real(kind=dp), dimension(:), allocatable :: jm_zr
       real(kind=dp), dimension(:), allocatable :: jm_zi
       !dir$ attributes align : 64 :: jm_xr
       !dir$ attributes align : 64 :: jm_xi
       !dir$ attributes align : 64 :: jm_yr
       !dir$ attributes align : 64 :: jm_yi
       !dir$ attributes align : 64 :: jm_zr
       !dir$ attributes align : 64 :: jm_zi
    end type JM_r8_t


    type, public :: EIKR_c4_t
       ! Time-Harmonic complex exponential
       integer(kind=i4)        :: npts
       real(kind=sp)           :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,24)
#endif
       real(kind=sp),    dimension(:), allocatable :: R
       complex(kind=sp), dimension(:), allocatable :: eikr
       !dir$ attributes align : 64 :: R
       !dir$ attributes align : 64 :: eikr
    end type EIKR_c4_t


    type, public :: EIKR_c8_t
       ! Time-Harmonic complex exponential
       integer(kind=i4)        :: npts
       real(kind=dp)           :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
       real(kind=dp),    dimension(:), allocatable :: R
       complex(kind=dp), dimension(:), allocatable :: eikr
       !dir$ attributes align : 64 :: R
       !dir$ attributes align : 64 :: eikr
    end type EIKR_c8_t


    type, public :: EIKR_r4_t
        ! Time-Harmonic complex exponential decomposed into 
        ! real and imaginary parts
       integer(kind=i4)        :: npts
       real(kind=sp)           :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,24)
#endif
       real(kind=sp), dimension(:), allocatable :: R
       real(kind=sp), dimension(:), allocatable :: e_re
       real(kind=sp), dimension(:), allocatable :: e_im
       !dir$ attributes align : 64 :: R
       !dir$ attributes align : 64 :: e_re
       !dir$ attributes align : 64 :: e_im
    end type EIKR_r4_t


    type, public :: EIKR_r8_t
        ! Time-Harmonic complex exponential decomposed into 
        ! real and imaginary parts
       integer(kind=i4)        :: npts
       real(kind=dp)           :: k
       
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
       real(kind=dp), dimension(:), allocatable :: R
       real(kind=dp), dimension(:), allocatable :: e_re
       real(kind=dp), dimension(:), allocatable :: e_im
       !dir$ attributes align : 64 :: R
       !dir$ attributes align : 64 :: e_re
       !dir$ attributes align : 64 :: e_im
    end type EIKR_r8_t


    
    ! Formula (1-37)
    ! Average level of side lobes
    type, public :: avg_slobes_r4_t

         integer(kind=i4)      :: nth  ! number of theta angles
         integer(kind=i4)      :: nphi ! number of phi angles
         real(kind=sp)         :: ifac  ! Integral factor
         real(kind=sp)         :: omega ! Steradian angles
         real(kind=sp)         :: asl ! the result
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif   
         real(kind=sp), dimension(:), allocatable :: sinth ! Sine of theta angle
         real(kind=sp), dimension(:), allocatable :: F     ! Antenna  rdiation pattern 
         !dir$ attributes align : 64 :: sinth
         !dir$ attributes align : 64 :: F
    end type avg_slobes_r4_t


     ! Formula (1-37)
    ! Average level of side lobes
    type, public :: avg_slobes_r8_t

         integer(kind=i4)      :: nth  ! number of theta angles
         integer(kind=i4)      :: nphi ! number of phi angles
         real(kind=dp)         :: ifac  ! Integral factor
         real(kind=dp)         :: omega ! Steradian angles
         real(kind=dp)         :: asl ! the result
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif   
         real(kind=dp), dimension(:), allocatable :: sinth ! Sine of theta angle
         real(kind=dp), dimension(:), allocatable :: F     ! Antenna  radiation pattern 
         !dir$ attributes align : 64 :: sinth
         !dir$ attributes align : 64 :: F
    end type avg_slobes_r8_t


    ! Formula (1-38)
    ! Average (squared) level of side lobes
    type, public :: avgsqr_slobes_r4_t

          integer(kind=i4)      :: nth  ! number of theta angles
          integer(kind=i4)      :: nphi ! number of phi angles
          real(kind=sp)         :: ifac  ! Integral factor
          real(kind=sp)         :: omega ! Steradian angles
          real(kind=sp)         :: asl ! the result
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif   
          real(kind=sp), dimension(:), allocatable :: sinth ! Sine of theta angle
          real(kind=sp), dimension(:), allocatable :: Fsqr     ! Antenna  radiation pattern 
          !dir$ attributes align : 64 :: sinth
          !dir$ attributes align : 64 :: Fsqr
    end type avgsqr_slobes_r4_t


     ! Formula (1-38)
    ! Average (squared) level of side lobes
    type, public :: avgsqr_slobes_r8_t

          integer(kind=i4)      :: nth  ! number of theta angles
          integer(kind=i4)      :: nphi ! number of phi angles
          real(kind=dp)         :: ifac  ! Integral factor
          real(kind=dp)         :: omega ! Steradian angles
          real(kind=dp)         :: asl ! the result
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif   
          real(kind=dp), dimension(:), allocatable :: sinth ! Sine of theta angle
          real(kind=dp), dimension(:), allocatable :: Fsqr     ! Antenna  radiation pattern 
          !dir$ attributes align : 64 :: sinth
          !dir$ attributes align : 64 :: Fsqr
    end type avgsqr_slobes_r8_t


    ! Formula (1-39)
    ! Dispersion coefficient
    type, public :: dispers_coef_r4_t

          integer(kind=i4)      :: nth  ! number of theta angles
          integer(kind=i4)      :: nphi ! number of phi angles
          real(kind=sp)         :: omega ! steradian angle
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif  
          real(kind=sp), dimension(:), allocatable :: P     ! Antenna power radiation.
          real(kind=sp), dimension(:), allocatable :: sinth
          !dir$ attributes align : 64 :: P
          !dir$ attributes align : 64 :: sinth
    end type dispers_coef_r4_t

    ! Formula (1-39)
    ! Dispersion coefficient
    type, public :: dispers_coef_r8_t

          integer(kind=i4)      :: nth  ! number of theta angles
          integer(kind=i4)      :: nphi ! number of phi angles
          real(kind=dp)         :: omega ! steradian angle
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif  
          real(kind=dp), dimension(:), allocatable :: P     ! Antenna power radiation.
          real(kind=dp), dimension(:), allocatable :: sinth
          !dir$ attributes align : 64 :: P
          !dir$ attributes align : 64 :: sinth
    end type dispers_coef_r8_t

    ! Formula (2-13)
    type, public :: HVE_c4_t
          ! Hertz vector electric
          integer(kind=i4)      :: npts
          complex(kind=sp)      :: ifac
          type(JE_c4_t)         :: jec4
          type(EIKR_c4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif  
          complex(kind=sp), dimension(:), allocatable :: he_x
          complex(kind=sp), dimension(:), allocatable :: he_y
          complex(kind=sp), dimension(:), allocatable :: he_z
          !dir$ attributes align : 64 :: he_x
          !dir$ attributes align : 64 :: he_y
          !dir$ attributes align : 64 :: he_z
    end type HVE_c4_t

    ! Formula (2-13)
    type, public :: HVE_c8_t
          ! Hertz vector electric
          integer(kind=i4)      :: npts
          complex(kind=dp)      :: ifac
          type(JE_c8_t)         :: jec4
          type(EIKR_c8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif  
          complex(kind=dp), dimension(:), allocatable :: he_x
          complex(kind=dp), dimension(:), allocatable :: he_y
          complex(kind=dp), dimension(:), allocatable :: he_z
          !dir$ attributes align : 64 :: he_x
          !dir$ attributes align : 64 :: he_y
          !dir$ attributes align : 64 :: he_z
    end type HVE_c8_t


    ! Formula (2-13)
    type, public :: HVE_r4_t
          ! Hertz vector electronic decomposed 
          integer(kind=i4)      :: npts
          real(kind=sp)         :: if_re
          real(kind=sp)         :: if_im
          type(JE_r4_t)         :: jer4
          type(EIKR_r4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif 
          real(kind=sp), dimension(:), allocatable :: he_xr
          real(kind=sp), dimension(:), allocatable :: he_xi
          real(kind=sp), dimension(:), allocatable :: he_yr
          real(kind=sp), dimension(:), allocatable :: he_yi
          real(kind=sp), dimension(:), allocatable :: he_zr
          real(kind=sp), dimension(:), allocatable :: he_zi
          !dir$ attributes align : 64 :: he_xr
          !dir$ attributes align : 64 :: he_xi
          !dir$ attributes align : 64 :: he_yr
          !dir$ attributes align : 64 :: he_yi
          !dir$ attributes align : 64 :: he_zr
          !dir$ attributes align : 64 :: he_zi
    end type HVE_r4_t


     ! Formula (2-13)
    type, public :: HVE_r8_t
          ! Hertz vector electronic decomposed 
          integer(kind=i4)      :: npts
          real(kind=dp)         :: if_re
          real(kind=dp)         :: if_im
          type(JE_r8_t)         :: jer4
          type(EIKR_r8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          real(kind=dp), dimension(:), allocatable :: he_xr
          real(kind=dp), dimension(:), allocatable :: he_xi
          real(kind=dp), dimension(:), allocatable :: he_yr
          real(kind=dp), dimension(:), allocatable :: he_yi
          real(kind=dp), dimension(:), allocatable :: he_zr
          real(kind=dp), dimension(:), allocatable :: he_zi
          !dir$ attributes align : 64 :: he_xr
          !dir$ attributes align : 64 :: he_xi
          !dir$ attributes align : 64 :: he_yr
          !dir$ attributes align : 64 :: he_yi
          !dir$ attributes align : 64 :: he_zr
          !dir$ attributes align : 64 :: he_zi
    end type HVE_r8_t


    ! Formula (2-15)
    type, public :: HVM_c4_t
          ! Hertz vector magnetic
          integer(kind=i4)      :: npts
          complex(kind=sp)      :: ifac
          type(JM_c4_t)         :: jmc4
          type(EIKR_c4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif  
          complex(kind=sp), dimension(:), allocatable :: hm_x
          complex(kind=sp), dimension(:), allocatable :: hm_y
          complex(kind=sp), dimension(:), allocatable :: hm_z
          !dir$ attributes align : 64 :: hm_x
          !dir$ attributes align : 64 :: hm_y
          !dir$ attributes align : 64 :: hm_z
    end type HVM_c4_t

    ! Formula (2-15)
    type, public :: HVM_c8_t
          ! Hertz vector magnetic
          integer(kind=i4)      :: npts
          complex(kind=dp)      :: ifac
          type(JM_c8_t)         :: jmc8
          type(EIKR_c8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif  
          complex(kind=dp), dimension(:), allocatable :: hm_x
          complex(kind=dp), dimension(:), allocatable :: hm_y
          complex(kind=dp), dimension(:), allocatable :: hm_z
          !dir$ attributes align : 64 :: hm_x
          !dir$ attributes align : 64 :: hm_y
          !dir$ attributes align : 64 :: hm_z
    end type HVM_c8_t


    ! Formula (2-15)
    type, public :: HVM_r4_t
          ! Hertz vector magnetic decomposed 
          integer(kind=i4)      :: npts
          real(kind=sp)         :: if_re
          real(kind=sp)         :: if_im
          type(JM_r4_t)         :: jmr4
          type(EIKR_r4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif 
          real(kind=sp), dimension(:), allocatable :: hm_xr
          real(kind=sp), dimension(:), allocatable :: hm_xi
          real(kind=sp), dimension(:), allocatable :: hm_yr
          real(kind=sp), dimension(:), allocatable :: hm_yi
          real(kind=sp), dimension(:), allocatable :: hm_zr
          real(kind=sp), dimension(:), allocatable :: hm_zi
          !dir$ attributes align : 64 :: hm_xr
          !dir$ attributes align : 64 :: hm_xi
          !dir$ attributes align : 64 :: hm_yr
          !dir$ attributes align : 64 :: hm_yi
          !dir$ attributes align : 64 :: hm_zr
          !dir$ attributes align : 64 :: hm_zi
    end type HVM_r4_t

     ! Far-field radiation-patter of antenna
     ! Formula (2-15)
    type, public :: HVM_r8_t
          ! Hertz vector magnetic decomposed 
          integer(kind=i4)      :: npts
          real(kind=dp)         :: if_re
          real(kind=dp)         :: if_im
          type(JM_r8_t)         :: jmr8
          type(EIKR_r8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          real(kind=dp), dimension(:), allocatable :: hm_xr
          real(kind=dp), dimension(:), allocatable :: hm_xi
          real(kind=dp), dimension(:), allocatable :: hm_yr
          real(kind=dp), dimension(:), allocatable :: hm_yi
          real(kind=dp), dimension(:), allocatable :: hm_zr
          real(kind=dp), dimension(:), allocatable :: hm_zi
          !dir$ attributes align : 64 :: hm_xr
          !dir$ attributes align : 64 :: hm_xi
          !dir$ attributes align : 64 :: hm_yr
          !dir$ attributes align : 64 :: hm_yi
          !dir$ attributes align : 64 :: hm_zr
          !dir$ attributes align : 64 :: hm_zi
    end type HVM_r8_t


    ! Formula (2-22,2-23)
    type, public :: NEVec_r4_t

          integer(kind=i4)      :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JE_r4_t)         :: jer4
          type(EIKR_r4_t)       :: eikr
          real(kind=sp), dimension(:), allocatable :: costh
          real(kind=sp), dimension(:), allocatable :: ne_xr
          real(kind=sp), dimension(:), allocatable :: ne_xi
          real(kind=sp), dimension(:), allocatable :: ne_yr
          real(kind=sp), dimension(:), allocatable :: ne_yi
          real(kind=sp), dimension(:), allocatable :: ne_zr
          real(kind=sp), dimension(:), allocatable :: ne_zi
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: ne_xr
          !dir$ attributes align : 64 :: ne_xi
          !dir$ attributes align : 64 :: ne_yr
          !dir$ attributes align : 64 :: ne_yi
          !dir$ attributes align : 64 :: ne_zr
          !dir$ attributes align : 64 :: ne_zi
    end type NEVec_r4_t


     ! Formula (2-22,2-23)
    type, public :: NEVec_r8_t

          integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JE_r8_t)          :: jer8
          type(EIKR_r8_t)        :: eikr
          real(kind=dp), dimension(:), allocatable :: costh
          real(kind=dp), dimension(:), allocatable :: ne_xr
          real(kind=dp), dimension(:), allocatable :: ne_xi
          real(kind=dp), dimension(:), allocatable :: ne_yr
          real(kind=dp), dimension(:), allocatable :: ne_yi
          real(kind=dp), dimension(:), allocatable :: ne_zr
          real(kind=dp), dimension(:), allocatable :: ne_zi
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: ne_xr
          !dir$ attributes align : 64 :: ne_xi
          !dir$ attributes align : 64 :: ne_yr
          !dir$ attributes align : 64 :: ne_yi
          !dir$ attributes align : 64 :: ne_zr
          !dir$ attributes align : 64 :: ne_zi
    end type NEVec_r8_t


    ! Formula (2-22,2-23) 
    type, public :: NEVec_c4_t

           integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JE_c4_t)           :: jec4
          type(EIKR_c4_t)         :: eikr
          real(kind=sp),    dimension(:), allocatable :: costh
          complex(kind=sp), dimension(:), allocatable :: ne_x
          complex(kind=sp), dimension(:), allocatable :: ne_y
          complex(kind=sp), dimension(:), allocatable :: ne_z
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: ne_x
          !dir$ attributes align : 64 :: ne_y
          !dir$ attributes align : 64 :: ne_z
    end type NEVec_c4_t


     ! Formula (2-22,2-23) 
    type, public :: NEVec_c8_t

           integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JE_c8_t)           :: jec8
          type(EIKR_c8_t)         :: eikr
          real(kind=dp),    dimension(:), allocatable :: costh
          complex(kind=dp), dimension(:), allocatable :: ne_x
          complex(kind=dp), dimension(:), allocatable :: ne_y
          complex(kind=dp), dimension(:), allocatable :: ne_z
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: ne_x
          !dir$ attributes align : 64 :: ne_y
          !dir$ attributes align : 64 :: ne_z
    end type NEVec_c8_t


     ! Formula (2-24,2-25)
    type, public :: NMVec_r4_t

          integer(kind=i4)      :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JM_r4_t)         :: jmr4
          type(EIKR_r4_t)       :: eikr
          real(kind=sp), dimension(:), allocatable :: costh
          real(kind=sp), dimension(:), allocatable :: nm_xr
          real(kind=sp), dimension(:), allocatable :: nm_xi
          real(kind=sp), dimension(:), allocatable :: nm_yr
          real(kind=sp), dimension(:), allocatable :: nm_yi
          real(kind=sp), dimension(:), allocatable :: nm_zr
          real(kind=sp), dimension(:), allocatable :: nm_zi
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: nm_xr
          !dir$ attributes align : 64 :: nm_xi
          !dir$ attributes align : 64 :: nm_yr
          !dir$ attributes align : 64 :: nm_yi
          !dir$ attributes align : 64 :: nm_zr
          !dir$ attributes align : 64 :: nm_zi
    end type NMVec_r4_t


     ! Formula (2-24,2-25)
    type, public :: NMVec_r8_t

          integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JM_r8_t)          :: jmr8
          type(EIKR_r8_t)        :: eikr
          real(kind=dp), dimension(:), allocatable :: costh
          real(kind=dp), dimension(:), allocatable :: nm_xr
          real(kind=dp), dimension(:), allocatable :: nm_xi
          real(kind=dp), dimension(:), allocatable :: nm_yr
          real(kind=dp), dimension(:), allocatable :: nm_yi
          real(kind=dp), dimension(:), allocatable :: nm_zr
          real(kind=dp), dimension(:), allocatable :: nm_zi
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: nm_xr
          !dir$ attributes align : 64 :: nm_xi
          !dir$ attributes align : 64 :: nm_yr
          !dir$ attributes align : 64 :: nm_yi
          !dir$ attributes align : 64 :: nm_zr
          !dir$ attributes align : 64 :: nm_zi
    end type NMVec_r8_t


    ! Formula (2-24,2-25) 
    type, public :: NMVec_c4_t

           integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JM_c4_t)           :: jmc4
          type(EIKR_c4_t)         :: eikr
          real(kind=sp),    dimension(:), allocatable :: costh
          complex(kind=sp), dimension(:), allocatable :: nm_x
          complex(kind=sp), dimension(:), allocatable :: nm_y
          complex(kind=sp), dimension(:), allocatable :: nm_z
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: nm_x
          !dir$ attributes align : 64 :: nm_y
          !dir$ attributes align : 64 :: nm_z
    end type NMVec_c4_t


     ! Formula (2-24,2-25) 
    type, public :: NMVec_c8_t

           integer(kind=i4)       :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif 
          type(JE_c8_t)           :: jec8
          type(EIKR_r8_t)         :: eikr
          real(kind=dp),    dimension(:), allocatable :: costh
          complex(kind=dp), dimension(:), allocatable :: nm_x
          complex(kind=dp), dimension(:), allocatable :: nm_y
          complex(kind=dp), dimension(:), allocatable :: nm_z
          !dir$ attributes align : 64 :: costh
          !dir$ attributes align : 64 :: nm_x
          !dir$ attributes align : 64 :: nm_y
          !dir$ attributes align : 64 :: nm_z
    end type NMVec_c8_t

    
    ! Formula (2-22)
    ! "Far-field" Hertz vector electric
    type, public :: FFHEV_r4_t

          real(kind=sp)      :: if_re
          real(kind=sp)      :: if_im
          type(EIKR_r4_t)    :: eikr
          type(NEVec_r4_t)   :: nev
          real(kind=sp), dimension(:), allocatable :: hv_xr
          real(kind=sp), dimension(:), allocatable :: hv_xi
          real(kind=sp), dimension(:), allocatable :: hv_yr
          real(kind=sp), dimension(:), allocatable :: hv_yi
          real(kind=sp), dimension(:), allocatable :: hv_zr
          real(kind=sp), dimension(:), allocatable :: hv_zi
          !dir$ attributes align : 64 :: hv_xr
          !dir$ attributes align : 64 :: hv_xi
          !dir$ attributes align : 64 :: hv_yr
          !dir$ attributes align : 64 :: hv_yi
          !dir$ attributes align : 64 :: hv_zr
          !dir$ attributes align : 64 :: hv_zi
    end type FFHEV_r4_t


    ! Formula (2-22)
    ! "Far-field" Hertz vector electric
    type, public :: FFHEV_r8_t

          real(kind=dp)      :: if_re
          real(kind=dp)      :: if_im
          type(EIKR_r8_t)    :: eikr
          type(NEVec_r8_t)   :: nev
          real(kind=dp), dimension(:), allocatable :: hv_xr
          real(kind=dp), dimension(:), allocatable :: hv_xi
          real(kind=dp), dimension(:), allocatable :: hv_yr
          real(kind=dp), dimension(:), allocatable :: hv_yi
          real(kind=dp), dimension(:), allocatable :: hv_zr
          real(kind=dp), dimension(:), allocatable :: hv_zi
          !dir$ attributes align : 64 :: hv_xr
          !dir$ attributes align : 64 :: hv_xi
          !dir$ attributes align : 64 :: hv_yr
          !dir$ attributes align : 64 :: hv_yi
          !dir$ attributes align : 64 :: hv_zr
          !dir$ attributes align : 64 :: hv_zi
    end type FFHEV_r8_t


    ! Formula (2-22)
    ! "Far-field" Hertz vector electric
    type, public :: FFHEV_c4_t

          complex(kind=sp)       :: ifac
          type(EIKR_c4_t)        :: eikr
          type(NEVec_c4_t)       :: nev
          complex(kind=sp), dimension(:), allocatable :: hv_x
          complex(kind=sp), dimension(:), allocatable :: hv_y
          complex(kind=sp), dimension(:), allocatable :: hv_z
          !dir$ attributes align : 64 :: hv_x
          !dir$ attributes align : 64 :: hv_y
          !dir$ attributes align : 64 :: hv_z
    end type FFHEV_c4_t


    ! Formula (2-22)
    ! "Far-field" Hertz vector electric
    type, public :: FFHEV_c8_t

          complex(kind=dp)       :: ifac
          type(EIKR_c8_t)        :: eikr
          type(NEVec_c8_t)       :: nev
          complex(kind=dp), dimension(:), allocatable :: hv_x
          complex(kind=dp), dimension(:), allocatable :: hv_y
          complex(kind=dp), dimension(:), allocatable :: hv_z
          !dir$ attributes align : 64 :: hv_x
          !dir$ attributes align : 64 :: hv_y
          !dir$ attributes align : 64 :: hv_z
    end type FFHEV_c8_t


    ! Formula (2-24)
    ! "Far-field" Hertz vector magnetic
    type, public :: FFHMV_r4_t

          real(kind=sp)      :: if_re
          real(kind=sp)      :: if_im
          type(EIKR_r4_t)    :: eikr
          type(NMVec_r4_t)   :: nmv
          real(kind=sp), dimension(:), allocatable :: hv_xr
          real(kind=sp), dimension(:), allocatable :: hv_xi
          real(kind=sp), dimension(:), allocatable :: hv_yr
          real(kind=sp), dimension(:), allocatable :: hv_yi
          real(kind=sp), dimension(:), allocatable :: hv_zr
          real(kind=sp), dimension(:), allocatable :: hv_zi
          !dir$ attributes align : 64 :: hv_xr
          !dir$ attributes align : 64 :: hv_xi
          !dir$ attributes align : 64 :: hv_yr
          !dir$ attributes align : 64 :: hv_yi
          !dir$ attributes align : 64 :: hv_zr
          !dir$ attributes align : 64 :: hv_zi
    end type FFHMV_r4_t


    ! Formula (2-24)
    ! "Far-field" Hertz vector magnetic
    type, public :: FFHMV_r8_t

          real(kind=dp)      :: if_re
          real(kind=dp)      :: if_im
          type(EIKR_r8_t)    :: eikr
          type(NMVec_r8_t)   :: nmv
          real(kind=dp), dimension(:), allocatable :: hv_xr
          real(kind=dp), dimension(:), allocatable :: hv_xi
          real(kind=dp), dimension(:), allocatable :: hv_yr
          real(kind=dp), dimension(:), allocatable :: hv_yi
          real(kind=dp), dimension(:), allocatable :: hv_zr
          real(kind=dp), dimension(:), allocatable :: hv_zi
          !dir$ attributes align : 64 :: hv_xr
          !dir$ attributes align : 64 :: hv_xi
          !dir$ attributes align : 64 :: hv_yr
          !dir$ attributes align : 64 :: hv_yi
          !dir$ attributes align : 64 :: hv_zr
          !dir$ attributes align : 64 :: hv_zi
    end type FFHMV_r8_t


    ! Formula (2-24)
    ! "Far-field" Hertz vector magnetic
    type, public :: FFHMV_c4_t

          complex(kind=sp)       :: ifac
          type(EIKR_c4_t)        :: eikr
          type(NMVec_c4_t)       :: nmv
          complex(kind=sp), dimension(:), allocatable :: hv_x
          complex(kind=sp), dimension(:), allocatable :: hv_y
          complex(kind=sp), dimension(:), allocatable :: hv_z
          !dir$ attributes align : 64 :: hv_x
          !dir$ attributes align : 64 :: hv_y
          !dir$ attributes align : 64 :: hv_z
    end type FFHMV_c4_t


    ! Formula (2-24)
    ! "Far-field" Hertz vector electric
    type, public :: FFHMV_c8_t

          complex(kind=dp)       :: ifac
          type(EIKR_c8_t)        :: eikr
          type(NMVec_c8_t)       :: nmv
          complex(kind=dp), dimension(:), allocatable :: hv_x
          complex(kind=dp), dimension(:), allocatable :: hv_y
          complex(kind=dp), dimension(:), allocatable :: hv_z
          !dir$ attributes align : 64 :: hv_x
          !dir$ attributes align : 64 :: hv_y
          !dir$ attributes align : 64 :: hv_z
    end type FFHMV_c8_t


    ! Formula (2-26)
    ! Cosine of integration angle.
    type, public :: cos_iang_r4_t
          
          integer(kind=i4)          :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif      
          real(kind=sp), dimension(:), allocatable :: cth
          real(kind=sp), dimension(:), allocatable :: cthi
          real(kind=sp), dimension(:), allocatable :: sth
          real(kind=sp), dimension(:), allocatable :: sthi
          real(kind=sp), dimension(:), allocatable :: cphphi
          real(kind=sp), dimension(:), allocatable :: ciang ! the result  
          !dir$ attributes align : 64 :: cth  
          !dir$ attributes align : 64 :: cthi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: sthi
          !dir$ attributes align : 64 :: cphphi
          !dir$ attributes align : 64 :: ciang 
    end type cos_iang_r4_t


    ! Formula (2-26)
    ! Cosine of integration angle.
    type, public :: cos_iang_r8_t
          
          integer(kind=i4)          :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif      
          real(kind=dp), dimension(:), allocatable :: cth
          real(kind=dp), dimension(:), allocatable :: cthi
          real(kind=dp), dimension(:), allocatable :: sth
          real(kind=dp), dimension(:), allocatable :: sthi
          real(kind=dp), dimension(:), allocatable :: cphphi
          real(kind=dp), dimension(:), allocatable :: ciang ! the result  
          !dir$ attributes align : 64 :: cth  
          !dir$ attributes align : 64 :: cthi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: sthi
          !dir$ attributes align : 64 :: cphphi
          !dir$ attributes align : 64 :: ciang 
    end type cos_iang_r8_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NEsph_r4_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NEVec_r4_t)        :: nev
          real(kind=sp), dimension(:), allocatable :: cphi
          real(kind=sp), dimension(:), allocatable :: cth
          real(kind=sp), dimension(:), allocatable :: sphi
          real(kind=sp), dimension(:), allocatable :: sth
          real(kind=sp), dimension(:), allocatable :: nth_re
          real(kind=sp), dimension(:), allocatable :: nth_im
          real(kind=sp), dimension(:), allocatable :: nph_re
          real(kind=sp), dimension(:), allocatable :: nph_im
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth_re
          !dir$ attributes align : 64 :: nth_im
          !dir$ attributes align : 64 :: nph_re
          !dir$ attributes align : 64 :: nph_im
    end type NEsph_r4_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NEsph_r8_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NEVec_r8_t)        :: nev
          real(kind=dp), dimension(:), allocatable :: cphi
          real(kind=dp), dimension(:), allocatable :: cth
          real(kind=dp), dimension(:), allocatable :: sphi
          real(kind=dp), dimension(:), allocatable :: sth
          real(kind=dp), dimension(:), allocatable :: nth_re
          real(kind=dp), dimension(:), allocatable :: nth_im
          real(kind=dp), dimension(:), allocatable :: nph_re
          real(kind=dp), dimension(:), allocatable :: nph_im
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth_re
          !dir$ attributes align : 64 :: nth_im
          !dir$ attributes align : 64 :: nph_re
          !dir$ attributes align : 64 :: nph_im
    end type NEsph_r8_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NEsph_c4_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NEVec_c4_t)        :: nev
          real(kind=sp),    dimension(:), allocatable :: cphi
          real(kind=sp),    dimension(:), allocatable :: cth
          real(kind=sp),    dimension(:), allocatable :: sphi
          real(kind=sp),    dimension(:), allocatable :: sth
          complex(kind=sp), dimension(:), allocatable :: nth
          complex(kind=sp), dimension(:), allocatable :: nph
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth
          !dir$ attributes align : 64 :: nph
     end type NEsph_c4_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NEsph_c8_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NEVec_c8_t)        :: nev
          real(kind=dp),    dimension(:), allocatable :: cphi
          real(kind=dp),    dimension(:), allocatable :: cth
          real(kind=dp),    dimension(:), allocatable :: sphi
          real(kind=dp),    dimension(:), allocatable :: sth
          complex(kind=dp), dimension(:), allocatable :: nth
          complex(kind=dp), dimension(:), allocatable :: nph
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth
          !dir$ attributes align : 64 :: nph
     end type NEsph_c8_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NMsph_r4_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NMVec_r4_t)        :: nev
          real(kind=sp), dimension(:), allocatable :: cphi
          real(kind=sp), dimension(:), allocatable :: cth
          real(kind=sp), dimension(:), allocatable :: sphi
          real(kind=sp), dimension(:), allocatable :: sth
          real(kind=sp), dimension(:), allocatable :: nth_re
          real(kind=sp), dimension(:), allocatable :: nth_im
          real(kind=sp), dimension(:), allocatable :: nph_re
          real(kind=sp), dimension(:), allocatable :: nph_im
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth_re
          !dir$ attributes align : 64 :: nth_im
          !dir$ attributes align : 64 :: nph_re
          !dir$ attributes align : 64 :: nph_im
    end type NMsph_r4_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NMsph_r8_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NMVec_r8_t)        :: nev
          real(kind=dp), dimension(:), allocatable :: cphi
          real(kind=dp), dimension(:), allocatable :: cth
          real(kind=dp), dimension(:), allocatable :: sphi
          real(kind=dp), dimension(:), allocatable :: sth
          real(kind=dp), dimension(:), allocatable :: nth_re
          real(kind=dp), dimension(:), allocatable :: nth_im
          real(kind=dp), dimension(:), allocatable :: nph_re
          real(kind=dp), dimension(:), allocatable :: nph_im
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth_re
          !dir$ attributes align : 64 :: nth_im
          !dir$ attributes align : 64 :: nph_re
          !dir$ attributes align : 64 :: nph_im
    end type NMsph_r8_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NMsph_c4_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NMVec_c4_t)        :: nev
          real(kind=sp),    dimension(:), allocatable :: cphi
          real(kind=sp),    dimension(:), allocatable :: cth
          real(kind=sp),    dimension(:), allocatable :: sphi
          real(kind=sp),    dimension(:), allocatable :: sth
          complex(kind=sp), dimension(:), allocatable :: nth
          complex(kind=sp), dimension(:), allocatable :: nph
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth
          !dir$ attributes align : 64 :: nph
     end type NMsph_c4_t


    ! Formula (2-36)
    ! 'N'-electric and 'N'-magnetic vector functions of fields voltage
    ! in spherical coordinate system.
    type, public NMsph_c8_t
          
          integer(kind=i4)        :: npts
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
          type(NMVec_c8_t)        :: nev
          real(kind=dp),    dimension(:), allocatable :: cphi
          real(kind=dp),    dimension(:), allocatable :: cth
          real(kind=dp),    dimension(:), allocatable :: sphi
          real(kind=dp),    dimension(:), allocatable :: sth
          complex(kind=dp), dimension(:), allocatable :: nth
          complex(kind=dp), dimension(:), allocatable :: nph
          !dir$ attributes align : 64 :: cphi
          !dir$ attributes align : 64 :: cth
          !dir$ attributes align : 64 :: sphi
          !dir$ attributes align : 64 :: sth
          !dir$ attributes align : 64 :: nth
          !dir$ attributes align : 64 :: nph
     end type NMsph_c8_t


     ! Formula (2-27)
     ! Far field (zone) 'electric current' for R>=2*D^2/gamma
     type, public :: FFEC_r4_t

           integer(kind=i4)          :: npts
           real(kind=sp)             :: if_re
           real(kind=sp)             :: if_im
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
           type(EIKR_r4_t)           :: eikr
           type(NEsph_r4_t)          :: nsph
           type(NEVec_r4_t)          :: nev
           real(kind=sp), dimension(:), allocatable :: e_r  ! r     -unit vector spherical
           real(kind=sp), dimension(:), allocatable :: e_th ! theta -unit vector spherical
           real(kind=sp), dimension(:), allocatable :: e_ph ! phi   -unit vector spherical
           real(kind=sp), dimension(:), allocatable :: ffec_xr
           real(kind=sp), dimension(:), allocatable :: ffec_xi
           real(kind=sp), dimension(:), allocatable :: ffec_yr
           real(kind=sp), dimension(:), allocatable :: ffec_yi
           real(kind=sp), dimension(:), allocatable :: ffec_zr
           real(kind=sp), dimension(:), allocatable :: ffec_zi
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffec_xr
           !dir$ attributes align : 64 :: ffec_xi
           !dir$ attributes align : 64 :: ffec_yr
           !dir$ attributes align : 64 :: ffec_yi
           !dir$ attributes align : 64 :: ffec_zr
           !dir$ attributes align : 64 :: ffec_zi      
     end type FFEC_r4_t


     ! Formula (2-27)
     ! Far field (zone) 'electric current' for R>=2*D^2/gamma
     type, public :: FFEC_r8_t

           integer(kind=i4)          :: npts
           real(kind=dp)             :: if_re
           real(kind=dp)             :: if_im
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif
           type(EIKR_r8_t)           :: eikr
           type(NEsph_r8_t)          :: nsph
           type(NEVec_r8_t)          :: nev
           real(kind=dp), dimension(:), allocatable :: e_r  ! r     -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: e_th ! theta -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: e_ph ! phi   -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: ffec_xr
           real(kind=dp), dimension(:), allocatable :: ffec_xi
           real(kind=dp), dimension(:), allocatable :: ffec_yr
           real(kind=dp), dimension(:), allocatable :: ffec_yi
           real(kind=dp), dimension(:), allocatable :: ffec_zr
           real(kind=dp), dimension(:), allocatable :: ffec_zi
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffec_xr
           !dir$ attributes align : 64 :: ffec_xi
           !dir$ attributes align : 64 :: ffec_yr
           !dir$ attributes align : 64 :: ffec_yi
           !dir$ attributes align : 64 :: ffec_zr
           !dir$ attributes align : 64 :: ffec_zi      
     end type FFEC_r8_t


     ! Formula (2-27)
     ! Far field (zone) 'electric current' for R>=2*D^2/gamma
     type, public :: FFEC_c4_t

           integer(kind=i4)          :: npts
           complex(kind=sp)          :: ifac
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
           type(EIKR_c4_t)           :: eikr
           type(NEsph_c4_t)          :: nsph
           type(NEVec_c4_t)          :: nev
           real(kind=sp), dimension(:), allocatable    :: e_r  ! r     -unit vector spherical
           real(kind=sp), dimension(:), allocatable    :: e_th ! theta -unit vector spherical
           real(kind=sp), dimension(:), allocatable    :: e_ph ! phi   -unit vector spherical
           complex(kind=sp), dimension(:), allocatable :: ffec_x
           complex(kind=sp), dimension(:), allocatable :: ffec_y
           complex(kind=sp), dimension(:), allocatable :: ffec_z
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffec_x
           !dir$ attributes align : 64 :: ffec_y
           !dir$ attributes align : 64 :: ffec_z
      end type FFEC_c4_t


     ! Formula (2-27)
     ! Far field (zone) 'electric current' for R>=2*D^2/gamma
     type, public :: FFEC_c8_t

           integer(kind=i4)          :: npts
           complex(kind=dp)          :: ifac
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif
           type(EIKR_c8_t)           :: eikr
           type(NEsph_c8_t)          :: nsph
           type(NEVec_c8_t)          :: nev
           real(kind=dp), dimension(:),    allocatable    :: e_r  ! r     -unit vector spherical
           real(kind=dp), dimension(:),    allocatable    :: e_th ! theta -unit vector spherical
           real(kind=dp), dimension(:),    allocatable    :: e_ph ! phi   -unit vector spherical
           complex(kind=dp), dimension(:), allocatable    :: ffec_x
           complex(kind=dp), dimension(:), allocatable    :: ffec_y
           complex(kind=dp), dimension(:), allocatable    :: ffec_z
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffec_x
           !dir$ attributes align : 64 :: ffec_y
           !dir$ attributes align : 64 :: ffec_z
      end type FFEC_c8_t


       ! Formula (2-27)
     ! Far field (zone) 'magnetic current' for R>=2*D^2/gamma
     type, public :: FFMC_r4_t

           integer(kind=i4)          :: npts
           real(kind=sp)             :: if_re
           real(kind=sp)             :: if_im
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
           type(EIKR_r4_t)           :: eikr
           type(NMsph_r4_t)          :: nmsp
           type(NMVec_r4_t)          :: nmv
           real(kind=sp), dimension(:),    allocatable :: e_r  ! r     -unit vector spherical
           real(kind=sp), dimension(:),    allocatable :: e_th ! theta -unit vector spherical
           real(kind=sp), dimension(:),    allocatable :: e_ph ! phi   -unit vector spherical
           real(kind=sp), dimension(:),    allocatable :: ffmc_xr
           real(kind=sp), dimension(:),    allocatable :: ffmc_xi
           real(kind=sp), dimension(:),    allocatable :: ffmc_yr
           real(kind=sp), dimension(:),    allocatable :: ffmc_yi
           real(kind=sp), dimension(:),    allocatable :: ffmc_zr
           real(kind=sp), dimension(:),    allocatable :: ffmc_zi
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffmc_xr
           !dir$ attributes align : 64 :: ffmc_xi
           !dir$ attributes align : 64 :: ffmc_yr
           !dir$ attributes align : 64 :: ffmc_yi
           !dir$ attributes align : 64 :: ffmc_zr
           !dir$ attributes align : 64 :: ffmc_zi      
     end type FFMC_r4_t


     ! Formula (2-27)
     ! Far field (zone) 'magnetic current' for R>=2*D^2/gamma
     type, public :: FFMC_r8_t

           integer(kind=i4)          :: npts
           real(kind=dp)             :: if_re
           real(kind=dp)             :: if_im
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif
           type(EIKR_r8_t)           :: eikr
           type(NMsph_r8_t)          :: nmsp
           type(NMVec_r8_t)          :: nmv
           real(kind=dp), dimension(:), allocatable :: e_r  ! r     -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: e_th ! theta -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: e_ph ! phi   -unit vector spherical
           real(kind=dp), dimension(:), allocatable :: ffmc_xr
           real(kind=dp), dimension(:), allocatable :: ffmc_xi
           real(kind=dp), dimension(:), allocatable :: ffmc_yr
           real(kind=dp), dimension(:), allocatable :: ffmc_yi
           real(kind=dp), dimension(:), allocatable :: ffmc_zr
           real(kind=dp), dimension(:), allocatable :: ffmc_zi
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffmc_xr
           !dir$ attributes align : 64 :: ffmc_xi
           !dir$ attributes align : 64 :: ffmc_yr
           !dir$ attributes align : 64 :: ffmc_yi
           !dir$ attributes align : 64 :: ffmc_zr
           !dir$ attributes align : 64 :: ffmc_zi      
     end type FFMC_r8_t


     ! Formula (2-27)
     ! Far field (zone) 'magnetic current' for R>=2*D^2/gamma
     type, public :: FFMC_c4_t

           integer(kind=i4)          :: npts
           complex(kind=sp)          :: ifac
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
           type(EIKR_c4_t)           :: eikr
           type(NMsph_c4_t)          :: mmsp
           type(NMVec_c4_t)          :: nmv
           real(kind=dp), dimension(:),    allocatable :: e_r  ! r     -unit vector spherical
           real(kind=dp), dimension(:),    allocatable :: e_th ! theta -unit vector spherical
           real(kind=dp), dimension(:),    allocatable :: e_ph ! phi   -unit vector spherical
           complex(kind=sp), dimension(:), allocatable :: ffmc_x
           complex(kind=sp), dimension(:), allocatable :: ffmc_y
           complex(kind=sp), dimension(:), allocatable :: ffmc_z
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffmc_x
           !dir$ attributes align : 64 :: ffmc_y
           !dir$ attributes align : 64 :: ffmc_z
      end type FFMC_c4_t


     ! Formula (2-27)
     ! Far field (zone) 'magnetic current' for R>=2*D^2/gamma
     type, public :: FFMC_c8_t

           integer(kind=i4)          :: npts
           complex(kind=dp)          :: ifac
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif
           type(EIKR_c8_t)           :: eikr
           type(NMsph_c8_t)          :: nmsp
           type(NMVec_c8_t)          :: nmv
           real(kind=dp), dimension(:),    allocatable :: e_r  ! r     -unit vector spherical
           real(kind=dp), dimension(:),    allocatable :: e_th ! theta -unit vector spherical
           real(kind=dp), dimension(:),    allocatable :: e_ph ! phi   -unit vector spherical
           complex(kind=dp), dimension(:), allocatable :: ffmc_x
           complex(kind=dp), dimension(:), allocatable :: ffmc_y
           complex(kind=dp), dimension(:), allocatable :: ffmc_z
           !dir$ attributes align : 64 :: e_r
           !dir$ attributes align : 64 :: e_th
           !dir$ attributes align : 64 :: e_ph
           !dir$ attributes align : 64 :: ffmc_x
           !dir$ attributes align : 64 :: ffmc_y
           !dir$ attributes align : 64 :: ffmc_z
      end type FFMC_c8_t


      ! Formula (2-38)
      type, public :: poynting_avg_r4_t

            integer(kind=i4)         :: nth
            integer(kind=i4)         :: nphi
            real(kind=sp)            :: k2
            real(kind=sp)            :: eps_r
            real(kind=sp)            :: eps_i
            real(kind=sp)            :: mu_r
            real(kind=sp)            :: mu_i
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
            type(NMsph_r4_t)         :: nmsp
            type(NEsph_r4_t)         :: nesp
            real(kind=sp), dimension(:), allocatable :: R2
            real(kind=sp), dimension(:), allocatable :: e_r
            real(kind=sp), dimension(:), allocatable :: S_re 
            real(kind=sp), dimension(:), allocatable :: S_im 
            !dir$ attributes align : 64 :: R2
            !dir$ attributes align : 64 :: e_r
            !dir$ attributes align : 64 :: S_re
            !dir$ attributes align : 64 :: S_im      
      end type poynting_avg_r4_t


       ! Formula (2-38)
      type, public :: poynting_avg_r8_t

            integer(kind=i4)         :: nth
            integer(kind=i4)         :: nphi
            real(kind=dp)            :: k2
            real(kind=dp)            :: eps_r
            real(kind=dp)            :: eps_i
            real(kind=dp)            :: mu_r
            real(kind=dp)            :: mu_i
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif
            type(NMsph_r8_t)         :: nmsp
            type(NEsph_r8_t)         :: nesp
            real(kind=dp), dimension(:), allocatable :: R2
            real(kind=dp), dimension(:), allocatable :: e_r
            real(kind=dp), dimension(:), allocatable :: S_re 
            real(kind=dp), dimension(:), allocatable :: S_im 
            !dir$ attributes align : 64 :: R2
            !dir$ attributes align : 64 :: e_r
            !dir$ attributes align : 64 :: S_re
            !dir$ attributes align : 64 :: S_im      
      end type poynting_avg_r8_t


        ! Formula (2-38)
      type, public :: poynting_avg_c4_t

            integer(kind=i4)         :: nth
            integer(kind=i4)         :: nphi
            real(kind=sp)            :: k2
            complex(kind=sp)         :: eps
            complex(kind=sp)         :: mu_r
           
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
            type(NMsph_c4_t)         :: nmsp
            type(NEsph_c4_t)         :: nesp
            real(kind=sp),    dimension(:), allocatable :: R2
            real(kind=sp),    dimension(:), allocatable :: e_r
            complex(kind=sp), dimension(:), allocatable :: S
            !dir$ attributes align : 64 :: R2
            !dir$ attributes align : 64 :: e_r
            !dir$ attributes align : 64 :: S
       end type poynting_avg_r4_t


       ! Formula (2-38)
      type, public :: poynting_avg_c8_t

            integer(kind=i4)         :: nth
            integer(kind=i4)         :: nphi
            real(kind=dp)            :: k2
            complex(kind=dp)         :: eps
            complex(kind=dp)         :: mu
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif
            type(NMsph_c8_t)         :: nmsp
            type(NEsph_c8_t)         :: nesp
            real(kind=dp),    dimension(:), allocatable :: R2
            real(kind=dp),    dimension(:), allocatable :: e_r
            complex(kind=dp), dimension(:), allocatable :: S
            !dir$ attributes align : 64 :: R2
            !dir$ attributes align : 64 :: e_r
            !dir$ attributes align : 64 :: S
      end type poynting_avg_c8_t


      ! Formula (2-37)
      ! Intensity of radiation (steradian angle) in directions: phi,theta
      type, public :: Rad_power_r4_t

            type(poynting_avg_r4_t) :: pavg
            real(kind=sp), dimension(:), allocatable :: P_re
            real(kind=sp), dimension(:), allocatable :: P_im
            !dir$ attributes align : 64 :: P_re
            !dir$ attributes align : 64 :: P_im
      end type Rad_power_r4_t


      ! Formula (2-37)
      ! Intensity of radiation (steradian angle) in directions: phi,theta
      type, public :: Rad_power_r8_t

            type(poynting_avg_r8_t) :: pavg
            real(kind=dp), dimension(:), allocatable :: P_re
            real(kind=dp), dimension(:), allocatable :: P_im
            !dir$ attributes align : 64 :: P_re
            !dir$ attributes align : 64 :: P_im
      end type Rad_power_r8_t


      ! Formula (2-37)
      ! Intensity of radiation (steradian angle) in directions: phi,theta
      type, public :: Rad_power_c4_t

            type(poynting_avg_c4_t) :: pavg
            complex(kind=sp), dimension(:), allocatable :: P
            !dir$ attributes align : 64 :: P
      end type Rad_power_c4_t


      ! Formula (2-37)
      ! Intensity of radiation (steradian angle) in directions: phi,theta
      type, public :: Rad_power_c8_t

            type(poynting_avg_c8_t) :: pavg
            complex(kind=dp), dimension(:), allocatable :: P
            !dir$ attributes align : 64 :: P
      end type Rad_power_c8_t


      ! Formula (2-39)
      ! Normalized radiation pattern expressed by the ratio
      ! of poynting vectors.
      type, public :: RP_normalized_r4_t
            
            type(poynting_avg_r4_t)  :: pv
            type(poynting_avg_r4_t)  :: pv_max
            real(kind=sp), dimension(:), allocatable :: psi_re
            real(kind=sp), dimension(:), allocatable :: psi_im
            !dir$ attributes align : 64 :: psi_re
            !dir$ attributes align : 64 :: psi_im
      end type RP_normalized_r4_t


      ! Formula (2-39)
      ! Normalized radiation pattern expressed by the ratio
      ! of poynting vectors.
      type, public :: RP_normalized_r8_t
            
            type(poynting_avg_r8_t)  :: pv
            type(poynting_avg_r8_t)  :: pv_max
            real(kind=dp), dimension(:), allocatable :: psi_re
            real(kind=dp), dimension(:), allocatable :: psi_im
            !dir$ attributes align : 64 :: psi_re
            !dir$ attributes align : 64 :: psi_im
      end type RP_normalized_r8_t


      ! Formula (2-39)
      ! Normalized radiation pattern expressed by the ratio
      ! of poynting vectors.
      type, public :: RP_normalized_c4_t
            
            type(poynting_avg_c4_t)  :: pv
            type(poynting_avg_c4_t)  :: pv_max
            complex(kind=sp), dimension(:), allocatable :: psi_re
            !dir$ attributes align : 64 :: psi_re
      end type RP_normalized_c4_t


      ! Formula (2-39)
      ! Normalized radiation pattern expressed by the ratio
      ! of poynting vectors.
      type, public :: RP_normalized_c8_t
            
            type(poynting_avg_c8_t)  :: pv
            type(poynting_avg_c8_t)  :: pv_max
            complex(kind=dp), dimension(:), allocatable :: psi_re
            !dir$ attributes align : 64 :: psi_re
      end type RP_normalized_c8_t


      ! Formula (2-43)
      ! The current distribution along the 'z' coordiante
      type, public :: f243_r4_t

            integer(kind=i4)        :: npts
            real(kind=sp)           :: I0
            real(kind=sp)           :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
            real(kind=sp), dimension(:), allocatable ::  Iz
            !dir$ attributes align : 64 :: Iz
      end type f243_r4_t


      ! Formula (2-43)
      ! The current distribution along the 'z' coordiante
      type, public :: f243_r8_t

            integer(kind=i4)        :: npts
            real(kind=dp)           :: I0
            real(kind=dp)           :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif
            real(kind=dp), dimension(:), allocatable ::  Iz
            !dir$ attributes align : 64 :: Iz
      end type f243_r8_t


      ! Formula (2-44)
      ! Radiation pattern of thin wire (odd m)
      type, public :: f244_r4_t

            integer(kind=i4)      :: npts
            real(kind=sp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,24)
#endif
            real(kind=sp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f244_r4_t

      
      ! Formula (2-44)
      ! Radiation pattern of thin wire (odd m)
      type, public :: f244_r8_t

            integer(kind=i4)      :: npts
            real(kind=dp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
            real(kind=dp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f244_r8_t


      ! Formula (2-45)
      ! Radiation pattern of thin wire (even m)
      type, public :: f245_r4_t

            integer(kind=i4)      :: npts
            real(kind=sp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,24)
#endif
            real(kind=sp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f245_r4_t

      
      ! Formula (2-45)
      ! Radiation pattern of thin wire (even m)
      type, public :: f245_r8_t

            integer(kind=i4)      :: npts
            real(kind=dp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,20)
#endif
            real(kind=dp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f245_r8_t


      ! Formula (2-48)
      ! For a wire with current modulated by phase speed
      type, public :: f248_r4_t
            
            integer(kind=i4)      :: nz
            real(kind=sp)         :: I0_re
            real(kind=sp)         :: I0_im
            real(kind=sp)         :: beta  ! freq/c
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif
            real(kind=sp), dimension(:), allocatable :: Iz_re
            real(kind=sp), dimension(:), allocatable :: Iz_im
            !dir$ attributes align : 64 :: Iz_re
            !dir$ attributes align : 64 :: Iz_im
      end type f248_r4_t


      ! Formula (2-48)
      ! For a wire with current modulated by phase speed
      type, public :: f248_r8_t
            
            integer(kind=i4)      :: nz
            real(kind=dp)         :: I0_re
            real(kind=dp)         :: I0_im
            real(kind=dp)         :: beta  ! freq/c
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
            real(kind=dp), dimension(:), allocatable :: Iz_re
            real(kind=dp), dimension(:), allocatable :: Iz_im
            !dir$ attributes align : 64 :: Iz_re
            !dir$ attributes align : 64 :: Iz_im
      end type f248_r8_t


      ! Formula (2-48)
      ! For a wire with current modulated by phase speed
      type, public :: f248_c4_t
            
            integer(kind=i4)      :: nz
            complex(kind=sp)      :: I0
            real(kind=sp)         :: beta  ! freq/c
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif
            complex(kind=sp), dimension(:), allocatable :: Iz
            !dir$ attributes align : 64 :: Iz
      end type f248_c4_t


      ! Formula (2-48)
      ! For a wire with current modulated by phase speed
      type, public :: f248_c8_t
            
            integer(kind=i4)      :: nz
            complex(kind=dp)      :: I0
            real(kind=dp)         :: beta  ! freq/c
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif
            complex(kind=dp), dimension(:), allocatable :: Iz
            !dir$ attributes align : 64 :: Iz
      end type f248_c8_t


      ! Formula (2-46)
      ! Symmetric sinusoidal current distribution.
      type, public :: f246_r4_t
            
            integer(kind=i4)      :: nz
            real(kind=sp)         :: I0_re
            real(kind=sp)         :: I0_im
            real(kind=sp)         :: L
            real(kind=sp)         :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif            
            real(kind=sp), dimension(:), allocatable :: Iz_re
            real(kind=sp), dimension(:), allocatable :: Iz_im
            !dir$ attributes align : 64 :: Iz_re
            !dir$ attributes align : 64 :: Iz_im
      end type f246_r4_t


      ! Formula (2-46)
      ! Symmetric sinusoidal current distribution.
      type, public :: f246_r8_t
            
            integer(kind=i4)      :: nz
            real(kind=dp)         :: I0_re
            real(kind=dp)         :: I0_im
            real(kind=dp)         :: L
            real(kind=dp)         :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,28)
#endif            
            real(kind=dp), dimension(:), allocatable :: Iz_re
            real(kind=dp), dimension(:), allocatable :: Iz_im
            !dir$ attributes align : 64 :: Iz_re
            !dir$ attributes align : 64 :: Iz_im
      end type f246_r8_t


      ! Formula (2-46)
      ! Symmetric sinusoidal current distribution.
      type, public :: f246_c4_t
            
            integer(kind=i4)      :: nz
            complex(kind=sp)      :: I0
            real(kind=sp)         :: L
            real(kind=sp)         :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif            
            complex(kind=sp), dimension(:), allocatable :: Iz
            !dir$ attributes align : 64 :: Iz
            
      end type f246_c4_t


      ! Formula (2-46)
      ! Symmetric sinusoidal current distribution.
      type, public :: f246_c8_t
            
            integer(kind=i4)      :: nz
            complex(kind=dp)      :: I0
            real(kind=dp)         :: L
            real(kind=dp)         :: k
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,28)
#endif            
            complex(kind=dp), dimension(:), allocatable :: Iz
            !dir$ attributes align : 64 :: Iz
      end type f246_c8_t


      ! Formula (2-47)
      ! Wire symmetric current radiation pattern
      type, public :: f247_r4_t
            
            integer(kind=i4)      :: nth
            real(kind=sp)         :: k
            real(kind=sp)         :: L
            real(kind=sp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,16)
#endif  
            real(kind=sp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f247_r4_t


      ! Formula (2-47)
      ! Wire symmetric current radiation pattern
      type, public :: f247_r8_t
            
            integer(kind=i4)      :: nth
            real(kind=dp)         :: k
            real(kind=dp)         :: L
            real(kind=dp)         :: A
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif  
            real(kind=dp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f247_r8_t


      ! Formula (2-49)
      ! Wire (running-wave) radiation pattern
      type, public :: f249_r4_t

            integer(kind=i4)      :: nth
            real(kind=sp)         :: k
            real(kind=sp)         :: L
            real(kind=sp)         :: A
            real(kind=sp)         :: beta
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif  
            real(kind=sp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f249_r4_t


      ! Formula (2-49)
      ! Wire (running-wave) radiation pattern
      type, public :: f249_r8_t

            integer(kind=i4)      :: nth
            real(kind=dp)         :: k
            real(kind=dp)         :: L
            real(kind=dp)         :: A
            real(kind=dp)         :: beta
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,28)
#endif  
            real(kind=dp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth
      end type f249_r8_t


      ! Formula (2-62)
      ! Flat 2D antenna radiating slot (Integral of electric field)
      ! x-component only
      type, public :: f262_r4_t

            integer(kind=i4)      :: nx
            integer(kind=i4)      :: ny
            integer(kind=i4)      :: nphi
            integer(kind=i4)      :: ntht
            real(kind=sp)         :: k
            real(kind=sp)         :: A
            real(kind=sp)         :: B
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,4)
#endif           
            real(kind=sp), dimension(:), allocatable :: Ex_re
            real(kind=sp), dimension(:), allocatable :: Ex_im
            real(kind=sp), dimension(:), allocatable :: Ey_re
            real(kind=sp), dimension(:), allocatable :: Ey_im
            real(kind=sp), dimension(:), allocatable :: N_re
            real(kind=sp), dimension(:), allocatable :: N_im  
            !dir$ attributes align : 64 :: Ex_re
            !dir$ attributes align : 64 :: Ex_im
            !dir$ attributes align : 64 :: Ey_re
            !dir$ attributes align : 64 :: Ey_im
            !dir$ attributes align : 64 :: N_re
            !dir$ attributes align : 64 :: N_im
      end type f262_r4_t


      ! Formula (2-62)
      ! Flat 2D antenna radiating slot (Integral of electric field)
      ! x-component only
      type, public :: f262_r8_t

            integer(kind=i4)      :: nx
            integer(kind=i4)      :: ny
            integer(kind=i4)      :: nphi
            integer(kind=i4)      :: ntht
            real(kind=dp)         :: k
            real(kind=dp)         :: A
            real(kind=dp)         :: B
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,24)
#endif           
            real(kind=dp), dimension(:), allocatable :: Ex_re
            real(kind=dp), dimension(:), allocatable :: Ex_im
            real(kind=dp), dimension(:), allocatable :: Ey_re
            real(kind=dp), dimension(:), allocatable :: Ey_im
            real(kind=dp), dimension(:), allocatable :: N_re
            real(kind=dp), dimension(:), allocatable :: N_im  
            !dir$ attributes align : 64 :: Ex_re
            !dir$ attributes align : 64 :: Ex_im
            !dir$ attributes align : 64 :: Ey_re
            !dir$ attributes align : 64 :: Ey_im
            !dir$ attributes align : 64 :: N_re
            !dir$ attributes align : 64 :: N_im
      end type f262_r8_t


      ! Formula (2-62)
      ! Flat 2D antenna radiating slot (Integral of electric field)
      ! x-component only
      type, public :: f262_c4_t

            integer(kind=i4)      :: nx
            integer(kind=i4)      :: ny
            integer(kind=i4)      :: nphi
            integer(kind=i4)      :: ntht
            real(kind=sp)         :: k
            real(kind=sp)         :: A
            real(kind=sp)         :: B
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,4)
#endif           
            complex(kind=sp), dimension(:), allocatable :: Ex
            complex(kind=sp), dimension(:), allocatable :: Ey
            complex(kind=sp), dimension(:), allocatable :: N
            !dir$ attributes align : 64 :: Ex
            !dir$ attributes align : 64 :: Ey
            !dir$ attributes align : 64 :: Ey
            !dir$ attributes align : 64 :: N
       end type f262_c4_t


      ! Formula (2-62)
      ! Flat 2D antenna radiating slot (Integral of electric field)
      ! x-component only
      type, public :: f262_c8_t

            integer(kind=i4)      :: nx
            integer(kind=i4)      :: ny
            integer(kind=i4)      :: nphi
            integer(kind=i4)      :: ntht
            real(kind=dp)         :: k
            real(kind=dp)         :: A
            real(kind=dp)         :: B
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,24)
#endif           
            complex(kind=dp), dimension(:), allocatable :: Ex
            complex(kind=dp), dimension(:), allocatable :: Ey
            complex(kind=dp), dimension(:), allocatable :: N
            !dir$ attributes align : 64 :: Ex
            !dir$ attributes align : 64 :: Ey
            !dir$ attributes align : 64 :: N
       end type f262_c8_t


       ! Formula (2-65)
       type, public :: f265_r4_t
             
             integer(kind=i4)      :: nth
             integer(kind=i4)      :: nx
             real(kind=sp)         :: A
             real(kind=sp)         :: L
             real(kind=sp)         :: gamm
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,12)
#endif   
             real(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f265_r4_t


       ! Formula (2-65)
       type, public :: f268_r8_t
             
             integer(kind=i4)      :: nth
             integer(kind=i4)      :: nx
             real(kind=dp)         :: A
             real(kind=dp)         :: L
             real(kind=dp)         :: gamm
             real(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f268_r8_t


       
       ! Formula (2-67)
       ! Main lobe width
       type, public :: f267_r4_t
 
             integer(kind=i4)      :: nth
             integer(kind=i4)      :: nx
             real(kind=sp)         :: A
             real(kind=sp)         :: gamm
             real(kind=sp)         :: L
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,12)
#endif 
             real(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f267_r4_t


       ! Formula (2-67)
       ! Main lobe width
       type, public :: f267_r8_t

             integer(kind=i4)      :: nth
             integer(kind=i4)      :: nx
             real(kind=dp)         :: A
             real(kind=dp)         :: gamm
             real(kind=dp)         :: L
             real(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f267_r8_t


       ! Formula (2-68)
       ! Normalized radiation pattern.
       type, public :: f268_r4_t

             integer(kind=i4)      :: nx
             integer(kind=i4)      :: nth
             real(kind=sp)         :: A
             real(kind=sp)         :: gamm
             real(kind=sp)         :: L
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,12)
#endif 
             real(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth 
       end type f268_r4_t


       ! Formula (2-68)
       ! Normalized radiation pattern.
       type, public :: f268_r8_t

             integer(kind=i4)      :: nx
             integer(kind=i4)      :: nth
             real(kind=dp)         :: A
             real(kind=dp)         :: gamm
             real(kind=dp)         :: L
             real(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth 
       end type f268_r8_t


       ! Formula (2-71)
       ! The area of homogenous sidelobs
       type, public :: f271_r4_t

             integer(kind=i4)   :: m
             integer(kind=i4)   :: n
             integer(kind=i4)   :: nth
             integer(kind=i4)   :: nx
             real(kind=sp)      :: gamm
             real(kind=sp)      :: L
             real(kind=sp)      :: A
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,4)
#endif
            real(kind=sp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth  
       end type f271_r4_t


        ! Formula (2-71)
       ! The area of homogenous sidelobs
       type, public :: f271_r8_t

             integer(kind=i4)   :: m
             integer(kind=i4)   :: n
             integer(kind=i4)   :: nth
             integer(kind=i4)   :: nx
             real(kind=dp)      :: gamm
             real(kind=dp)      :: L
             real(kind=dp)      :: A
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,24)
#endif
            real(kind=dp), dimension(:), allocatable :: Fth
            !dir$ attributes align : 64 :: Fth  
       end type f271_r8_t


       ! Formula (2-74)
       ! The antenna current distribution for creating
       ! the radiation pattern (2-71)
       ! Changed the naming pattern.
       type, public :: f274_r4_t

             integer(kind=i4)        :: nx
             integer(kind=i4)        :: m
             integer(kind=i4)        :: n
             real(kind=sp)           :: L
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,16)
#endif
             real(kind=sp), dimension(:), allocatable :: F0mM
             real(kind=dp), dimension(:), allocatable :: FnmM
             real(kind=sp), dimension(:), allocatable :: f2xL
             !dir$ attributes align : 64 :: F0mM
             !dir$ attributes align : 64 :: FnmM
             !dir$ attributes align : 64 :: f2xL
       end type f274_r4_t


       ! Formula (2-74)
       ! The antenna current distribution for creating
       ! the radiation pattern (2-71)
       type, public :: f274_r8_t

             integer(kind=i4)        :: nx
             integer(kind=i4)        :: m
             integer(kind=i4)        :: n
             real(kind=dp)           :: L
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,12)
#endif
             real(kind=dp), dimension(:), allocatable :: F0mM
             real(kind=dp), dimension(:), allocatable :: FnmM
             real(kind=dp), dimension(:), allocatable :: f2xL
             !dir$ attributes align : 64 :: F0mM
             !dir$ attributes align : 64 :: FnmM
             !dir$ attributes align : 64 :: f2xL
       end type f274_r8_t


       ! Formula (2-78)
       ! The antenna current distribution for creating
       ! the radiation pattern (2-75)
       
       type, public :: f278_r4_t
     
             integer(kind=i4)       :: nx
             real(kind=sp)          :: L
             real(kind=sp)          :: A
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,20)
#endif
             real(kind=sp), dimension(:), allocatable :: fx
             !dir$ attributes align : 64 :: fx
       end type f278_r4_t


       ! Formula (2-78)
       ! The antenna current distribution for creating
       ! the radiation pattern (2-75)
       
       type, public :: f278_r8_t
     
             integer(kind=i4)       :: nx
             real(kind=dp)          :: L
             real(kind=dp)          :: A
#if (USE_STRUCT_PADDING) == 1
            STRUCT_PADDING(0,12)
#endif
             real(kind=dp), dimension(:), allocatable :: fx
             !dir$ attributes align : 64 :: fx
       end type f278_r8_t


       ! Formula (2-79)
       ! Constant and squared-cosine distribution.
       type, public :: f279_r4_t

             integer(kind=i4)       :: nx
             real(kind=sp)          :: C
             real(kind=sp)          :: initx
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,20)
#endif       
             real(kind=sp), dimension(:), allocatable :: fx
             !dir$ attributes align : 64 :: fx      
       end type f279_r4_t


       ! Formula (2-79)
       ! Constant and squared-cosine distribution.
       type, public :: f279_r8_t

             integer(kind=i4)       :: nx
             real(kind=dp)          :: C
             real(kind=dp)          :: initx
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,12)
#endif       
             real(kind=dp), dimension(:), allocatable :: fx
             !dir$ attributes align : 64 :: fx      
       end type f279_r8_t


       ! Formula (2-80)
       ! Radiation pattern of (2-79)
       type, public :: f280_r4_t

             integer(kind=i4)        :: nth
             real(kind=sp)           :: L
             real(kind=sp)           :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,20)
#endif  
             real(kind=sp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f280_r4_t


       ! Formula (2-80)
       ! Radiation pattern of (2-79)
       type, public :: f280_r8_t

             integer(kind=i4)        :: nth
             real(kind=dp)           :: L
             real(kind=dp)           :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,12)
#endif  
             real(kind=dp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f280_r8_t


       ! Formula (2-83)
       ! Circular slit radiation pattern (for each point of observation i.e. theta,phi do:)
       type, public :: f283_r4_t

             integer(kind=i4)        :: nth1
             integer(kind=i4)        :: nph1
             integer(kind=i4)        :: nph2
             integer(kind=i4)        :: nr
             real(kind=sp)           :: k
             real(kind=sp)           :: iph1  ! starting values
             real(kind=sp)           :: iph2  ! starting values
             real(kind=sp)           :: ir    ! starting values
             real(kind=sp)           :: ith   ! starting values
             real(kind=sp)           :: R     ! slit diameter
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             real(kind=sp), dimension(:), allocatable :: Ex_re 
             real(kind=sp), dimension(:), allocatable :: Ex_im
             real(kind=sp), dimension(:), allocatable :: Nx_re
             real(kind=sp), dimension(:), allocatable :: Nx_im
             !dir$ attributes align : 64 :: Ex_re
             !dir$ attributes align : 64 :: Ex_im
             !dir$ attributes align : 64 :: Nx_re
             !dir$ attributes align : 64 :: Nx_im
       end type f283_r4_t


          ! Formula (2-83)
       ! Circular slit radiation pattern (for each point of observation i.e. theta,phi do:)
       type, public :: f283_r8_t

             integer(kind=i4)        :: nth1
             integer(kind=i4)        :: nph1
             integer(kind=i4)        :: nph2
             integer(kind=i4)        :: nr
             real(kind=dp)           :: k
             real(kind=dp)           :: iph1  ! starting values
             real(kind=dp)           :: iph2  ! starting values
             real(kind=dp)           :: ir    ! starting values
             real(kind=dp)           :: ith   ! starting values
             real(kind=dp)           :: R     ! slit diameter
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,2)
#endif
             real(kind=dp), dimension(:), allocatable :: Ex_re 
             real(kind=dp), dimension(:), allocatable :: Ex_im
             real(kind=dp), dimension(:), allocatable :: Nx_re
             real(kind=dp), dimension(:), allocatable :: Nx_im
             !dir$ attributes align : 64 :: Ex_re
             !dir$ attributes align : 64 :: Ex_im
             !dir$ attributes align : 64 :: Nx_re
             !dir$ attributes align : 64 :: Nx_im
       end type f283_r8_t


       ! Formula (2-83)
       ! Circular slit radiation pattern (for each point of observation i.e. theta,phi do:)
       type, public :: f283_c4_t

             integer(kind=i4)        :: nth1
             integer(kind=i4)        :: nph1
             integer(kind=i4)        :: nph2
             integer(kind=i4)        :: nr
             real(kind=sp)           :: k
             real(kind=sp)           :: iph1  ! starting values
             real(kind=sp)           :: iph2  ! starting values
             real(kind=sp)           :: ir    ! starting values
             real(kind=sp)           :: ith   ! starting values
             real(kind=sp)           :: R     ! slit diameter
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             complex(kind=sp), dimension(:), allocatable :: Ex 
             complex(kind=sp), dimension(:), allocatable :: Nx
             !dir$ attributes align : 64 :: Ex
             !dir$ attributes align : 64 :: Nx
       end type f283_c4_t


          ! Formula (2-83)
       ! Circular slit radiation pattern (for each point of observation i.e. theta,phi do:)
       type, public :: f283_c8_t

             integer(kind=i4)        :: nth1
             integer(kind=i4)        :: nph1
             integer(kind=i4)        :: nph2
             integer(kind=i4)        :: nr
             real(kind=dp)           :: k
             real(kind=dp)           :: iph1  ! starting values
             real(kind=dp)           :: iph2  ! starting values
             real(kind=dp)           :: ir    ! starting values
             real(kind=dp)           :: ith   ! starting values
             real(kind=dp)           :: R     ! slit diameter
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,2)
#endif
             complex(kind=dp), dimension(:), allocatable :: Ex
             complex(kind=dp), dimension(:), allocatable :: Nx
             !dir$ attributes align : 64 :: Ex
             !dir$ attributes align : 64 :: Nx
       end type f283_c8_t


       ! Formula (2-84)
       ! The current distribution for (2-83)
       type, public :: f284_r4_t

             integer(kind=i4)        :: nr
             real(kind=sp)           :: ir
             real(kind=sp)           :: D
             real(kind=sp)           :: delta
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,16)
#endif
             real(kind=sp), dimension(:), allocatable :: fp
             !dir$ attributes align : 64 :: fp
       end type f284_r4_t


       ! Formula (2-84)
       ! The current distribution for (2-83)
       type, public :: f284_r8_t

             integer(kind=i4)        :: nr
             real(kind=dp)           :: ir
             real(kind=dp)           :: D
             real(kind=dp)           :: delta
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,4)
#endif
             real(kind=dp), dimension(:), allocatable :: fp
             !dir$ attributes align : 64 :: fp
       end type f284_r8_t


       ! Formula (2-85)
       ! The radiation pattern of circular synch-phase slit
       ! for the current distribution (2-84)
       type, public :: f285_r4_t

             integer(kind=i4)        :: nr
             integer(kind=i4)        :: nth
             real(kind=sp)           :: ir
             real(kind=sp)           :: D
             real(kind=sp)           :: delta
             real(kind=sp)           :: R0
             real(kind=sp)           :: k
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,4)
#endif
             real(kind=sp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f285_r4_t


       ! Formula (2-85)
       ! The radiation pattern of circular synch-phase slit
       ! for the current distribution (2-84)
       type, public :: f285_r8_t

             integer(kind=i4)        :: nr
             integer(kind=i4)        :: nth
             real(kind=dp)           :: ir
             real(kind=dp)           :: D
             real(kind=dp)           :: delta
             real(kind=dp)           :: R0
             real(kind=dp)           :: k
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,16)
#endif
             real(kind=dp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f285_r8_t


       ! Formula (2-87)
       ! 
       type, public :: f287_r4_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: nph
             real(kind=sp)            :: ir
             real(kind=sp)            :: delta1
             real(kind=sp)            :: delta2
             real(kind=sp)            :: D
             real(kind=sp)            :: iph
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,4)
#endif
             real(kind=sp), dimension(:), allocatable :: fph
             !dir$ attributes align : 64 :: fph
       end type f287_r4_t


       ! Formula (2-87)
       ! 
       type, public :: f287_r8_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: nph
             real(kind=dp)            :: ir
             real(kind=dp)            :: delta1
             real(kind=dp)            :: delta2
             real(kind=dp)            :: D
             real(kind=dp)            :: iph
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,16)
#endif
             real(kind=dp), dimension(:), allocatable :: fph
             !dir$ attributes align : 64 :: fph
       end type f287_r8_t


       ! Formula (2-88), and (2-89)
       ! Radiation pattern for 2-87, iff phi'=0
       type, public :: f2889_r4_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: nth
             real(kind=sp)            :: ir
             real(kind=sp)            :: delta1
             real(kind=sp)            :: delta2
             real(kind=sp)            :: D
             real(kind=sp)            :: k
             real(kind=sp)            :: R0
             real(kind=sp)            :: ith
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,28)
#endif
             real(kind=sp), dimension(:), allocatable :: Fu0
             !dir$ attributes align : 64 :: Fu0
       end type f2889_r4_t


       ! Formula (2-88) and (2-89)
       ! Radiation pattern for 2-87, iff phi'= 0
       type, public :: f2889_r8_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: nth
             real(kind=dp)            :: ir
             real(kind=dp)            :: delta1
             real(kind=dp)            :: delta2
             real(kind=dp)            :: D
             real(kind=dp)            :: k
             real(kind=dp)            :: R0
             real(kind=dp)            :: ith
             real(kind=dp), dimension(:), allocatable :: Fu0
             !dir$ attributes align : 64 :: Fu0
       end type f2889_r8_t


       ! Formula (2-91)
       ! Quasi-optimal radiation pattern
       type, public :: f291_r4_t

             integer(kind=i4)         :: nth
             integer(kind=i4)         :: nx
             integer(kind=i4)         :: m
             integer(kind=i4)         :: n
             real(kind=sp)            :: ith
             real(kind=sp)            :: ix
             real(kind=sp)            :: A
             real(kind=sp)            :: D
             real(kind=sp)            :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,28)
#endif          
             real(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth   
       end type f291_r4_t   


       ! Formula (2-91)
       ! Quasi-optimal radiation pattern
       type, public :: f291_r8_t

             integer(kind=i4)         :: nth
             integer(kind=i4)         :: nx
             integer(kind=i4)         :: m
             integer(kind=i4)         :: n
             real(kind=dp)            :: ith
             real(kind=dp)            :: ix
             real(kind=dp)            :: A
             real(kind=dp)            :: D
             real(kind=dp)            :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,8)
#endif          
             real(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth   
       end type f291_r8_t 


       ! Formula (2-67a)
       ! Radiation patter main lobe width       
       type, public :: f267a_r4_t

             integer(kind=i4)         :: nx
             integer(kind=i4)         :: nth
             real(kind=sp)            :: A
             real(kind=sp)            :: D
             real(kind=sp)            :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,8)
#endif 
             real(kind=sp), dimension(:), allocatable :: twoth
             !dir$ attributes align : 64 :: twoth
       end type f267a_r4_t


       ! Formula (2-67a)
       ! Radiation patter main lobe width       
       type, public :: f267a_r8_t

             integer(kind=i4)         :: nx
             integer(kind=i4)         :: nth
             real(kind=dp)            :: A
             real(kind=dp)            :: D
             real(kind=dp)            :: gamm
             real(kind=dp), dimension(:), allocatable :: twoth
             !dir$ attributes align : 64 :: twoth
       end type f267a_r8_t 


       ! Formula (2-93)
       ! 
       type, public :: f293_r4_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: m
             integer(kind=i4)         :: nth
             real(kind=sp)            :: D
             real(kind=sp)            :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,12)
#endif 
             real(kind=sp), dimension(:), allocatable :: fp
             !dir$ attributes align : 64 :: fp
       end type f293_r4_t


       ! Formula (2-93)
       ! 
       type, public :: f293_r8_t

             integer(kind=i4)         :: nr
             integer(kind=i4)         :: m
             integer(kind=i4)         :: nth
             real(kind=dp)            :: D
             real(kind=dp)            :: gamm
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,4)
#endif 
             real(kind=dp), dimension(:), allocatable :: fp
             !dir$ attributes align : 64 :: fp
       end type f293_r8_t


       ! Formula (2-94)
       ! Fourier series of current distribution of elliptic slit.
       type, public :: f294_r4_t

             integer(kind=i4)          :: nx
             integer(kind=i4)          :: ny
             real(kind=sp)             :: a
             real(kind=sp)             :: b
             real(kind=sp)             :: ir
             real(kind=sp)             :: is
             real(kind=sp)             :: ix
             real(kind=sp)             :: iy
             real(kind=sp), dimension(:,:), allocatable :: fxy
             !dir$ attributes align : 64 :: fxy
       end type f294_r4_t


       ! Formula (2-94)
       ! Fourier series of current distribution of elliptic slit.
       type, public :: f294_r8_t

             integer(kind=i4)          :: nx
             integer(kind=i4)          :: ny
             real(kind=dp)             :: a
             real(kind=dp)             :: b
             real(kind=dp)             :: ir
             real(kind=dp)             :: is
             real(kind=dp)             :: ix
             real(kind=dp)             :: iy
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,8)
#endif 
             real(kind=dp), dimension(:,:), allocatable :: fxy
             !dir$ attributes align : 64 :: fxy
       end type f294_r8_t


       ! Formula (2-95)
       ! Radiation pattern of (2-94)
       type, public :: f295_r4_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nG
             real(kind=sp)             :: a
             real(kind=sp)             :: b
             real(kind=sp)             :: alpha
             real(kind=sp)             :: beta
             real(kind=sp)             :: ith
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,4)
#endif
             real(kind=sp), dimension(32) :: Gu

             real(kind=sp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f295_r4_t


       ! Formula (2-95)
       ! Radiation pattern of (2-94)
       type, public :: f295_r8_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nG
             real(kind=dp)             :: a
             real(kind=dp)             :: b
             real(kind=dp)             :: alpha
             real(kind=dp)             :: beta
             real(kind=dp)             :: ith
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,16)
#endif 
             real(kind=dp), dimension(32) :: Gu
             real(kind=dp), dimension(:), allocatable :: Fu
             !dir$ attributes align : 64 :: Fu
       end type f295_r8_t


       ! Formula (2-100)
       type, public :: f2100_r4_t

             integer(kind=i4)          :: nx
             integer(kind=i4)          :: nth
             real(kind=sp)             :: ix
             real(kind=sp)             :: ith
             real(kind=sp)             :: k
             real(kind=sp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,8)
#endif 
             real(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f2100_r4_t


        ! Formula (2-100)
       type, public :: f2100_r8_t

             integer(kind=i4)          :: nx
             integer(kind=i4)          :: nth
             real(kind=dp)             :: ix
             real(kind=dp)             :: ith
             real(kind=dp)             :: k
             real(kind=dp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif 
             real(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
       end type f2100_r8_t


       ! Formula (2-102)
       ! Radiation pattern of slit of squared phase error.
       type, public :: f2102_r4_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=sp)             :: ith
             real(kind=sp)             :: ir
             real(kind=sp)             :: ix
             real(kind=sp)             :: R
             real(kind=sp)             :: A
             real(kind=sp)             :: k
             real(kind=sp)             :: B
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             real(kind=sp), dimension(:), allocatable :: Fth_re
             real(kind=sp), dimension(:), allocatable :: Fth_im
             !dir$ attributes align : 64 :: Fth_re
             !dir$ attributes align : 64 :: Fth_im
       end type f2102_r4_t


       ! Formula (2-102)
       ! Radiation pattern of slit of squared phase error.
       type, public :: f2102_r8_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=dp)             :: ith
             real(kind=dp)             :: ir
             real(kind=dp)             :: ix
             real(kind=dp)             :: R
             real(kind=dp)             :: A
             real(kind=dp)             :: k
             real(kind=dp)             :: B
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,60)
#endif
             real(kind=dp), dimension(:), allocatable :: Fth_re
             real(kind=dp), dimension(:), allocatable :: Fth_im
             !dir$ attributes align : 64 :: Fth_re
             !dir$ attributes align : 64 :: Fth_im
       end type f2102_r8_t


       ! Formula (2-102)
       ! Radiation pattern of slit of squared phase error.
       type, public :: f2102_c4_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=sp)             :: ith
             real(kind=sp)             :: ir
             real(kind=sp)             :: ix
             real(kind=sp)             :: R
             real(kind=sp)             :: A
             real(kind=sp)             :: k
             real(kind=sp)             :: B
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             complex(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
        end type f2102_c4_t


       ! Formula (2-102)
       ! Radiation pattern of slit of squared phase error.
       type, public :: f2102_c8_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=dp)             :: ith
             real(kind=dp)             :: ir
             real(kind=dp)             :: ix
             real(kind=dp)             :: R
             real(kind=dp)             :: A
             real(kind=dp)             :: k
             real(kind=dp)             :: B
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,60)
#endif
             complex(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
        end type f2102_c8_t


        ! Formula (2-105)
        ! Radiation pattern of slit -- squared phase error and
        ! cosine amplitude distribution.
        type, public :: f2105_r4_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=sp)             :: ith
             real(kind=sp)             :: ir
             real(kind=sp)             :: ix
             real(kind=sp)             :: beta
             real(kind=sp)             :: k
             real(kind=sp)             :: L
             real(kind=sp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             real(kind=sp), dimension(:), allocatable :: Fth_re
             real(kind=sp), dimension(:), allocatable :: Fth_im
             !dir$ attributes align : 64 :: Fth_re
             !dir$ attributes align : 64 :: Fth_im
        end type f2105_r4_t


        ! Formula (2-105)
        ! Radiation pattern of slit -- squared phase error and
        ! cosine amplitude distribution.
        type, public :: f2105_r8_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=dp)             :: ith
             real(kind=dp)             :: ir
             real(kind=dp)             :: ix
             real(kind=dp)             :: beta
             real(kind=dp)             :: k
             real(kind=dp)             :: L
             real(kind=dp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,60)
#endif
             real(kind=dp), dimension(:), allocatable :: Fth_re
             real(kind=dp), dimension(:), allocatable :: Fth_im
             !dir$ attributes align : 64 :: Fth_re
             !dir$ attributes align : 64 :: Fth_im
        end type f2105_r8_t


        ! Formula (2-105)
        ! Radiation pattern of slit -- squared phase error and
        ! cosine amplitude distribution.
        type, public :: f2105_c4_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=sp)             :: ith
             real(kind=sp)             :: ir
             real(kind=sp)             :: ix
             real(kind=sp)             :: beta
             real(kind=sp)             :: k
             real(kind=sp)             :: L
             real(kind=sp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,24)
#endif
             complex(kind=sp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
        end type f2105_c4_t


        ! Formula (2-105)
        ! Radiation pattern of slit -- squared phase error and
        ! cosine amplitude distribution.
        type, public :: f2105_c8_t

             integer(kind=i4)          :: nth
             integer(kind=i4)          :: nr
             integer(kind=i4)          :: nx
             real(kind=dp)             :: ith
             real(kind=dp)             :: ir
             real(kind=dp)             :: ix
             real(kind=dp)             :: beta
             real(kind=dp)             :: k
             real(kind=dp)             :: L
             real(kind=dp)             :: A
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,60)
#endif
             complex(kind=dp), dimension(:), allocatable :: Fth
             !dir$ attributes align : 64 :: Fth
        end type f2105_c8_t


        ! Formula (2-107)
        type, public :: f2107_r4_t

              integer(kind=i4)         :: n
              integer(kind=i4)         :: nth
              integer(kind=i4)         :: nx
              real(kind=sp)            :: ith
              real(kind=sp)            :: ix
              real(kind=sp)            :: A
              real(kind=sp)            :: k
              real(kind=sp)            :: L
              real(kind=sp), dimension(:), allocatable :: Fu0
              real(kind=sp), dimension(:), allocatable :: Fu
              !dir$ attributes align : 64 :: Fu0
              !dir$ attributes align : 64 :: Fu
        end type f2107_r4_t

      
        ! Formula (2-107)
        type, public :: f2107_r8_t

              integer(kind=i4)         :: n
              integer(kind=i4)         :: nth
              integer(kind=i4)         :: nx
              real(kind=dp)            :: ith
              real(kind=dp)            :: ix
              real(kind=dp)            :: A
              real(kind=dp)            :: k
              real(kind=dp)            :: L
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,12)
#endif
              real(kind=dp), dimension(:), allocatable :: Fu0
              real(kind=dp), dimension(:), allocatable :: Fu
              !dir$ attributes align : 64 :: Fu0
              !dir$ attributes align : 64 :: Fu
        end type f2107_r8_t


        ! Formula (2-110)
        ! The averaged radiation pattern of antenna array
        type, public :: f2110_r4_t

              integer(kind=i4)         :: m
              integer(kind=i4)         :: n
              integer(kind=i4)         :: nth
              integer(kind=i4)         :: nph
              real(kind=sp)            :: sig
              real(kind=sp)            :: delta
#if (USE_STRUCT_PADDING) == 1
             STRUCT_PADDING(0,8)
#endif
              real(kind=sp), dimension(:,:), allocatable :: P0
              real(kind=sp), dimension(:,:), allocatable :: s
              real(kind=sp), dimension(:,:), allocatable :: Imn
              real(kind=sp), dimension(:,:), allocatable :: P
              !dir$ attributes align : 64 :: P0
              !dir$ attributes align : 64 :: s
              !dir$ attributes align : 64 :: Imn
              !dir$ attributes align : 64 :: P
        end type f2110_r4_t


        ! Formula (2-110)
        ! The averaged radiation pattern of antenna array
        type, public :: f2110_r8_t

              integer(kind=i4)         :: m
              integer(kind=i4)         :: n
              integer(kind=i4)         :: nth
              integer(kind=i4)         :: nph
              real(kind=dp)            :: sig
              real(kind=dp)            :: delta
              real(kind=dp), dimension(:,:), allocatable :: P0
              real(kind=dp), dimension(:,:), allocatable :: s
              real(kind=dp), dimension(:,:), allocatable :: Imn
              real(kind=dp), dimension(:,:), allocatable :: P
              !dir$ attributes align : 64 :: P0
              !dir$ attributes align : 64 :: s
              !dir$ attributes align : 64 :: Imn
              !dir$ attributes align : 64 :: P
        end type f2110_r8_t
 
       



    
    
    
 









end module antenna_types
