

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

    
    type, public :: E_C4_t
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
    end type E_C4_t


    type, public :: H_C4_t
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
    end type H_C4_t


    type, public :: E_C8_t
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
    end type E_C8_t


    type, public :: H_C8_t
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
    end type H_C8_t


    type, public :: E_R4_t
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
    end type E_R4_t


    type, public :: H_R4_t
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
    end type H_R4_t


    type, public :: E_R8_t
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
    end type E_R8_t


    type, public :: H_R8_t
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
    end type H_R8_t


    type, public :: JE_C4_t
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
    end type JE_C4_t


    type, public :: JM_C4_t
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
    end type JM_C4_t


    type, public :: JE_C8_t
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
    end type JE_C8_t


    type, public :: JM_C8_t
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
    end type JM_C8_t


    type, public :: JE_R4_t
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
    end type JE_R4_t


    type, public :: JM_R4_t
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
    end type JM_R4_t


    type, public :: JE_R8_t
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
    end type JE_R8_t


    type, public :: JM_R8_t
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
    end type JM_R8_t


    type, public :: EIKR_C4_t
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
    end type EIKR_C4_t


    type, public :: EIKR_C8_t
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
    end type EIKR_C8_t


    type, public :: EIKR_R4_t
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
    end type EIKR_R4_t


    type, public :: EIKR_R8_t
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
    end type EIKR_R8_t


    
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
         real(kind=dp), dimension(:), allocatable :: F     ! Antenna  rdiation pattern 
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
          real(kind=sp), dimension(:), allocatable :: Fsqr     ! Antenna  rdiation pattern 
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
          real(kind=dp), dimension(:), allocatable :: Fsqr     ! Antenna  rdiation pattern 
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
    type, public :: HVE_C4_t
          ! Hertz vector electric
          integer(kind=i4)      :: npts
          complex(kind=sp)      :: ifac
          type(JE_C4_t)         :: jec4
          type(EIKR_C4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif  
          complex(kind=sp), dimension(:), allocatable :: he_x
          complex(kind=sp), dimension(:), allocatable :: he_y
          complex(kind=sp), dimension(:), allocatable :: he_z
          !dir$ attributes align : 64 :: he_x
          !dir$ attributes align : 64 :: he_y
          !dir$ attributes align : 64 :: he_z
    end type HVE_C4_t

    ! Formula (2-13)
    type, public :: HVE_C8_t
          ! Hertz vector electric
          integer(kind=i4)      :: npts
          complex(kind=dp)      :: ifac
          type(JE_C8_t)         :: jec4
          type(EIKR_C8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif  
          complex(kind=dp), dimension(:), allocatable :: he_x
          complex(kind=dp), dimension(:), allocatable :: he_y
          complex(kind=dp), dimension(:), allocatable :: he_z
          !dir$ attributes align : 64 :: he_x
          !dir$ attributes align : 64 :: he_y
          !dir$ attributes align : 64 :: he_z
    end type HVE_C8_t


    ! Formula (2-13)
    type, public :: HVE_R4_t
          ! Hertz vector electronic decomposed 
          integer(kind=i4)      :: npts
          real(kind=sp)         :: if_re
          real(kind=sp)         :: if_im
          type(JE_R4_t)         :: jer4
          type(EIKR_R4_t)       :: eikr
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
    end type HVE_R4_t


     ! Formula (2-13)
    type, public :: HVE_R8_t
          ! Hertz vector electronic decomposed 
          integer(kind=i4)      :: npts
          real(kind=dp)         :: if_re
          real(kind=dp)         :: if_im
          type(JE_R8_t)         :: jer4
          type(EIKR_R8_t)       :: eikr
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
    end type HVE_R8_t


    ! Formula (2-15)
    type, public :: HVM_C4_t
          ! Hertz vector magnetic
          integer(kind=i4)      :: npts
          complex(kind=sp)      :: ifac
          type(JM_C4_t)         :: jmc4
          type(EIKR_C4_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,12)
#endif  
          complex(kind=sp), dimension(:), allocatable :: hm_x
          complex(kind=sp), dimension(:), allocatable :: hm_y
          complex(kind=sp), dimension(:), allocatable :: hm_z
          !dir$ attributes align : 64 :: hm_x
          !dir$ attributes align : 64 :: hm_y
          !dir$ attributes align : 64 :: hm_z
    end type HVM_C4_t

    ! Formula (2-15)
    type, public :: HVM_C8_t
          ! Hertz vector magnetic
          integer(kind=i4)      :: npts
          complex(kind=dp)      :: ifac
          type(JM_C8_t)         :: jmc8
          type(EIKR_C8_t)       :: eikr
#if (USE_STRUCT_PADDING) == 1
           STRUCT_PADDING(0,4)
#endif  
          complex(kind=dp), dimension(:), allocatable :: hm_x
          complex(kind=dp), dimension(:), allocatable :: hm_y
          complex(kind=dp), dimension(:), allocatable :: hm_z
          !dir$ attributes align : 64 :: hm_x
          !dir$ attributes align : 64 :: hm_y
          !dir$ attributes align : 64 :: hm_z
    end type HVM_C8_t


    ! Formula (2-15)
    type, public :: HVM_R4_t
          ! Hertz vector magnetic decomposed 
          integer(kind=i4)      :: npts
          real(kind=sp)         :: if_re
          real(kind=sp)         :: if_im
          type(JM_R4_t)         :: jmr4
          type(EIKR_R4_t)       :: eikr
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
    end type HVM_R4_t


     ! Formula (2-15)
    type, public :: HVM_R8_t
          ! Hertz vector magnetic decomposed 
          integer(kind=i4)      :: npts
          real(kind=dp)         :: if_re
          real(kind=dp)         :: if_im
          type(JM_R8_t)         :: jmr8
          type(EIKR_R8_t)       :: eikr
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
    end type HVM_R8_t


    ! Formula (2-22,2-23)
    type, public :: NEVec_r4_t

          !
          
    end type NEVec_r4_t




    



    
    
    
 









end module antenna_types
