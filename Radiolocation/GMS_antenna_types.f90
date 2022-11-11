

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

    
    type, public :: E_C4
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
    end type E_C4


    type, public :: H_C4
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
    end type H_C4


    type, public :: E_C8
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
    end type E_C8


    type, public :: H_C8
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
    end type H_C8


    type, public :: E_R4
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
    end type E_R4


    type, public :: H_R4
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
    end type H_R4


    type, public :: E_R8
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
    end type E_R8


    type, public :: H_R8
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
    end type H_R8


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
    
    
 









end module antenna_types
