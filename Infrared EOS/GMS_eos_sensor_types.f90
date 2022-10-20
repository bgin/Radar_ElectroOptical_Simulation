

module eos_sensor_types



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         eos_sensor_types
 !          
 !          Purpose:
 !                        Derived data types for 'eos_sensor' module implementation.
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !          History:
 !                        Date: 20-10-2022
 !                        Time: 15:33 GMT+2
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
 !                       Miroshenko M.M book (rus):          
 !                      "Mathematical Theory of Electro-Optical Sensors"     
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
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_TYPES_FULLVER =   &
            1000*EOS_SENSOR_TYPES_MAJOR+100*EOS_SENSOR_TYPES_MINOR+10*EOS_SENSOR_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_TYPES_CREATE_DATE = "20-10-2022 15:34 +00200 (THR 20 OCT 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_TYPES_SYNOPSIS    = "EO Sensors derived data types."



     type, public :: param_gamma_r4_t
  
           integer(kind=i4)                         :: n
           real(kind=sp), dimension(:), allocatable ::  phi  ! input
           real(kind=sp), dimesnion(:), allocatable :: gamma ! output    
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma     
     end type param_gamma_r4_t


     type, public :: param_gamma_r8_t
  
           integer(kind=i4)                         :: n
           real(kind=dp), dimension(:), allocatable ::  phi  ! input
           real(kind=dp), dimesnion(:), allocatable :: gamma ! output    
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma     
     end type param_gamma_r8_t


     type, public :: SN_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: R
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: sn
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sn
     end type SN_r4_t


     type, public :: SN_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: R
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: sn
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sn
     end type SN_r8_t


     type, public :: SM_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: R
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: sm
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sm
     end type SM_r4_t


     type, public :: SM_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: R
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: sm
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: sm
     end type SM_r8_t


     type, public :: ratio_FH_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp), dimension(:), allocatable :: psi
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: fh
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: fh
     end type ratio_FH_r4_t


     type, public :: ratio_FH_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp), dimension(:), allocatable :: psi
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: fh
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: fh
     end type ratio_FH_r8_t


     type, public :: scan_mirror_ang_r4_t
 
           integer(kind=i4)                         :: n
           character(len=3)                         :: dir
           real(kind=sp), dimension(:), allocatable :: gam0
           real(kind=sp), dimension(:), allocatable :: psi
           real(kind=sp), dimension(:), allocatable :: phi
           real(kind=sp), dimension(:), allocatable :: gamma
           !dir$ attributes align : 64 :: gam0
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
     end type scan_mirror_ang_r4_t


     type, public :: scan_mirror_ang_r8_t
 
           integer(kind=i4)                         :: n
           character(len=3)                         :: dir
           real(kind=dp), dimension(:), allocatable :: gam0
           real(kind=dp), dimension(:), allocatable :: psi
           real(kind=dp), dimension(:), allocatable :: phi
           real(kind=dp), dimension(:), allocatable :: gamma
           !dir$ attributes align : 64 :: gam0
           !dir$ attributes align : 64 :: psi
           !dir$ attributes align : 64 :: phi
           !dir$ attributes align : 64 :: gamma
     end type scan_mirror_ang_r8_t


     type, public :: Dmax_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: d_ob
           real(kind=sp), dimension(:), allocatable :: h
           real(kind=sp), dimension(:), allocatable :: delta
           real(kind=sp), dimension(:), allocatable :: gamma
           real(kind=sp), dimension(:), allocatable :: dmax
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: dmax
     end type Dmax_r4_t


     type, public :: Dmax_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: d_ob
           real(kind=dp), dimension(:), allocatable :: h
           real(kind=dp), dimension(:), allocatable :: delta
           real(kind=dp), dimension(:), allocatable :: gamma
           real(kind=dp), dimension(:), allocatable :: dmax
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: gamma
           !dir$ attributes align : 64 :: dmax
     end type Dmax_r8_t


     type, public :: Dmin_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: d_ob
           real(kind=sp), dimension(:), allocatable :: h
           real(kind=sp), dimension(:), allocatable :: delta
           real(kind=sp), dimension(:), allocatable :: dmin
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: dmin
     end type Dmin_r4_t


     type, public :: Dmin_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: d_ob
           real(kind=dp), dimension(:), allocatable :: h
           real(kind=dp), dimension(:), allocatable :: delta
           real(kind=dp), dimension(:), allocatable :: dmin
           !dir$ attributes align : 64 :: h
           !dir$ attributes align : 64 :: delta
           !dir$ attributes align : 64 :: dmin
     end type Dmin_r8_t


     type, public :: defocus_cof_r4_t

           integer(kind=i4)                         :: n
           real(kind=sp)                            :: l2
           real(kind=sp)                            :: o
           logical(kind=i4)                         :: inf
           real(kind=sp), dimension(:), allocatable :: alpha
           real(kind=sp), dimension(:), allocatable :: dc
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: dc
     end type defocus_cof_r4_t


     type, public :: defocus_cof_r8_t

           integer(kind=i4)                         :: n
           real(kind=dp)                            :: l2
           real(kind=dp)                            :: o
           logical(kind=i4)                         :: inf
           real(kind=dp), dimension(:), allocatable :: alpha
           real(kind=dp), dimension(:), allocatable :: dc
           !dir$ attributes align : 64 :: alpha
           !dir$ attributes align : 64 :: dc
     end type defocus_cof_r8_t









end module eos_sensor_types
