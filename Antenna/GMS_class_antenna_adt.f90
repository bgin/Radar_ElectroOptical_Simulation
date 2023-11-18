
#include "GMS_config.fpp"


module antenna_adt_class



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         antenna_adt_class
 !          
 !          Purpose:
 !                        Module types for 'antenna_sensor'  implementation.
 !                        Various characteristics of different antenna types  
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
    integer(kind=i4),  parameter :: ANTENNA_ADT_CLASS_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: ANTENNA_ADT_CLASS_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: ANTENNA_ADT_CLASS_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: ANTENNA_ADT_CLASS_FULLVER =   &
            1000*ANTENNA_ADT_CLASS_MAJOR+100*ANTENNA_ADT_CLASS_MINOR+10*ANTENNA_ADT_CLASS_MICRO
    ! Module creation date
    character(*),        parameter :: ANTENNA_ADT_CLASS_CREATE_DATE = "18-11-2023 09:53 +00200 (SAT 18 NOV 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: ANTENNA_ADT_CLASS_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: ANTENNA_ADT_CLASS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: ANTENNA_ADT_CLASS_SYNOPSIS    = "ADT describing various antenna types characteristics -- module based."

    
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
    
    
    ! Complex Electric Field (complex-single)
    ! First dimension nth field, second dimension number of sample points
    complex(kind=sp), dimension(:,:), allocatable :: exc4
    complex(kind=sp), dimension(:,:), allocatable :: eyc4
    complex(kind=sp), dimension(:,:), allocatable :: ezc4
    !dir$ attributes align : 64 :: exc4
    !dir$ attributes align : 64 :: eyc4
    !dir$ attributes align : 64 :: ezc4
    
    ! Complex Electric Field (complex-double)
    complex(kind=dp), dimension(:), allocatable :: exc8
    complex(kind=dp), dimension(:), allocatable :: eyc8
    complex(kind=dp), dimension(:), allocatable :: ezc8
    !dir$ attributes align : 64 :: exc8
    !dir$ attributes align : 64 :: eyc8
    !dir$ attributes align : 64 :: ezc8
    
    ! Complex Electric Field (real-single) (decomposed)
    real(kind=sp), dimension(:), allocatable :: ex4r
    real(kind=sp), dimension(:), allocatable :: ex4i
    real(kind=sp), dimension(:), allocatable :: ey4r
    real(kind=sp), dimension(:), allocatable :: ey4i
    real(kind=sp), dimension(:), allocatable :: ez4r
    real(kind=sp), dimension(:), allocatable :: ez4i
    !dir$ attributes align : 64 :: ex4r
    !dir$ attributes align : 64 :: ex4i
    !dir$ attributes align : 64 :: ey4r
    !dir$ attributes align : 64 :: ey4i
    !dir$ attributes align : 64 :: ez4r
    !dir$ attributes align : 64 :: ez4i

    ! Complex Electric Field (real-double) (decomposed)
    real(kind=dp), dimension(:), allocatable :: ex8r
    real(kind=dp), dimension(:), allocatable :: ex8i
    real(kind=dp), dimension(:), allocatable :: ey8r
    real(kind=dp), dimension(:), allocatable :: ey8i
    real(kind=dp), dimension(:), allocatable :: ez8r
    real(kind=dp), dimension(:), allocatable :: ez8i
    !dir$ attributes align : 64 :: ex8r
    !dir$ attributes align : 64 :: ex8i
    !dir$ attributes align : 64 :: ey8r
    !dir$ attributes align : 64 :: ey8i
    !dir$ attributes align : 64 :: ez8r
    !dir$ attributes align : 64 :: ez8i
























end module antenna_adt_class
