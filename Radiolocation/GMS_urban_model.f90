

#include "GMS_config.fpp"


module urban_model

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         urban_model
 !          
 !          Purpose:
 !                        Module data based description of urban built up area.
 !                        Various characteristics of different urban building being
 !                        a model for EM-wave diffraction and scattering computations.
 !                        
 !          History:
 !                        Date: 18-12-2023
 !                        Time: 06:28 GMT+2
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
 !                      Own design.
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
    use mod_kinds,    only : i4,sp
    public
    implicit none


     ! Major version
    integer(kind=i4),  parameter :: URBAN_MODEL_MAJOR = 1
    ! Minor version
    integer(kind=i4),  parameter :: URBAN_MODEL_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: URBAN_MODEL_MICRO = 0
    ! Full version
    integer(kind=i4),  parameter :: URBAN_MODEL_FULLVER =   &
            1000*URBAN_MODEL_MAJOR+100*URBAN_MODEL_MINOR+10*URBAN_MODEL_MICRO
    ! Module creation date
    character(*),        parameter :: URBAN_MODEL_CREATE_DATE = "12-12-2023 15:32 +00200 (TUE 12 DEC 20223 GMT+2)"
    ! Module build date
    character(*),        parameter :: URBAN_MODEL_BUILD_DATE  = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: URBAN_MODEL_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Short description
    character(*),        parameter :: URBAN_MODEL_SYNOPSIS    = "Data describing various dipole antenna characteristics -- type based, AVX(256-bit)-sized."

    
    !************************************************************************************************!
    !************************************* Arrays size parameters ***********************************!

    ! Number of latitude   values (deg), per building
    integer(i4) :: nblatd
    ! Number of longtitude values (deg), per building
    integer(i4) :: nblond
    
    ! Number of latitude   values (rad), per building
    integer(i4) :: nblatr
    ! Number of longtitude values (rad), per building
    integer(i4) :: nblonr
    
    ! Number of ellipsoidal (radar waveform irradiating field) cells for building column
    integer(i4) :: nellb
    
    ! Parametric equation x (acos(t)) values (building)
    integer(i4) :: npxb
    
    ! Parametric equation y (b(sin(t)) values (building)
    integer(i4) :: npyb

    ! Number of building units per column
    integer(i4) :: nbpc
    ! Number of building units per row
    integer(i4) :: nbpr
    
    ! Number of streets
    integer(i4) :: nstr
    ! Length of every street
    integer(i4) :: nlstr
    ! Width of every street
    integer(i4) :: nwstr
    ! Moisture of every street  at each ellipsoidal cell
    integer(i4) :: nwtstr
    ! Percent of moist to dry area of evey street at each ellipsoidal cell
    integer(i4) :: nphds
    ! Coverage of every street (like: snow,mud, ...etc)
    integer(i4) :: ncstr
    ! Percent of covered to non-covered portion of every street
    integer(i4) :: npcns
    ! Average thickness of each layer (cover) of every street
    integer(i4) :: natcstr
    ! Thickness of cover along street (number of values)
    integer(i4) :: ntcstr
    ! Mu0 for 'clean' street interpolated along the street length
    integer(i4) :: nmu0str1
    ! Eps0 for clean street street length interpolated
    integer(i4) :: neps0str1
    ! Mu for covered street interpolated along the street length
    integer(i4) :: nmu0str2
    ! Eps for covered street street length interpolated
    integer(i4) :: neps0str2
    ! Street curvature parametric equation u-parameter
    integer(i4) :: nupstr
    ! Street curvature parametric equation v-parameter
    integer(i4) :: nvpstr
    ! Street surface normal vectors x-components along the street length
    integer(i4) :: nvxstr
    ! Street surface normal vectors y-components along the street length
    integer(i4) :: nvystr
    ! Street surface normal vectors z-components along the street length
     ! Number of latitude   values (deg), per street length (at irradiance point)
    integer(i4) :: nslatd
    ! Number of longtitude values (deg), per street length (at irradiance point)
    integer(i4) :: nslond
    ! Number of latitude   values (rad), per street length (at irradiance point)
    integer(i4) :: nslatr
    ! Number of longtitude values (rad), per street length (at irradiance point)
    integer(i4) :: nslonr
    
    ! latitude   values (deg), per building
    real(sp), dimension(:), allocatable :: blatd
    !dir$ attributes align : 64 :: blatd
   
    ! longtitude values (deg), per building
    real(sp), dimension(:), allocatable :: blond
    !dir$ attributes align : 64 :: blond
    
    ! latitude   values (rad), per building
     real(sp), dimension(:), allocatable :: blatr
    !dir$ attributes align : 64 :: blatr
    
    ! longtitude values (rad), per building
    real(sp), dimension(:), allocatable :: blonr
    !dir$ attributes align : 64 :: blonr
    
    ! ellipsoidal (radar waveform irradiating field) cells for building column
    integer(i4), dimension(:,:), allocatable :: ellpb
    !dir$ attributes align : 64 :: ellpb
    
    ! Parametric equation x (acos(t)) values (building)
    ! 1st dimension building column, 2nd dimension 'x' parameter values
    real(sp), dimension(:,:),    allocatable :: pxb
    !dir$ attributes align : 64 :: pxb
    
    ! Parametric equation y (b(sin(t)) values (building)
    ! 1st dimension building column, 2nd dimension 'y' parameter values
    real(sp), dimension(:,:),    allocatable :: pyb
    !dir$ attributes align : 64 :: pyb
    
    ! building units per column
    integer(i4), dimension(:), allocatable :: bpc
    !dir$ attributes align : 64 :: bpc
    
    ! building units per raw
    integer(i4), dimension(:), allocatable :: bpr
    !dir$ attributes align : 64 :: bpr
    
    ! number of streets
    integer(i4), dimension(:), allocatable :: str
    !dir$ attributes align : 64 :: str
    
    ! Length of every street
    real(sp), dimension(:), allocatable :: lstr
    !dir$ attributes align : 64 :: lstr
    
    ! Width of every street
    real(sp), dimension(:), allocatable :: wstr
    !dir$ attributes align : 64 :: wstr
    
    ! Moisture of every street (2D array)
    ! 1st dimension humidity values (per street), 2nd dimension street numbers
    real(sp), dimension(:,:), allocatable :: wtstr
    !dir$ attributes align : 64 :: wtstr
    
    ! Percent of moist to dry area of evey street at each cell
    real(sp), dimension(:,:), allocatable :: phds
    !dir$ attributes align : 64 :: phds
    
    !Coverage of every street (like: snow,mud, ...etc)
    integer(i4), dimension(:,:), allocatable :: cstr
    !dir$ attributes align : 64 :: cstr
    
    ! Percent of covered to non-covered portion of every street (at irradiated cell)
    real(sp),  dimension(:,:),  allocatable :: pcns
    !dir$ attributes align : 64 :: pcns
    
end module urban_model
