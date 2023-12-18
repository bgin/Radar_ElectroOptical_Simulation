

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
    integer(i4) :: nlatd
    ! Number of longtitude values (deg), per building
    integer(i4) :: nlond
    
    ! Number of latitude   values (rad), per building
    integer(i4) :: nlatr
    ! Number of longtitude values (rad), per building
    integer(i4) :: nlonr

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
    ! Moisture of every street 
    integer(i4) :: nwtstr
    ! Percent of moist to dry area of evey street
    integer(i4) :: nphds
    ! Coverage of every street (like: snow,mud, ...etc)
    integer(i4) :: ncstr
    ! Percent of covered to non-covered portion of every street
    integer(i4) :: npcns
    ! Average thickness of each layer (cover) of every street
    integer(i4) :: natcstr
    ! Thickness of cover along street (number of values)
    integer(i4) :: ntcstr






end module urban_model
