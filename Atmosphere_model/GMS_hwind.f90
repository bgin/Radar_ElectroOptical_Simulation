

module Hwind

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'Hwind'
 !          
 !          Purpose:
 !                     This module contains a single derived type "hwind_t"
 !                     which stores the input parameters and resulting output
 !                     of HWM14 model
 !          History:
 !                        
 !                        Date: 16-06-2022
 !                        Time: 10:29 GMT+2
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
 !          Contact:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp
     implicit none
     public


     !===============================================!
     !      Horizontal Wind 14 derived type          !
     !===============================================!

!!!============================================================================
!!! Input arguments:
!!!        iyd - year and day as yyddd
!!!        sec - ut(sec)
!!!        alt - altitude(km)
!!!        glat - geodetic latitude(deg)
!!!        glon - geodetic longitude(deg)
!!!        stl - not used
!!!        f107a - not used
!!!        f107 - not used
!!!        ap - two element array with
!!!             ap(1) = not used
!!!             ap(2) = current 3hr ap index
!!!
!!! Output argument:
!!!        w(1) = meridional wind (m/sec + northward)
!!!        w(2) = zonal wind (m/sec + eastward)
!!!
!!!============================================================================

     type, public :: hwind_t
     
        integer(kind=i4) :: n_samp
        integer(kind=i4) :: iyd
        real(kind=sp), dimension(:), allocatable :: alt
        real(kind=sp), dimension(:), allocatable :: sec
        real(kind=sp), dimension(:), allocatable :: glon
        real(kind=dp), dimension(:), allocatable :: glat
        real(kind=sp), dimension(:), allocatable :: meridional
        real(kind=sp), dimension(:), allocatable :: zonal
        !dir$ attributes align : 64 :: alt
        !dir$ attributes align : 64 :: sec
        !dir$ attributes align : 64 :: glon
        !dir$ attributes align : 64 :: glat
        !dir$ attributes align : 64 :: meridional
        !dir$ attributes align : 64 :: zonal
     end type hwind_t









end module Hwind
