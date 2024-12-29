#include "GMS_config.fpp"

!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module emw_refraction_types


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         emw_refraction_types
 !          
 !          Purpose:
 !                        Derived data types for 'emw_refraction' module implementation.
 !                        Various characteristics and formulae of atmospheric refraction (radio waves and visible light/IR wavelengths)  
 !                        Based mainly on      Колосов М.А., Шабельников А.В. - Рефракция электромагнитных волн в атмосферах Земли, Венеры и Марса-Советское Радио (1976)    
 !                       
 !          History:
 !                        Date: 29-12-2024
 !                        Time: 13:11 GMT+2
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
 !                       Колосов М.А., Шабельников А.В. 
 !                       "Рефракция электромагнитных волн в атмосферах Земли, Венеры и Марса-Советское Радио (1976)"   
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
     integer(kind=i4),  parameter :: EMW_REFRACTION_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EMW_REFRACTION_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EMW_REFRACTION_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EMW_REFRACTION_TYPES_FULLVER =   &
            1000*EMW_REFRACTION_TYPES_MAJOR+100*EMW_REFRACTION_TYPES_MINOR+10*EMW_REFRACTION_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: EMW_REFRACTION_TYPES_CREATE_DATE = "29-12-2024 13:13 +00200 (SUN 29 DEC 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: EMW_REFRACTION_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EMW_REFRACTION_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EMW_REFRACTION_TYPES_SYNOPSIS    = "EM Wave atmospheric refraction data types."

     ! Constants:

     real(kind=sp), parameter, private :: L    = 6.02214076e+23_sp ! (mol-1), avogadro constant
     real(kind=sp), parameter, private :: K1h  = 0.77607_sp+0.0013_sp ! K/Pa
     real(kind=sp), parameter, private :: K1l  = 0.77607_sp-0.0013_sp ! K/Pa
     real(kind=sp), parameter, private :: K2h  = 0.716_sp+0.085_sp    ! K/Pa
     real(kind=sp), parameter, private :: K2l  = 0.716_sp-0.085_sp    ! K/Pa
     real(kind=sp), parameter, private :: K3h  = 3747.0_sp+31.0_sp    ! K^2/Pa
     real(kind=sp), parameter, private :: K3l  = 3747.0_sp-31.0_sp    ! K^2/Pa
     real(kind=sp), parameter, private :: K4h  = 1.2934_sp+0.0002_sp  ! K/Pa
     real(kind=sp), parameter, private :: K4l  = 1.2934_sp-0.0002_sp  ! K/Pa

     ! WRF compatibile array indexing
     type, public :: wrf_indexing
            
           ! Tile indices
           integer(kind=i4) :: jts
           integer(kind=i4) :: jte 
           integer(kind=i4) :: kts
           integer(kind=i4) :: kte 
           integer(kind=i4) :: its 
           integer(kind=i4) :: ite 
           ! Memory indices
           integer(kind=i4) :: jms 
           integer(kind=i4) :: jme 
           integer(kind=i4) :: kms 
           integer(kind=i4) :: kme 
           integer(kind=i4) :: ims 
           integer(kind=i4) :: ime 
           ! Domain indices
           integer(kind=i4) :: jds 
           integer(kind=i4) :: jde 
           integer(kind=i4) :: kds 
           integer(kind=i4) :: kde 
           integer(kind=i4) :: ids 
           integer(kind=i4) :: ide 
     end type wrf_indexing

     ! Диэлектрическая проницаемость любого газа (1.4)
     type, public :: gas_permittivity_f14_t
           
           character(len=8)                             :: wavelength ! can be either optical,submilimeter/milimeter or radio, i.e. [optical, submm,radio]
           type(wrf_indexing)                           :: indices 
           real(kind=sp), dimension(:,:,:), allocatable :: gas_eps
           !dir$ attributes align : 64 :: gas_eps

     end type gas_permittivity_f14_t

     !удельной .влажности в тропосфере и стратосфере , formula: 1.44, page 28
     type, public :: hum_avg_f144_t
           
           type(wrf_indexing) :: indices 
           real(kind=sp), dimension(:,:,:), allocatable :: Sh ! average of humidity (tropo,stratosphere)
            !dir$ attributes align : 64 :: Sh
     end type hum_avg_f144_t

     ! Усредненная зависимость показателя преломления, formula 1.45, page 29
     type, public :: n_avg_f145_t
           
           type(wrf_indexing) :: indices 
           real(kind=sp), dimension(:,:,:), allocatable :: n_avg ! average refraction index as height function.
            !dir$ attributes align : 64 :: n_avg
     end type n_avg_f145_t

     

    















end module emw_refraction_types