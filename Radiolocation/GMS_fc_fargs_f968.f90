

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


module fc_fargs_f968








!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: 
 !                         fc_fargs_f968
 !          
 !          Purpose:
 !                       Fading channels integrand functions additional arguments (to overcome a limit of argument
 !                        passing as set by the Quadpack integrators).
 !                       Aditional arguments of integrand(s), formula 9.68, p. 284
 !                        
 !          History:
 !                        Date: 14-12-2024
 !                        Time: 12:30PM GMT+2
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
 !          References:
 !         
 !                          Digital Communication over Fading Channels a Unified Approach, Marvin K. Simon, Mohamed Slim-Alouini
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i4,sp,dp
    
#if (USE_OPENMP) == 1
    use omp_lib
#endif

    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: FC_FARGS_F968_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: FC_FARGS_F968_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: FC_FARGS_F968_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: FC_FARGS_F968_FULLVER =   &
            1000*FC_FARGS_F968_MAJOR+100*FC_FARGS_F968_MINOR+10*FC_FARGS_F9568_MICRO
     ! Module creation date
     character(*),        parameter :: FC_FARGS_F968_CREATE_DATE = "14-12-2024 12:30PM +00200 (SAT 14 DEC 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: FC_FARGS_F968_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: FC_FARGS_F968_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: FC_FARGS_F968_SYNOPSIS  = "Aditional arguments of integrand(s), formula 9.68, p. 284." 
 
     ! Exact error rate analysis

integer(kind=i4) :: L
real(kind=sp)    :: Ar4
real(kind=sp)    :: vr4
real(kind=sp)    :: Omlr4
real(kind=sp)    :: mlr4
real(kind=sp)    :: Ar8
real(kind=sp)    :: vr8
real(kind=sp)    :: Omlr8
real(kind=sp)    :: mlr8

#if (USE_OPENMP) == 1
!$omp threadprivate(L,Ar4,vr4,Omlr4,mlr4,Ar8,vr8,Omlr8,mlr8)
#endif

end module fc_fargs_f968