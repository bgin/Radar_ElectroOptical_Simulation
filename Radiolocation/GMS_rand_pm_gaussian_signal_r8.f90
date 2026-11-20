

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

module mod_rand_pm_gaussian_signal

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_rand_pm_gaussian_signal
 !                         
 !          
 !          Purpose:
 !                       Randomly Phase-Modulated Gaussian Signal (double precision)
 !                        
 !          History:
 !                        Date: 20-11-2026
 !                        Time: 06:29AM GMT+2
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
 !                     Ported to Fortran 90 from the C++ version: "GMS_rand_pm_gaussian_signal_f64.h" a part of the RF-EMT project 
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i1,i4,dp
    use omp_lib
    implicit none
    public
      ! Major version
     integer(kind=i4),  parameter :: RAND_PM_GAUSSIAN_SIGNAL_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RAND_PM_GAUSSIAN_SIGNAL_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RAND_PM_GAUSSIAN_SIGNAL_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RAND_PM_GAUSSIAN_SIGNAL_FULLVER =   &
            1000*RAND_PM_GAUSSIAN_SIGNAL_MAJOR+100*RAND_PM_GAUSSIAN_SIGNAL_MINOR+10*RAND_PM_GAUSSIAN_SIGNAL_MICRO
     ! Module creation date
     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_CREATE_DATE = "20-11-2026 06:22AM +00200 (FRI 20 11 2026 GMT+2)"
     ! Module build date
     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_SYNOPSIS    = "Randomly Phase-Modulated Gaussian Signal (double precision)."

     character(*),        parameter :: RAND_PM_GAUSSIAN_SIGNAL_REFERENCE   = "Ported to Fortran 90 from the C++ version: 'GMS_rand_pm_gaussian_signal_f64.h' a part of the RF-EMT project."

!===========================================================================================================================================================================!

#define RAND_PM_GAUSSIAN_SIGNAL_SAFE_ALLOC(mem,size) if(allocated(mem)) deallocate(mem); allocate(mem size)
#define RAND_PM_GAUSSIAN_SIGNAL_SAFE_DEALLOC(mem)    if(allocated(mem)) deallocate(mem)

#if !defined(RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE)
#define RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE 1
#endif

#if !defined(RAND_PM_GAUSSIAN_SIGNAL_USE_EXPLICIT_DEALLOCATION)
#define RAND_PM_GAUSSIAN_SIGNAL_USE_EXPLICIT_DEALLOCATION 1 
#endif 

#define RAND_PM_GAUSSIAN_SIGNAL_TRACE_START \
print*, "[EXECUTION-TRACE: START]";\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;\
print*, "TSC-Start:", rdtsc_wrap()

#define RAND_PM_GAUSSIAN_SIGNAL_TRACE_END \
print*, "TSC-End:", rdtsc_wrap();\
print*, "[EXECUTION-TRACE: END]" ;\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;


type, public :: rand_pm_gaussian_signal_r8_t
    integer(kind=i4)  :: m_nsamples 
    integer(kind=i4)  :: m_num_gaussians
    real(kind=dp)     :: m_tstart 
    real(kind=dp)     :: m_tend 
    real(kind=dp)     :: m_min_sigma 
    real(kind=dp)     :: m_max_sigma
    real(kind=dp)     :: m_max_amplitude
    logical(kind=i4)  :: m_build_state 
    real(kind=dp), dimension(:), allocatable :: m_signal_samples
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attribute align : 64 :: m_signal_samples
#endif
end type rand_pm_gaussian_signal_r8_t

contains 

subroutine construct(this,nsamples,num_gaussians,tstart,  &
                    tend,min_sigma,max_sigma,max_amplitude, &
                    ier)
       implicit none   
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "construct"
#endif 
       type(rand_pm_gaussian_signal_r8_t), intent(inout)        :: this 
       integer(kind=i4),                   intent(in), value    :: nsamples 
       integer(kind=i4),                   intent(in), value    :: num_gaussians 
       real(kind=dp),                      intent(in), value    :: tstart 
       real(kind=dp),                      intent(in), value    :: tend 
       real(kind=dp),                      intent(in), value    :: min_sigma 
       real(kind=dp),                      intent(in), value    :: max_sigma 
       real(kind=dp),                      intent(in), value    :: max_amplitude 
       integer(kind=i4),                   intent(inout)        :: ier 
       ! Exec code 
       ier = -9999
       if(this.m_build_state .eq. .true.) then 
          ier = -1
          return 
       end if 
       this.m_nsamples = nsamples 
       if(num_gaussians .gt. 1000_i4) then 
          this.m_num_gaussians = 1000_i4 
       else 
          this.m_num_gaussians = num_gaussians 
       end if 
       if(tstart .ge. 0.0_dp) then 
          this.m_tstart = 0.0_dp 
       else 
          this.m_tstart  = tstart 
       end if 
       if(tend .lt. this.m_tstart) then 
          this.m_tend = this.m_tstart+2.0_dp 
       else 
          this.m_tend = tend 
       end if 
       this.m_min_sigma = min_sigma 
       this.m_max_sigma = max_sigma 
       this.m_max_amplitude = max_amplitude 
       RAND_PM_GAUSSIAN_SIGNAL_SAFE_ALLOC(this.m_signal_samples, (this.m_nsamples))
       this.m_build_state = .true. 
       ier = 0
end subroutine construct 


end module mod_rand_pm_gaussian_signal

