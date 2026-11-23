

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

    use mod_kinds,     only : i1,i4,i8,dp
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

real(kind=i4), parameter, private :: MAX_BUF_SIZE = 1000_i4 


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
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif 
       this.m_nsamples = nsamples 
       if(num_gaussians .gt. 1000_i4) then 
          this.m_num_gaussians = 1000_i4 
       else 
          this.m_num_gaussians = num_gaussians 
       end if 
       if(tstart .lt. 0.0_dp) then 
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
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE)
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END
#endif 
end subroutine construct 

subroutine destroy(this,to_clear_values,ier)
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
       character(*), parameter :: sub_name = "destroy"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this 
    logical(kind=i4),                   intent(in), value :: to_clear_values
    integer(kind=i4),                   intent(inout)     :: ier 
    ! Exec code 
    ier = -9999
    if(this.m_build_state .eq. .false.) then 
       ier = -1 
       return 
    end if 
    if(allocated(this.m_signal_samples) .eq. .false.) then 
       ier = -2 
       return  
    end if 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif 
    RAND_PM_GAUSSIAN_SIGNAL_SAFE_DEALLOC(this.m_signal_samples) 
    if(to_clear_values .eq. .true.) then 
       this.m_nsamples = 0_i4 
       this.m_num_gaussians = 0_i4 
       this.m_tstart        = 0.0_dp 
       this.m_tend          = 0.0_dp 
       this.m_min_sigma     = 0.0_dp 
       this.m_max_sigma     = 0.0_dp 
       this.m_max_amplitude = 0.0_dp
    endif 
    this.m_build_state = .false. 
    ier = 0
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END 
#endif 
end subroutine destroy

subroutine clear_samples(this,r8_def,use_memset,ier)
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
       character(*), parameter :: sub_name = "clear_samples"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this
    real(kind=dp),                      intent(in), value :: r8_def
    logical(kind=i4),                   intent(in), value :: use_memset 
    integer(kind=i4),                   intent(inout)     :: ier 
    integer(kind=i4), automatic :: i,j 
    ier = -9999_i4
    if(this.m_build_state .eq. .false.) then 
       ier = -1_i4
       return 
    end if 
    if(allocated(this.m_signal_samples) .eq. .false.) then 
       ier = -2_i4 
       return 
    end if 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif 
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = r8_def 
    else 
       do i=1_i4,iand(this.m_nsamples,inot(3_i4)),4_i4
            this.m_signal_samples(i+0_i4) = r8_def 
            this.m_signal_samples(i+1_i4) = r8_def 
            this.m_signal_samples(i+2_i4) = r8_def 
            this.m_signal_samples(i+3_i4) = r8_def
       end do 
       do j=i,this.m_nsamples 
            this.m_signal_samples(j) = r8_def
       end do 
    endif 
    ier = 0_i4 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END 
#endif 
end subroutine clear_samples 

subroutine copy_create(this,other,use_memset,ier)
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
       character(*), parameter :: sub_name = "copy_create"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this
    type(rand_pm_gaussian_signal_r8_t), intent(in)        :: other 
    logical(kind=i4),                   intent(in), value :: use_memset 
    integer(kind=i4),                   intent(inout)     :: ier 
    integer(kind=i4), automatic :: i,j 
    ! Exec code 
    ier = -9999_i4 
    if(this.m_build_state .eq. .false.) then 
       ier = -1_i4
       return 
    end if 
    if(other.m_build_state .eq. .false.) then 
       ier = -2_i4
       return 
    end if 
    if(allocated(other.m_signal_samples) .eq. .false.) then 
       ier = -3_i4 
       return 
    end if 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif
    this.m_nsamples      = other.m_nsamples 
    this.m_num_gaussians = other.m_num_gaussians
    this.m_tstart        = other.m_tstart 
    this.m_tend          = other.m_tend  
    this.m_min_sigma     = other.m_min_sigma  
    this.m_max_sigma     = other.m_max_sigma  
    this.m_max_amplitude = other.m_max_amplitude 
    RAND_PM_GAUSSIAN_SIGNAL_SAFE_ALLOC(this.m_signal_samples, (this.m_nsamples))
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = other.m_signal_samples 
    else 
       do i=1,iand(this.m_nsamples,inot(3_i4)),4_i4
            this.m_signal_samples(i+0_i4) = other.m_signal_samples(i+0_i4)
            this.m_signal_samples(i+1_i4) = other.m_signal_samples(i+1_i4)
            this.m_signal_samples(i+2_i4) = other.m_signal_samples(i+2_i4)
            this.m_signal_samples(i+3_i4) = other.m_signal_samples(i+3_i4)
       end do 
       do j=i,this.m_nsamples 
          this.m_signal_samples(j) = other.m_signal_samples(j)
       end do 
    end if 
    this.m_build_state = .true. 
    ier = 0
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END 
#endif 
end subroutine copy_create

subroutine copy_assign(this,other,use_memset,ier)
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
       character(*), parameter :: sub_name = "copy_assign"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this
    type(rand_pm_gaussian_signal_r8_t), intent(in)        :: other 
    logical(kind=i4),                   intent(in), value :: use_memset 
    integer(kind=i4),                   intent(inout)     :: ier 
    integer(kind=i4), automatic :: i,j 
    ! Exec code 
    ier = -9999_i4 
    if(this.m_build_state .eq. .false.) then 
       ier = -1_i4
       return 
    end if 
    if(other.m_build_state .eq. .false.) then 
       ier = -2_i4
       return 
    end if 
    if(allocated(other.m_signal_samples) .eq. .false.) then 
       ier = -3_i4 
       return 
    end if 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif
    this.m_nsamples      = other.m_nsamples 
    this.m_num_gaussians = other.m_num_gaussians
    this.m_tstart        = other.m_tstart 
    this.m_tend          = other.m_tend  
    this.m_min_sigma     = other.m_min_sigma  
    this.m_max_sigma     = other.m_max_sigma  
    this.m_max_amplitude = other.m_max_amplitude 
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = other.m_signal_samples 
    else 
       do i=1,iand(this.m_nsamples,inot(3_i4)),4_i4
            this.m_signal_samples(i+0_i4) = other.m_signal_samples(i+0_i4)
            this.m_signal_samples(i+1_i4) = other.m_signal_samples(i+1_i4)
            this.m_signal_samples(i+2_i4) = other.m_signal_samples(i+2_i4)
            this.m_signal_samples(i+3_i4) = other.m_signal_samples(i+3_i4)
       end do 
       do j=i,this.m_nsamples 
          this.m_signal_samples(j) = other.m_signal_samples(j)
       end do 
    end if 
    this.m_build_state = .true. 
    ier = 0
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END 
#endif 
end subroutine copy_assign 

subroutine show(this,ier)
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
       character(*), parameter :: sub_name = "show"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(in)        :: this
    integer(kind=i4),                   intent(inout)     :: ier 
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_START
#endif
    ier = -9999
    print*, "object-vaddr=", LOC(this)
    print*, "object-size =", SIZEOF(this), "bytes"
    call check_allocation_stats_real1D(this.m_signal_samples,"m_nsamples")
    print*, "[INFO]: Random Phase Modulated Gaussian SIgnal -- Scalar Members:"
    print*, "m_nsamples           =",  this.m_nsamples  
    print*, "m_num_gaussians      =",  this.m_num_gaussians
    print*, "m_tstart             =",  this.m_tstart 
    print*, "m_tend               =",  this.m_tend 
    print*, "this.m_min_sigma     =",  this.m_min_sigma
    print*, "this.m_max_sigma     =",  this.m_max_sigma 
    print*, "this.m_max_amplitude =",  this.m_max_amplitude  
    ier = 0
#if (RAND_PM_GAUSSIAN_SIGNAL_EXEC_TRACE) == 1
       RAND_PM_GAUSSIAN_SIGNAL_TRACE_END 
#endif
    contains 
    subroutine check_allocation_stats_real1D(array,name)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
                       character(len=*),                         intent(in) :: name 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*, "======================INFO==========================="
                       print*, "Array member name:", name 
                       print*, "vaddr=",vaddr
                       print*, "size=",size
                       print*, "allocated=",is_allocated
                       print*, "lo_bound=",lo_bound
                       print*, "hi_bound=",hi_bound 
                       print*,"======================INFO==========================="
    end subroutine check_allocation_stats_real1D
end subroutine show 


subroutine generate(this,ier)
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
       character(*), parameter :: sub_name = "generate"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this
    integer(kind=i4),                   intent(inout)     :: ier 
    type :: gaussian_components_t 
       real(kind=dp) :: amplitude 
       real(kind=dp) :: mu 
       real(kind=dp) :: two_sigma_sq 
    end type gaussian_components_t
    type(gaussian_components_t), dimension(1:MAX_BUF_SIZE) :: gc_store 
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attribute align : 64 :: gc_store
#endif
     integer(kind=i4),allocatable :: seed(:)
     real(kind=dp), parameter    :: sqrt2PI = 2.506628274631000502415765284811_dp 
     real(kind=dp), automatic    :: padding,dt
     real(kind=dp), automatic    :: randn_mu,randn_sigma,randn_amp 
     real(kind=dp), automatic    :: scaled_rmu,scaled_rsigma,scaled_ramp
     real(kind=dp), automatic    :: mu_lo,mu_hi 
     real(kind=dp), automatic    :: sigma_lo,sigma_hi 
     real(kind=dp), automatic    :: amp_lo,amp_hi
     real(kind=dp), automatic    :: t_j,t,phase_accumulator  
     real(kind=dp), automatic    :: tmu,diff,tamplitude,tsigma
     real(kind=dp), automatic    :: normalization,exp_val
     integer(kind=i4),automatic  :: seed_size, clock_count, ii
     integer(kind=i4), automatic :: i,j,k 
     ier = -9999_i4 
     if(this.m_num_gaussians .gt. MAX_BUF_SIZE) then 
        ier = -1_i4 
        return 
     end if       
     call random_seed(size = seed_size)
     allocate(seed(seed_size))
     call system_clock(count = clock_count)
     seed = clock_count + 37_i4 * [(ii - 1, ii = 1, seed_size)]
     call random_seed(put = seed)
     padding  = 3.0_dp*this.m_max_sigma
     mu_lo    = this.m_tstart-padding
     mu_hi    = this.m_tend+padding 
     sigma_lo = this.m_min_sigma 
     sigma_hi = this.m_max_sigma
     amp_lo   = -this.m_max_amplitude 
     amp_hi   = this.m_max_amplitude 
     randn_amp   = 0.0_dp 
     randn_mu    = 0.0_dp 
     randn_sigma = 0.0_dp 
     do i=1, MAX_BUF_SIZE 
        call random_number(randn_amp)
        scaled_ramp = amp_lo+(amp_hi-amp_lo)*randn_amp
        gc_store(i).amplitude = scaled_ramp
        call random_number(randn_mu)
        scaled_rmu = mu_lo+(mu_hi-mu_lo)*randn_mu
        gc_store(i).mu = scaled_rmu 
        call random_number(randn_sigma)
        scaled_rsigma = sigma_lo+(sigma_hi-sigma_lo)*randn_sigma 
        gc_store(i).two_sigma_sq = 2.0_dp*scaled_rsigma*scaled_rsigma 
     end do 
     dt = (this.m_tend-this.m_tstart)/real(this.m_nsamples,kind=dp)
     do j=1, this.m_nsamples 
       t_j = real(j,kind=dp)
       t   = this.m_tstart+t_j*dt 
       phase_accumulator = 0.0_dp 
       do k=1,this.m_num_gaussians
          tmu     = gc_store(k).mu 
          diff    = t-tmu 
          tamplitude = gc_store(k).amplitude 
          tsigma  = gc_store(k).two_sigma_sq 
          normalization = 1.0_dp/(tsigma*sqrt2PI) 
          exp_val = exp(-(diff*diff)/tsigma)
          phase_accumulator = phase_accumulator+(tamplitude*exp_val)
       end do 
       this.m_signal_samples(j) = phase_accumulator 
     end do 
     deallocate(seed)
     ier = 0 
end subroutine generate 

subroutine generate_simd(this,ier)
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
       character(*), parameter :: sub_name = "generate_simd"
#endif 
    type(rand_pm_gaussian_signal_r8_t), intent(inout)     :: this
    integer(kind=i4),                   intent(inout)     :: ier 
    !type :: gaussian_components_t 
    !   real(kind=dp) :: amplitude 
    !   real(kind=dp) :: mu 
     !  real(kind=dp) :: two_sigma_sq 
    !end type gaussian_components_t
    !type(gaussian_components_t), dimension(1:MAX_BUF_SIZE) :: gc_store 
!#if defined(__INTEL_COMPILER) || defined(__ICC)
   !    !dir$ attribute align : 64 :: gc_store
!#endif
     real(kind=dp), dimension(1:MAX_BUF_SIZE) :: amplitude_store
     real(kind=dp), dimension(1:MAX_BUF_SIZE) :: mu_store 
     real(kind=dp), dimension(1:MAX_BUF_SIZE) :: two_sigma_sq_store 
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attribute align : 64 :: amplitude_store
       !dir$ attribute align : 64 :: mu_store 
       !dir$ attribute align : 64 :: two_sigma_sq_store
#endif
     integer(kind=i4),allocatable :: seed(:)
     real(kind=dp), parameter    :: sqrt2PI = 2.506628274631000502415765284811_dp 
     real(kind=dp), automatic    :: padding,dt
     real(kind=dp), automatic    :: randn_mu,randn_sigma,randn_amp 
     real(kind=dp), automatic    :: scaled_rmu,scaled_rsigma,scaled_ramp
     real(kind=dp), automatic    :: mu_lo,mu_hi 
     real(kind=dp), automatic    :: sigma_lo,sigma_hi 
     real(kind=dp), automatic    :: amp_lo,amp_hi
     real(kind=dp), automatic    :: t_j,t,phase_accumulator  
     real(kind=dp), automatic    :: tmu,diff,tamplitude,tsigma
     real(kind=dp), automatic    :: normalization,exp_val
     integer(kind=i4),automatic  :: seed_size, clock_count, ii
     integer(kind=i4), automatic :: i,j,k 
     ier = -9999_i4 
     if(this.m_num_gaussians .gt. MAX_BUF_SIZE) then 
        ier = -1_i4 
        return 
     end if       
     call random_seed(size = seed_size)
     allocate(seed(seed_size))
     call system_clock(count = clock_count)
     seed = clock_count + 37_i4 * [(ii - 1, ii = 1, seed_size)]
     call random_seed(put = seed)
     padding  = 3.0_dp*this.m_max_sigma
     mu_lo    = this.m_tstart-padding
     mu_hi    = this.m_tend+padding 
     sigma_lo = this.m_min_sigma 
     sigma_hi = this.m_max_sigma
     amp_lo   = -this.m_max_amplitude 
     amp_hi   = this.m_max_amplitude 
     randn_amp   = 0.0_dp 
     randn_mu    = 0.0_dp 
     randn_sigma = 0.0_dp 
     do i=1, MAX_BUF_SIZE 
        call random_number(randn_amp)
        scaled_ramp = amp_lo+(amp_hi-amp_lo)*randn_amp
        amplitude_store(i) = scaled_ramp
        call random_number(randn_mu)
        scaled_rmu = mu_lo+(mu_hi-mu_lo)*randn_mu
        mu_store(i) = scaled_rmu 
        call random_number(randn_sigma)
        scaled_rsigma = sigma_lo+(sigma_hi-sigma_lo)*randn_sigma 
        two_sigma_sq_store(i) = 2.0_dp*scaled_rsigma*scaled_rsigma 
     end do 
     dt = (this.m_tend-this.m_tstart)/real(this.m_nsamples,kind=dp)
     do j=1, this.m_nsamples 
       t_j = real(j,kind=dp)
       t   = this.m_tstart+t_j*dt 
       phase_accumulator = 0.0_dp 
!omp simd linear(k:1) aligned(mu_store,amplitude_store,tw_sigma_sq_store:64) reduction(+:phase_accumulator)
       do k=1,this.m_num_gaussians
          tmu     = mu_store(k) 
          diff    = t-tmu 
          tamplitude = amplitude_store(k) 
          tsigma  = two_sigma_sq_store(k) 
          normalization = 1.0_dp/(tsigma*sqrt2PI) 
          exp_val = exp(-(diff*diff)/tsigma)
          phase_accumulator = phase_accumulator+(tamplitude*exp_val)
       end do 
       this.m_signal_samples(j) = phase_accumulator 
     end do 
     deallocate(seed)
     ier = 0 
end subroutine generate_simd 

end module mod_rand_pm_gaussian_signal

