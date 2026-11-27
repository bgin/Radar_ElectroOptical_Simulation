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

module mod_pm_bb_cmplx_cos_signal_c4

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_pm_bb_cmplx_cos_signal_r4
 !                         
 !          
 !          Purpose:
 !                       PM baseband complex cosine wave signal (single precision)
 !                        
 !          History:
 !                        Date: 25-11-2026
 !                        Time: 12:55PM GMT+2
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
 !                     Ported to Fortran 90 from the C++ version: "GMS_pm_bb_cmplx_cos_signal.h" a part of the RF-EMT project 
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i1,i4,i8,sp
    use omp_lib
    implicit none
    public
      ! Major version
     integer(kind=i4),  parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_FULLVER =   &
            1000*PM_BB_CMPLX_COS_SIGNAL_C4_MAJOR+100*PM_BB_CMPLX_COS_SIGNAL_C4_MINOR+10*PM_BB_CMPLX_COS_SIGNAL_C4_MICRO
     ! Module creation date
     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_CREATE_DATE = "25-11-2026 12:55PM +00200 (WED 25 2026 GMT+2)"
     ! Module build date
     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_SYNOPSIS    = "PM baseband complex cosine wave signal (single precision)."

     character(*),        parameter :: PM_BB_CMPLX_COS_SIGNAL_C4_REFERENCE   = "Ported to Fortran 90 from the C++ version: 'GMS_pm_bb_cmplx_cos_signal.h' a part of the RF-EMT project."

!===========================================================================================================================================================================!

#define PM_BB_CMPLX_COS_SIGNAL_C4_SAFE_ALLOC(mem,size) if(allocated(mem)) deallocate(mem); allocate(mem size)
#define PM_BB_CMPLX_COS_SIGNAL_C4_SAFE_DEALLOC(mem)    if(allocated(mem)) deallocate(mem)

#if !defined(PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE)
#define PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE 0
#endif

#if !defined(PM_BB_CMPLX_COS_SIGNAL_C4_USE_EXPLICIT_DEALLOCATION)
#define PM_BB_CMPLX_COS_SIGNAL_C4_USE_EXPLICIT_DEALLOCATION 1 
#endif 

#define PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START \
print*, "[EXECUTION-TRACE: START]";\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;\
print*, "TSC-Start:", rdtsc_wrap()

#define PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END \
print*, "TSC-End:", rdtsc_wrap();\
print*, "[EXECUTION-TRACE: END]" ;\
print*, "File:     ", __FILE__ ;\
print*, "Procedure:", sub_name ;\
print*, "LOC:      ", __LINE__ ;

! Helper derived type for holding random-distribution parameters
type, public :: cmplx_cos_signal_r4_pdf_params_t 
    SEQUENCE
    real(kind=sp)    :: random_gamma_s_r
    real(kind=sp)    :: random_gamma_s_i
    logical(kind=i4) :: random_gamma_first 
    real(kind=sp)    :: random_gamma1_s_r
    real(kind=sp)    :: random_gamma1_s_i
    logical(kind=i4) :: random_gamma1_first 
    real(kind=sp)    :: random_gamma2_s_r
    real(kind=sp)    :: random_gamma2_s_i
    logical(kind=i4) :: random_gamma2_first
    integer(kind=i4) :: rand_chisq_ndf 
    logical(kind=i4) :: rand_chisq_first 
    real(kind=sp)    :: random_weibull_a_r 
    real(kind=sp)    :: random_weibull_a_i 
    real(kind=sp)    :: random_beta_aa_r 
    real(kind=sp)    :: random_beta_aa_i
    real(kind=sp)    :: random_beta_bb_r 
    real(kind=sp)    :: random_beta_bb_i 
    logical(kind=i4) :: random_beta_first 
    real(kind=sp)    :: rand_poisson_mu_r 
    real(kind=sp)    :: rand_poisson_mu_i
    logical(kind=i4) :: rand_poisson_first
    real(kind=sp)    :: rand_von_Misses_k_r 
    real(kind=sp)    :: rand_von_Misses_k_i 
    logical(kind=i4) :: rand_von_Misses_first 
end type cmplx_cos_signal_r4_pdf_params_t 

type, public :: pm_bb_cmplx_cos_signal_c4_t 
    integer(kind=i4) :: m_nsamples 
    integer(kind=i4) :: m_nK 
    real(kind=sp)    :: m_w0 
    real(kind=sp)    :: m_ph 
    real(kind=sp)    :: m_n 
    real(kind=sp)    :: m_A 
    real(kind=sp)    :: m_P 
    logical(kind=i4) :: m_build_state 
    complex(kind=sp), dimension(:), allocatable :: m_signal_samples 
#if defined(__INTEL_COMPILER) || defined(__ICC)
       !dir$ attribute align : 64 :: m_signal_samples
#endif
end type pm_bb_cmplx_cos_signal_c4_t

contains 


subroutine construct(this,nsamples,nK,w0,ph, &
                     n,A,P,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "construct"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)    :: this 
    integer(kind=i4),                  intent(in),value :: nsamples 
    integer(kind=i4),                  intent(in),value :: nK 
    real(kind=sp),                     intent(in),value :: w0 
    real(kind=sp),                     intent(in),value :: ph 
    real(kind=sp),                     intent(in),value :: n 
    real(kind=sp),                     intent(in),value :: A 
    real(kind=sp),                     intent(in),value :: P 
    integer(kind=i4),                  intent(inout)    :: ier 
    ier = -9999
    if(this.m_build_state .eq. .true.) then 
        ier = -1
        return 
    end if 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif 
    this.m_nsamples = nsamples 
    this.m_nK       = nK 
    this.m_w0       = w0 
    this.m_ph       = ph 
    this.m_n        = n 
    this.m_A        = A 
    this.m_P        = P 
    PM_BB_CMPLX_COS_SIGNAL_C4_SAFE_ALLOC(this.m_signal_samples, (this.m_nsamples))
    this.m_build_state = .true. 
    ier = 0
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif
end subroutine construct 

subroutine destroy(this,to_clear_values,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "destroy"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)    :: this
    logical(kind=i4),                  intent(in)       :: to_clear_values 
    integer(kind=i4),                  intent(inout)    :: ier 
    ier = -9999_i4
    if((this.m_build_state .eq. .false.) .AND. &
       (allocated(this.m_signal_samples) .eq. .false.)) then
        ier = -1_i4 
        return 
    end if 
    !if(this.m_build_state .eq. .false.) then 
    !    ier = -1_i4
    !    return 
    !end if
    !if(allocated(this.m_signal_samples) .eq. .false. ) then 
    !    ier = -2_i4
    !    return 
    !end if 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    PM_BB_CMPLX_COS_SIGNAL_C4_SAFE_DEALLOC(this.m_signal_samples)
    if(to_clear_values .eq. .true.) then 
       this.m_nsamples = 0_i4 
       this.m_nK       = 0_i4 
       this.m_w0       = 0.0_sp 
       this.m_ph       = 0.0_sp 
       this.m_n        = 0.0_sp 
       this.m_A        = 0.0_sp 
       this.m_P        = 0.0_sp 
    end if 
    this.m_build_state = .false.
    ier = 0_i4
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif
end subroutine destroy 

subroutine clear_samples(this,use_memset,def_c4,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "clear_samples"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)     :: this
    logical(kind=i4),                  intent(in), value :: use_memset 
    complex(kind=sp),                  intent(in), value :: def_c4 
    integer(kind=i4),                  intent(inout)     :: ier 
    integer(kind=i4), automatic :: i,j 
    ier = -9999_i4 
    if((this.m_build_state .eq. .false.) .AND. &
       (allocated(this.m_signal_samples) .eq. .false.)) then
        ier = -1_i4 
        return 
    end if
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = def_c4 
    else 
       do i=1,iand(this.m_nsamples,inot(3_i4)),4_i4
          this.m_signal_samples(i+0_i4) = def_c4 
          this.m_signal_samples(i+1_i4) = def_c4 
          this.m_signal_samples(i+2_i4) = def_c4 
          this.m_signal_samples(i+3_i4) = def_c4 
       end do 
       do j=i,this.m_nsamples 
          this.m_signal_samples(j) = def_c4 
       end do 
    end if 
    ier = 0
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END
#endif
end subroutine clear_samples

subroutine copy_create(this,other,use_memset,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "copy_create"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)     :: this
    type(pm_bb_cmplx_cos_signal_c4_t), intent(in)        :: other 
    logical(kind=i4),                  intent(in), value :: use_memset 
    integer(kind=i4),                  intent(inout)     :: ier 
    complex(kind=sp), automatic :: c0
    complex(kind=sp), automatic :: c1 
    complex(kind=sp), automatic :: c2 
    complex(kind=sp), automatic :: c3 
    integer(kind=i4), automatic :: i,j 
    ier = -9999_i4 
    if(LOC(this) .eq. LOC(other)) then 
       ier = -1_i4 
       return 
    end if 
    if(this.m_build_state .eq. .true.) then 
       ier = -2_i4
       return 
    end if 
    if(other.m_build_state .eq. .false.) then 
       ier = -3_i4 
       return 
    end if 
    if(allocated(other.m_signal_samples) .eq. .false.) then 
       ier = -4_i4 
       return 
    end if 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    this.m_nsamples = other.m_nsamples 
    this.m_nK       = other.m_nK 
    this.m_w0       = other.m_w0 
    this.m_ph       = other.m_ph 
    this.m_n        = other.m_n 
    this.m_A        = other.m_A 
    this.m_P        = other.m_P 
    PM_BB_CMPLX_COS_SIGNAL_C4_SAFE_ALLOC(this.m_signal_samples, (this.m_nsamples))
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = other.m_signal_samples 
    else 
       do i=1,iand(this.m_nsamples,inot(3_i4)),4_i4
          c0 = other.m_signal_samples(i+0_i4)
          c1 = other.m_signal_samples(i+1_i4)
          c2 = other.m_signal_samples(i+2_i4)
          c3 = other.m_signal_samples(i+3_i4)
          this.m_signal_samples(i+0_i4) = c0 
          this.m_signal_samples(i+1_i4) = c1 
          this.m_signal_samples(i+2_i4) = c2 
          this.m_signal_samples(i+3_i4) = c3 
       end do 
       do j=i,this.m_nsamples 
          c0 = other.m_signal_samples(j)
          this.m_signal_samples(j) = c0 
       end do 
    end if 
    this.m_build_state = .true. 
    ier = 0
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif   
end subroutine copy_create 

subroutine copy_assign(this,other,use_memset,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "copy_assign"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)     :: this
    type(pm_bb_cmplx_cos_signal_c4_t), intent(in)        :: other 
    logical(kind=i4),                  intent(in), value :: use_memset 
    integer(kind=i4),                  intent(inout)     :: ier 
    complex(kind=sp), automatic :: c0
    complex(kind=sp), automatic :: c1 
    complex(kind=sp), automatic :: c2 
    complex(kind=sp), automatic :: c3 
    integer(kind=i4), automatic :: i,j 
    ier = -9999_i4 
    if(LOC(this) .eq. LOC(other)) then 
       ier = -1_i4 
       return 
    end if 
    if((this.m_build_state .eq. .false.) .AND. &
       (allocated(this.m_signal_samples) .eq. .false.)) then
        ier = -1_i4 
        return 
    end if
    if((other.m_build_state .eq. .false.) .AND. &
       (allocated(other.m_signal_samples) .eq. .false.)) then
        ier = -1_i4 
        return 
    end if
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    this.m_nsamples = other.m_nsamples 
    this.m_nK       = other.m_nK 
    this.m_w0       = other.m_w0 
    this.m_ph       = other.m_ph 
    this.m_n        = other.m_n 
    this.m_A        = other.m_A 
    this.m_P        = other.m_P 
    deallocate(this.m_signal_samples)
    allocate(this.m_signal_samples(1:this.m_nsamples))
    if(use_memset .eq. .true.) then 
       this.m_signal_samples = other.m_signal_samples 
    else 
       do i=1,iand(this.m_nsamples,inot(3_i4)),4_i4
          c0 = other.m_signal_samples(i+0_i4)
          c1 = other.m_signal_samples(i+1_i4)
          c2 = other.m_signal_samples(i+2_i4)
          c3 = other.m_signal_samples(i+3_i4)
          this.m_signal_samples(i+0_i4) = c0 
          this.m_signal_samples(i+1_i4) = c1 
          this.m_signal_samples(i+2_i4) = c2 
          this.m_signal_samples(i+3_i4) = c3 
       end do 
       do j=i,this.m_nsamples 
          c0 = other.m_signal_samples(j)
          this.m_signal_samples(j) = c0 
       end do 
    end if 
    this.m_build_state = .true. 
    ier = 0
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif   
end subroutine copy_assign 

subroutine show(this,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif           
                            
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "show"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(in)     :: this
    integer(kind=i4),                  intent(inout)  :: ier
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    ier = -9999
    if(this.m_build_state .eq. .false.) then 
       ier = -1_i4 
       return 
    end if 
    print*, "object-vaddr=", LOC(this)
    print*, "object-size =", SIZEOF(this), "bytes"
    call check_allocation_stats_complex1D(this.m_signal_samples,"m_nsamples")
    print*, "====================================================================================="
    print*, "[INFO]: Phase Modulated Base-Band Complex Cosine (Waveform) Signal -- Scalar Members:"
    print*, "m_nsamples           =",  this.m_nsamples  
    print*, "m_nK                 =",  this.m_nK 
    print*, "m_w0                 =",  this.m_w0 
    print*, "m_ph                 =",  this.m_ph  
    print*, "m_n                  =",  this.m_n 
    print*, "m_A                  =",  this.m_A  
    print*, "m_P                  =",  this.m_P 
    print*, "m_build_state        =",  this.m_build_state 
    print*, "======================================================================================"
    ier = 0
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif
    contains 
    subroutine check_allocation_stats_complex1D(array,name)
                       implicit none
                       complex(kind=sp), allocatable, dimension(:), intent(in) :: array 
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
    end subroutine check_allocation_stats_complex1D
end subroutine show 

function cos_sample(A,n,t,invT) result(sample)
    real(kind=sp), intent(in), value :: A 
    real(kind=sp), intent(in), value :: n 
    real(kind=sp), intent(in), value :: t 
    real(kind=sp), intent(in), value :: invT 
    real(kind=sp), parameter :: C628318530717958647692528676656 = 6.28318530717958647692528676656_sp 
    real(kind=sp), automatic :: arg 
    real(kind=sp) :: sample 
    arg     = n*C628318530717958647692528676656*t*invT 
    sample  = 0.5_sp*A*(1.0_sp-cos(arg))
end function cos_sample 

#if 0
TO DO: Update to include the full complex exponential
\(I=\cos (\omega _{c}t+\phi _{m})=\cos (\phi _{m})\cos (\omega _{c}t)-\sin (\phi _{m})\sin (\omega _{c}t)\)
\(Q=\sin (\omega _{c}t+\phi _{m})=\sin (\phi _{m})\cos (\omega _{c}t)+\cos (\phi _{m})\sin (\omega _{c}t)\)
#endif 

subroutine generate_with_user_data(this,user_symbols,n_T,n_K,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif                         
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "generate_with_user_data"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)     :: this
    real(kind=sp), dimension(:,:),     intent(in)        :: user_symbols 
    integer(kind=i4),                  intent(in), value :: n_T 
    integer(kind=i4),                  intent(in), value :: n_K 
    integer(kind=i4),                  intent(inout)     :: ier 
    ! Locals 
    complex(kind=sp), automatic :: sum, sample 
    real(kind=sp),    automatic :: T, invTc
    real(kind=sp),    automatic :: invT, t_i 
    real(kind=sp),    automatic :: carrier_arg,cos_carrier, sin_carrier 
    real(kind=sp),    automatic :: k_j, arg,sym  
    real(kind=sp),    automatic :: pm, I, Q 
    integer(kind=i4), automatic :: i,j 
    real(kind=sp),    parameter :: C141421356237309504880168872421  = 1.41421356237309504880168872421_sp 
    real(kind=sp),    parameter :: C6283185307179586476925286766559 = 6.283185307179586476925286766559_sp
    ier = -9999_i4 
    if((this.m_build_state .eq. .false.) .AND. &
       (allocated(this.m_signal_samples) .eq. .false.)) then
        ier = -1_i4 
        return 
    end if
    if((this.m_nsamples /= n_T) .OR. &
       (this.m_nK /= n_K)) then 
       ier = -2_i4 
       return 
    end if 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    T     = nint(this.m_nsamples,kind=i4)
    invTc = 1.0_sp/T 
    invT  = this.m_P/T 
    do i=1_i4,n_T 
        t_i = real(i,kind=sp)
        carrier_arg = this.m_ph+(C6283185307179586476925286766559* &
                                this.m_w0*t_i*invTc)
        cos_carrier = cos(carrier_arg)
        sin_carrier = sin(carrier_arg)
        sum = (0.0_sp,0.0_sp)
        do j=1,n_K 
           k_j = real(j,kind=sp)
           arg = t_i-k_j*T 
           sym = user_symbols(j,i)
           pm  = cos_sample(this.m_A,this.m_n,arg,invT)*sym 
           I   = cos(pm)*cos_carrier 
           Q   = sin(pm)*sin_carrier 
           sample = cmplx(I,-Q)
           sum = sum+sample
        end do 
        this.m_signal_samples(i) = sum 
    end do 
    ier = 0_i4 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif
end subroutine generate_with_user_data

subroutine generate_with_user_data_v2(this,user_symbols,n_T,n_K,ier)
    implicit none   
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       interface 
              function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                  use iso_c_binding, only : c_long_long 
                  integer(c_long_long) :: rdtsc_wrap 
              end function rdtsc_wrap 
       end interface
#endif                         
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
       character(*), parameter :: sub_name = "generate_with_user_data_v2"
#endif 
    type(pm_bb_cmplx_cos_signal_c4_t), intent(inout)     :: this
    real(kind=sp), dimension(:,:),     intent(in)        :: user_symbols 
    integer(kind=i4),                  intent(in), value :: n_T 
    integer(kind=i4),                  intent(in), value :: n_K 
    integer(kind=i4),                  intent(inout)     :: ier 
    ! Locals 
    complex(kind=sp), automatic :: sum, sample 
    real(kind=sp),    automatic :: T, invTc
    real(kind=sp),    automatic :: invT, t_i 
    real(kind=sp),    automatic :: carrier_arg, cos_carrier, sin_carrier 
    real(kind=sp),    automatic :: k_j, arg, sym  
    real(kind=sp),    automatic :: pm, I, Q 
    integer(kind=i4), automatic :: ii, jj 
    real(kind=sp),    parameter :: C6283185307179586476925286766559 = 6.283185307179586476925286766559_sp

    ier = -9999_i4 
    if(this.m_build_state .eq. .false.) then 
       ier = -1_i4 
       return 
    end if 
    if(allocated(this.m_signal_samples) .eq. .false.) then 
       ier = -2_i4 
       return 
    end if 
    if((this.m_nsamples /= n_T) .OR. &
       (this.m_nK /= n_K)) then 
       ier = -3_i4 
       return 
    end if 
    if((n_T <= 0_i4) .OR. (n_K <= 0_i4)) then 
       ier = -4_i4 
       return 
    end if 
    if((size(user_symbols, 1) < n_K) .OR. &
       (size(user_symbols, 2) < n_T)) then 
       ier = -5_i4 
       return 
    end if 

#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_START
#endif
    T     = real(this.m_nsamples, kind=sp)
    invTc = 1.0_sp/T 
    invT  = this.m_P/T 
    do ii = 0_i4, n_T - 1_i4 
        t_i = real(ii, kind=sp)
        carrier_arg = this.m_ph + (C6283185307179586476925286766559 * &
                                   this.m_w0 * t_i * invTc)
        cos_carrier = cos(carrier_arg)
        sin_carrier = sin(carrier_arg)
        sum = (0.0_sp, 0.0_sp)
        do jj = 0_i4, n_K - 1_i4 
           k_j = real(jj, kind=sp)
           arg = t_i - k_j*T 
           sym = user_symbols(jj + 1_i4, ii + 1_i4)
           pm  = cos_sample(this.m_A, this.m_n, arg, invT) * sym 
           I   = cos(pm)*cos_carrier 
           Q   = sin(pm)*sin_carrier 
           sample = cmplx(I, -Q, kind=sp)
           sum = sum + sample
        end do 
        this.m_signal_samples(ii + 1_i4) = sum 
    end do 
    ier = 0_i4 
#if (PM_BB_CMPLX_COS_SIGNAL_C4_EXEC_TRACE) == 1
     PM_BB_CMPLX_COS_SIGNAL_C4_TRACE_END 
#endif
end subroutine generate_with_user_data_v2 



end module mod_pm_bb_cmplx_cos_signal_c4