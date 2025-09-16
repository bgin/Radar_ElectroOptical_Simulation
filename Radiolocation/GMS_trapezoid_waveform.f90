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

module mod_trapezoid_waveform

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_trapezoid_waveform
 !                         
 !          
 !          Purpose:
 !                       TRapezoid waveform generating functions
 !                        
 !          History:
 !                        Date: 09-09-2025
 !                        Time: 09:22AM GMT+2
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
 !                     Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания
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
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_FULLVER =   &
            1000*TRAPEZOID_WAVEFORM_MAJOR+100*TRAPEZOID_WAVEFORM_MINOR+10*TRAPEZOID_WAVEFORM_MICRO
     ! Module creation date
     character(*),        parameter :: TRAPEZOID_WAVEFORM_CREATE_DATE = "09-09-2025 09:22AM +00200 (TUE 09 SEP 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: TRAPEZOID_WAVEFORM_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: TRAPEZOID_WAVEFORM_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: TRAPEZOID_WAVEFORM_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: TRAPEZOID_WAVEFORM_SYNOPSIS    = "Trapezoid waveform generating functions."

     character(*),        parameter :: TRAPEZOID_WAVEFORM_REFERENCE   = "Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания"

!===========================================================================================================================================================================!

#define TRAPEZOID_WAVEFORM_SAFE_ALLOC(mem,size) if(allocated(mem)) deallocate(mem); allocate(mem size)
#define TRAPEZOID_WAVEFORM_SAFE_DEALLOC(mem)    if(allocated(mem)) deallocate(mem)

#if !defined(TRAPEZOID_WAVEFORM_EXEC_TRACE)
#define TRAPEZOID_WAVEFORM_EXEC_TRACE 1
#endif

#if !defined(TRAPEZOID_WAVEFORM_USE_EXPLICIT_DEALLOCATION)
#define TRAPEZOID_WAVEFORM_USE_EXPLICIT_DEALLOCATION 1 
#endif 

#define TRAPEZOID_WAVEFORM_TRACE_START  \
print*, "[EXECUTION-TRACE: START]";     \
print*, "File:     ", __FILE__ ;        \
print*, "Procedure:", sub_name ;        \
print*, "LOC:      ", __LINE__ ;        \
print*, "TSC-Start:", rdtsc_wrap()

#define TRAPEZOID_WAVEFORM_TRACE_END \
print*, "[EXECUTION-TRACE: END]" ;    \
print*, "TSC-End:", rdtsc_wrap()

     
      ! Helper derived type for holding random-distribution parameters
     type, public :: tw_pdf_params_t 
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

     end type tw_pdf_params_t 

      type, public :: trapezoid_waveform_t 
            
            integer(kind=i4)                         :: n_samples__
            integer(kind=i4)                         :: n_waves__ 
            integer(kind=i4)                         :: n_param_a__ 
            integer(kind=i4)                         :: n_param_l__
            integer(kind=i4)                         :: n_param_c__ 
            integer(kind=i4)                         :: n_param_m__ 
            logical(kind=i4)                         :: build_state__ 
            real(kind=sp), dimension(:), allocatable :: tw_samples__
#if defined(__INTEL_COMPILER) || defined(__ICC)
            !dir$ attribute align : 64 :: tw_samples
#endif          
      end type trapezoid_waveform_t


      contains 

      subroutine construct(trapezw,n_samples,n_waves,n_param_a, &
                           n_param_l,n_param_c,n_param_m)
                 implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "construct"
#endif 
               type(trapezoid_waveform_t),    intent(out)      :: trapezw  
               integer(kind=i4),              intent(in),value :: n_samples
               integer(kind=i4),              intent(in),value :: n_waves 
               integer(kind=i4),              intent(in),value :: n_param_a 
               integer(kind=i4),              intent(in),value :: n_param_l 
               integer(kind=i4),              intent(in),value :: n_param_c 
               integer(kind=i4),              intent(in),value :: n_param_m 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
      TRAPEZOID_WAVEFORM_TRACE_START
#endif        
               if(trapezw.build_state__ .eq. .true.) return     
               trapezw.n_samples__ = n_samples 
               trapezw.n_waves__   = n_waves 
               trapezw.n_param_a__ = n_param_a 
               trapezw.n_param_l__ = n_param_l 
               trapezw.n_param_c__ = n_param_c 
               trapezw.n_param_m__ = n_param_m 
               TRAPEZOID_WAVEFORM_SAFE_ALLOC(trapezw.tw_samples__, (trapezw.n_samples__))
               trapezw.build_state__ = .true. 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
      TRAPEZOID_WAVEFORM_TRACE_END
#endif
      end subroutine construct 

      subroutine destroy(trapezw,clear_values)
          implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "destroy"
#endif 
           type(trapezoid_waveform_t),       intent(inout)     ::    trapezw
           logical(kind=i4),                 intent(in), value ::    clear_values 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
     TRAPEZOID_WAVEFORM_TRACE_START
#endif
            if(trapezw.build_state__ .eq. .false.) return 
            TRAPEZOID_WAVEFORM_SAFE_DEALLOC(trapezw.tw_samples__)
            if(clear_values .eq. .true.) then 
               trapezw.n_samples__ = 0 
               trapezw.n_waves__   = 0
               trapezw.n_param_a__ = 0
               trapezw.n_param_l__ = 0
               trapezw.n_param_c__ = 0
               trapezw.n_param_m__ = 0 
             end if 
             trapezw.build_state__ = .false. 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
      TRAPEZOID_WAVEFORM_TRACE_END
#endif 
      end subroutine destroy 

      subroutine clear(trapezw,fill,use_memset)
          implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "clear"
#endif 
           type(trapezoid_waveform_t),       intent(inout)     ::  trapezw
           real(kind=sp),                    intent(in),value  ::  fill 
           logical(kind=i4),                 intent(in),value  ::  use_memset 
           integer(kind=i4), automatic :: i__,j__ 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
     TRAPEZOID_WAVEFORM_TRACE_START
#endif
            if(trapezw.build_state__ .eq. .false.) return  
            if(use_memset .eq. .true.) then 
               trapezw.tw_samples__ = fill 
            else 
               do i__=1,iand(trapezw.n_samples__-1,inot(3)),4
                  trapezw.tw_samples__(i__+0) = fill 
                  trapezw.tw_samples__(i__+1) = fill 
                  trapezw.tw_samples__(i__+2) = fill 
                  trapezw.tw_samples__(i__+3) = fill 
               end do 
               do j__=i__,trapezw.n_samples__ 
                   trapezw.tw_samples__(j__) = fill 
               end do 
            end if   
            trapezw.n_samples__ = 0 
            trapezw.n_waves__   = 0
            trapezw.n_param_a__ = 0
            trapezw.n_param_l__ = 0
            trapezw.n_param_c__ = 0
            trapezw.n_param_m__ = 0    
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
     TRAPEZOID_WAVEFORM_TRACE_END
#endif                
      end subroutine clear 


      subroutine copy(this,other)
          implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "copy"
#endif 
           type(trapezoid_waveform_t),    intent(out) :: this 
           type(trapezoid_waveform_t),    intent(in)  :: other 
           integer(kind=i4), automatic :: i__,j__ 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
     TRAPEZOID_WAVEFORM_TRACE_START
#endif
           if(LOC(this)==LOC(other)) return 
           if(this.build_state__ .eq. .true. .or. &
              other.build_state__.eq. .false.) return 
           this.n_samples__ = other.n_samples__ 
           this.n_waves__   = other.n_waves__ 
           this.n_param_a__ = other.n_param_a__ 
           this.n_param_c__ = other.n_param_c__
           this.n_param_l__ = other.n_param_l__ 
           this.n_param_m__ = other.n_param_m__
           TRAPEZOID_WAVEFORM_SAFE_ALLOC(this.tw_samples__, (this.n_samples__))
            
           do i__=1,iand(this.n_samples__-1,inot(3)),4
                  this.tw_samples__(i__+0) = other.tw_samples__(i__+0)
                  this.tw_samples__(i__+1) = other.tw_samples__(i__+1)
                  this.tw_samples__(i__+2) = other.tw_samples__(i__+2) 
                  this.tw_samples__(i__+3) = other.tw_samples__(i__+3) 
           end do 
           do j__=i__,this.n_samples__ 
                   this.tw_samples__(j__) = other.tw_samples__(j__)
           end do    
           this.build_state__ = .true. 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
     TRAPEZOID_WAVEFORM_TRACE_END
#endif               
      end subroutine copy 

      subroutine show(trapezw)
          implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "show"
#endif 
          type(trapezoid_waveform_t),   intent(in) :: trapezw 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          TRAPEZOID_WAVEFORM_TRACE_START
#endif        
          print*, "object-vaddr=", LOC(trapezw)
          print*, "object-size =", SIZEOF(trapezw), "bytes"
          call check_allocation_stats_real1D(trapezw.tw_samples__,"tw_samples__")
          print*, "[INFO]: trapezoid_waveform_t -- scalar members:"
          print*, "n_samples__=", trapezw.n_samples__ 
          print*, "n_waves__  =", trapezw.n_waves__
          print*, "n_param_a__=", trapezw.n_param_a__
          print*, "n_param_l__=", trapezw.n_param_l__
          print*, "n_param_c__=", trapezw.n_param_c__
          print*, "n_param_m__=", trapezw.n_param_m__
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
          TRAPEZOID_WAVEFORM_TRACE_END
#endif
          contains 
          subroutine check_allocation_stats_real1D(array,name)
                       implicit none
                       real(kind=sp), allocatable, dimension(:), intent(in) :: array 
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

      subroutine single_trapezoid_wave(samples,n_samp,a,l,c,m)
                                       
                   implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   interface 
                      function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                       end function rdtsc_wrap 
                   end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   character(*), parameter :: sub_name = "single_trapezoid_wave"
#endif 
                   real(kind=sp),  dimension(n_samp), intent(out)       :: samples 
                   integer(kind=i4),                  intent(in), value :: n_samp
                   real(kind=sp),                     intent(in), value :: a 
                   real(kind=sp),                     intent(in), value :: l 
                   real(kind=sp),                     intent(in), value :: c 
                   real(kind=sp),                     intent(in), value :: m 
                   real(kind=sp),  parameter :: C314159265358979323846264338328 =  &
                                                3.14159265358979323846264338328_sp 
                   real(kind=sp),    automatic :: a_over_PI 
                   real(kind=sp),    automatic :: PI_over_m 
                   real(kind=sp),    automatic :: t__i 
                   real(kind=sp),    automatic :: arg 
                   real(kind=sp),    automatic :: t_as
                   real(kind=sp),    automatic :: t_ac 
                   integer(kind=i4), automatic :: i__ 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_START
#endif      
                   a_over_PI = a/C314159265358979323846264338328 
                   PI_over_m = C314159265358979323846264338328/m 
                   do i__ = 0, n_samp-1 
                      t__i   = real(i__,kind=sp)
                      arg    = PI_over_m*t__i+l 
                      t_as   = asin(sin(arg))
                      t_ac   = acos(cos(arg))
                      samples(i__) = a_over_PI*(t_as+t_ac)-5.0_sp+c 
                   end do
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_END
#endif 
      end subroutine single_trapezoid_wave

      subroutine series_of_trapezoid_wave(samples,n_samp,n_waves,a,l,c,m,shaping)
                   implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   interface 
                      function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                       end function rdtsc_wrap 
                   end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   character(*), parameter :: sub_name = "series_of_trapezoid_wave"
#endif 
                   real(kind=sp),  dimension(n_samp), intent(out)       :: samples 
                   integer(kind=i4),                  intent(in), value :: n_samp
                   integer(kind=i4),                  intent(in), value :: n_waves 
                   real(kind=sp),                     intent(in), value :: a 
                   real(kind=sp),                     intent(in), value :: l 
                   real(kind=sp),                     intent(in), value :: c 
                   real(kind=sp),                     intent(in), value :: m 
                   integer(kind=i4),                  intent(in), value :: shaping
                   real(kind=sp),  parameter :: C314159265358979323846264338328 =  &
                                                3.14159265358979323846264338328_sp 
                   real(kind=sp),    automatic :: a_over_PI 
                   real(kind=sp),    automatic :: PI_over_m 
                   real(kind=sp),    automatic :: t__i 
                   real(kind=sp),    automatic :: t__j 
                   real(kind=sp),    automatic :: arg 
                   real(kind=sp),    automatic :: t_as
                   real(kind=sp),    automatic :: t_ac 
                   real(kind=sp),    automatic :: sum 
                   integer(kind=i4), automatic :: i__ 
                   integer(kind=i4), automatic :: j__ 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_START
#endif      
                   a_over_PI = a/C314159265358979323846264338328 
                   PI_over_m = C314159265358979323846264338328/m 
                   select case (shaping)
                        case (0)
                             do i__ = 0, n_samp-1 
                                  t__i = real(i__,kind=sp)
                                  sum  = 0.0_sp 
                                  do j__ = 1, n_waves 
                                      arg  = PI_over_m*t__i+l 
                                      t_as = asin(sin(arg))
                                      t_ac = acos(cos(arg))
                                      sum  = sum+a_over_PI*(t_as+t_ac)-5.0_sp+c 
                                  end do 
                                  samples(i__) = sum 
                             end do 
                         case (1)
                              do i__ = 0, n_samp-1 
                                  t__i = real(i__,kind=sp)
                                  sum  = 0.0_sp 
                                  do j__ = 1, n_waves 
                                     t__j = real(j__,kind=sp)
                                      arg  = PI_over_m*(t__i+t__j)+l 
                                      t_as = asin(sin(arg))
                                      t_ac = acos(cos(arg))
                                      sum  = sum+a_over_PI*(t_as+t_ac)-5.0_sp+c 
                                  end do 
                                  samples(i__) = sum 
                             end do 
                         case (2)
                             do i__ = 0, n_samp-1 
                                  t__i = real(i__,kind=sp)
                                  sum  = 0.0_sp 
                                  do j__ = 1, n_waves 
                                     t__j = real(j__,kind=sp)
                                      arg  = PI_over_m*t__i*t__j+l 
                                      t_as = asin(sin(arg))
                                      t_ac = acos(cos(arg))
                                      sum  = sum+a_over_PI*(t_as+t_ac)-5.0_sp+c 
                                  end do 
                                  samples(i__) = sum 
                             end do 
                         case default 
                             return 
                   end select 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_END
#endif
      end subroutine series_of_trapezoid_wave

      subroutine series_of_trapezoid_wave_coded(samples,code_seq,n_samp,n_waves,a,l,c,m,sequence)
                   implicit none   
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   interface 
                      function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                       end function rdtsc_wrap 
                   end interface
#endif           
                            
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   character(*), parameter :: sub_name = "series_of_trapezoid_wave_coded"
#endif 
                   real(kind=sp),  dimension(n_samp), intent(out)       :: samples 
                   real(kind=sp),  dimension(n_samp), intent(in)        :: code_seq 
                   integer(kind=i4),                  intent(in), value :: n_samp
                   integer(kind=i4),                  intent(in), value :: n_waves 
                   real(kind=sp),                     intent(in), value :: a 
                   real(kind=sp),                     intent(in), value :: l 
                   real(kind=sp),                     intent(in), value :: c 
                   real(kind=sp),                     intent(in), value :: m 
                   integer(kind=i4),                  intent(in), value :: sequence  
                   real(kind=sp),  parameter :: C314159265358979323846264338328 =  &
                                                3.14159265358979323846264338328_sp 
                   real(kind=sp),    automatic :: a_over_PI 
                   real(kind=sp),    automatic :: PI_over_m 
                   real(kind=sp),    automatic :: t__i 
                   real(kind=sp),    automatic :: t__j 
                   real(kind=sp),    automatic :: arg 
                   real(kind=sp),    automatic :: t_as
                   real(kind=sp),    automatic :: t_ac 
                   real(kind=sp),    automatic :: code_val 
                   real(kind=sp),    automatic :: sum 
                   integer(kind=i4), automatic :: i__ 
                   integer(kind=i4), automatic :: j__ 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_START
#endif      
                   a_over_PI = a/C314159265358979323846264338328 
                   PI_over_m = C314159265358979323846264338328/m 
                   select case (sequence)
                      case (0) ! code_sequence processed by samples loop 
                          do i__ = 0, n_samp-1 
                                  t__i = real(i__,kind=sp)
                                  code_val = code_seq(i__)
                                  sum      = 0.0_sp 
                                  do j__ = 1, n_waves 
                                     t__j = real(j__,kind=sp)
                                      arg  = PI_over_m*t__i*t__j+l 
                                      t_as = asin(sin(arg))
                                      t_ac = acos(cos(arg))
                                      sum  = sum+(code_val*a_over_PI*(t_as+t_ac)-5.0_sp+c) 
                                  end do 
                                  samples(i__) = sum 
                          end do 
                       case (1) ! code sequence processed by single wave summation loop 
                          do i__ = 0, n_samp-1 
                                  t__i = real(i__,kind=sp)
                                  sum  = 0.0_sp 
                                  do j__ = 1, n_waves 
                                      t__j     = real(j__,kind=sp)
                                      code_val = code_seq(j__)
                                      arg      = PI_over_m*t__i*t__j+l 
                                      t_as     = asin(sin(arg))
                                      t_ac     = acos(cos(arg))
                                      sum      = sum+(code_val*a_over_PI*(t_as+t_ac)-5.0_sp+c)
                                  end do 
                                  samples(i__) = sum 
                             end do 
                       case default 
                              return 
                    end select 
#if (TRAPEZOID_WAVEFORM_EXEC_TRACE) == 1
                   TRAPEZOID_WAVEFORM_TRACE_END
#endif
        end subroutine series_of_trapezoid_wave_coded          








end module mod_trapezoid_waveform