


module simd_memops


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'simd_memops'
 !          
 !          Purpose:
 !                      Explicit vectorization of memory movement (copy,memset,init)  subroutines
 !          History:
 !                        
 !                        Date: 21-11-2021
 !                        Time: 13:25 GMT+2
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
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,   only : i4,i8,sp,dp
     use mod_vectypes
     use omp_lib

     implicit none
     
     public 
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MOD_SIMD_MEMOPS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_SIMD_MEMOPS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_SIMD_MEMOPS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_SIMD_MEMOPS_FULLVER = 1000*MOD_SIMD_MEMOPS_MAJOR+100*MOD_SIMD_MEMOPS_MINOR+ &
                                             *MOD_SIMD_MEMOPS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SIMD_MEMOPS_CREATE_DATE = "21-11-2021 13:27 +00200 (SUN 21 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SIMD_MEMOPS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_SIMD_MEMOPS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_SIMD_MEMOPS_SYNOPSIS = "SIMD optimized memory movement subrotines"

    integer(kind=i4), parameter, private :: MEMMOVE_1ELEM        = 1
    integer(kind=i4), parameter, private :: MEMMOVE_16ELEMS      = 16
    integer(kind=i4), parameter, private :: MEMMOVE_32ELEMS      = 32
    integer(kind=i4), parameter, private :: MEMMOVE_64ELEMS      = 64
    integer(kind=i4), parameter, private :: MEMMOVE_128ELEMS     = 128
    integer(kind=i4), parameter, private :: MEMMOVE_256ELEMS     = 256
    integer(kind=i4), parameter, private :: YMM_LEN              = 8
    integer(kind=i4), parameter, private :: ZMM_LEN              = 16
    integer(kind=i4), parameter, private :: PAGE4KiB             = 4096
    integer(kind=i4), parameter, private :: MAX_FLOAT_PAGE4KiB   = 1024
    integer(kind=i4), parameter, private :: MAX_DOUBLE_PAGE_4KiB = 512

    
    
  contains

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine ymm8r4_memcpy_unroll8x(src,dst,n) !GCC$ ATTRIBUTES aligned(32) ::  ymm8r4_memcpy_unroll8x !GCC$ ATTRIBUTES hot ::  ymm8r4_memcpy_unroll8x 
#elif defined(__ICC) || defined(__INTEL_COMPILER)
    subroutine ymm8r4_memcpy_unroll8x(src,dst,n)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_memcpy_unroll8x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ymm8r4_memcpy_unroll8x
#endif
      real(kind=sp), allocatable, dimension(:), intent(in)  :: src
      real(kind=sp), allocatable, dimension(:), intent(out) :: dst
      integer(kind=i4),               intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 32 :: src
      !DIR$ ASSUME_ALIGNED : 32 :: dst
#endif
      type(YMM8r4_t), automatic :: ymm0,ymm1,ymm2,ymm3,ymm4, &
                                   ymm5,ymm6,ymm7
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm1
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm2
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm3
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm4
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm5
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm6
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm7
#endif
      integer(kind=i4), automatic :: i,ii,j
      !Executable code!!
      if(n < MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1, n, YMM_LEN
            call mm_prefetch(src(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
            call mm_prefetch(dst(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*8
              call mm_prefetch(src(i+YMM_LEN*2),FOR_K_PREFETCH_T1)
              call mm_prefetch(dst(i+YMM_LEN*2),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)              
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)   =  src(i+0+ii)
               dst(i+0+ii)  =  ymm0.v(ii)
               ymm1.v(ii)   =  src(i+8+ii)
               dst(i+8+ii)  =  ymm1.v(ii)
               ymm2.v(ii)   =  src(i+16+ii)
               dst(i+16+ii) =  ymm2.v(ii)
               ymm3.v(ii)   =  src(i+24+ii)
               dst(i+24+ii) =  ymm3.v(ii)
               ymm4.v(ii)   =  src(i+32+ii)
               dst(i+32+ii) =  ymm4.v(ii)
               ymm5.v(ii)   =  src(i+40+ii)
               dst(i+40+ii) =  ymm5.v(ii)
               ymm6.v(ii)   =  src(i+48+ii)
               dst(i+48+ii) =  ymm6.v(ii)
               ymm7.v(ii)   =  src(i+56+ii)
               dst(i+56+ii) =  ymm7.v(ii)
            end do
         end do
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine ymm8r4_memcpy_unroll8x


    #if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine ymm8r4_memcpy_unroll16x(src,dst,n) !GCC$ ATTRIBUTES aligned(32) ::  ymm8r4_memcpy_unroll16x !GCC$ ATTRIBUTES hot ::  ymm8r4_memcpy_unroll16x 
#elif defined(__ICC) || defined(__INTEL_COMPILER)
    subroutine ymm8r4_memcpy_unroll16x(src,dst,n)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_memcpy_unroll16x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ymm8r4_memcpy_unroll16x
#endif
      real(kind=sp), allocatable, dimension(:), intent(in)  :: src
      real(kind=sp), allocatable, dimension(:), intent(out) :: dst
      integer(kind=i4),               intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 32 :: src
      !DIR$ ASSUME_ALIGNED : 32 :: dst
#endif
      type(YMM8r4_t), automatic :: ymm0,ymm1,ymm2,ymm3,ymm4, &
                                   ymm5,ymm6,ymm7
      type(YMM8r4_t), automatic :: ymm8,ymm9,ymm10,ymm11,    &
                                   ymm12,ymm13,ymm14,ymm15
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm1
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm2
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm3
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm4
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm5
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm6
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm7
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm8
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm9
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm10
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm11
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm12
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm13
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm14
      !DIR$ ATTRIBUTES ALIGN : 32 :: ymm15
#endif
      integer(kind=i4), automatic :: i,ii,j
      !Executable code!!
      if(n < MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1, n, YMM_LEN
            call mm_prefetch(src(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
            call mm_prefetch(dst(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*16
              call mm_prefetch(src(i+YMM_LEN*2),FOR_K_PREFETCH_T1)
              call mm_prefetch(dst(i+YMM_LEN*2),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)              
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)   =  src(i+0+ii)
               dst(i+0+ii)  =  ymm0.v(ii)
               ymm1.v(ii)   =  src(i+8+ii)
               dst(i+8+ii)  =  ymm1.v(ii)
               ymm2.v(ii)   =  src(i+16+ii)
               dst(i+16+ii) =  ymm2.v(ii)
               ymm3.v(ii)   =  src(i+24+ii)
               dst(i+24+ii) =  ymm3.v(ii)
               ymm4.v(ii)   =  src(i+32+ii)
               dst(i+32+ii) =  ymm4.v(ii)
               ymm5.v(ii)   =  src(i+40+ii)
               dst(i+40+ii) =  ymm5.v(ii)
               ymm6.v(ii)   =  src(i+48+ii)
               dst(i+48+ii) =  ymm6.v(ii)
               ymm7.v(ii)   =  src(i+56+ii)
               dst(i+56+ii) =  ymm7.v(ii)
               ymm8.v(ii)   =  src(i+64+ii)
               dst(i+64+ii) =  ymm8.v(ii)
               ymm9.v(ii)   =  src(i+72+ii)
               dst(i+72+ii) =  ymm9.v(ii)
               ymm10.v(ii)  =  src(i+80+ii)
               dst(i+80+ii) =  ymm10.v(ii)
               ymm11.v(ii)  =  src(i+88+ii)
               dst(i+88+ii) =  ymm11.v(ii)
               ymm12.v(ii)  =  src(i+96+ii)
               dst(i+96+ii) =  ymm12.v(ii)
               ymm13.v(ii)  =  src(i+104+ii)
               dst(i+104+ii)=  ymm13.v(ii)
               ymm14.v(ii)  =  src(i+112+ii)
               dst(i+112+ii)=  ymm14.v(ii)
               ymm15.v(ii)  =  src(i+120+ii)
               dst(i+120+ii)=  ymm15.v(ii)
            end do
         end do
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine ymm8r4_memcpy_unroll16x













end module simd_memops
