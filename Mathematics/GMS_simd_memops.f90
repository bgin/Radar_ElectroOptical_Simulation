


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
      real(kind=sp), dimension(size), intent(in)  :: src
      real(kind=sp), dimension(size), intent(out) :: dst
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
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1, n, YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
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
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*8
              call mm_prefetch(src(i+YMM_LEN*32),FOR_K_PREFETCH_T1)
              call mm_prefetch(dst(i+YMM_LEN*32),FOR_K_PREFETCH_T1)
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
      end if
      
    end subroutine ymm8r4_memcpy_unroll8x













end module simd_memops
