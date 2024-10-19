


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
     use mod_kinds,   only : i4,sp,dp
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
    integer(kind=i4), parameter, private :: MAX_DOUBLE_PAGE4KiB = 512

    
    
  contains



    subroutine ymm8r4_memcpy_unroll8x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_memcpy_unroll8x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ymm8r4_memcpy_unroll8x
#endif
      use mod_vecconst, only : v16_2
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
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
            call mm_prefetch(src(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*8
              call mm_prefetch(src(i+YMM_LEN*16),FOR_K_PREFETCH_T1)
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
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine ymm8r4_memcpy_unroll8x




    subroutine ymm8r4_memcpy_unroll8x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_memcpy_unroll8x_4kib
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ymm8r4_memcpy_unroll8x_4kib
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
      integer(kind=i4), automatic :: i,ii,j,k
      real(kind=sp),    automatic, volatile :: t
      !Executable code!!
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(YMM_LEN-1)), YMM_LEN
            call mm_prefetch(src(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
            
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MAX_FLOAT_PAGE4KiB) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*8
              call mm_prefetch(src(i+YMM_LEN*16),FOR_K_PREFETCH_T1)
             
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
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n > MAX_FLOAT_PAGE4KiB) then
         do k = 1, n, MAX_FLOAT_PAGE4KiB
                t = src(k+MAX_FLOAT_PAGE4KiB)
            do i = k+128, k+MAX_FLOAT_PAGE4KiB, 64
               call mm_prefetch(src(i),FOR_K_PREFETCH_T1)
            end do
               do i = k, k+MAX_FLOAT_PAGE4KiB, YMM_LEN*8
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
         end do
         return
      end if
      
    end subroutine ymm8r4_memcpy_unroll8x_4kib





    subroutine ymm8r4_memcpy_unroll16x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
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
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(YMM_LEN-1)) , YMM_LEN
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(YMM_LEN-1)) , YMM_LEN
            call mm_prefetch(src(i+YMM_LEN*4),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:32) aligned(dst:32) linear(ii:1)
            do ii = 0, YMM_LEN-1 
               ymm0.v(ii)  = src(i+ii)
               dst(i+ii)   = ymm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(YMM_LEN-1)), YMM_LEN*16
              call mm_prefetch(src(i+YMM_LEN*32),FOR_K_PREFETCH_T1)
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
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=8, MIN=1, AVG=4
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine ymm8r4_memcpy_unroll16x





    subroutine zmm16r4_memcpy_unroll8x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: zmm16r4_memcpy_unroll8x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: zmm16r4_memcpy_unroll8x
#endif
      real(kind=sp), allocatable, dimension(:), intent(in)  :: src
      real(kind=sp), allocatable, dimension(:), intent(out) :: dst
      integer(kind=i4),               intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 64 :: src
      !DIR$ ASSUME_ALIGNED : 64 :: dst
#endif
      type(ZMM16r_t), automatic :: zmm0,zmm1,zmm2,zmm3,zmm4, &
                                   zmm5,zmm6,zmm7
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
#endif
      integer(kind=i4), automatic :: i,ii,j
      !Executable code!!
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1,iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(ZMM_LEN-1)), ZMM_LEN
            call mm_prefetch(src(i+ZMM_LEN*2),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
         !Remainder loop
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif         
         do j = i, n
            dst(j) = src(j)
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN*8
              call mm_prefetch(src(i+ZMM_LEN*8),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)              
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)   =  src(i+0+ii)
               dst(i+0+ii)  =  zmm0.v(ii)
               ymm1.v(ii)   =  src(i+16+ii)
               dst(i+16+ii) =  zmm1.v(ii)
               zmm2.v(ii)   =  src(i+32+ii)
               dst(i+32+ii) =  zmm2.v(ii)
               zmm3.v(ii)   =  src(i+48+ii)
               dst(i+48+ii) =  zmm3.v(ii)
               zmm4.v(ii)   =  src(i+64+ii)
               dst(i+64+ii) =  zmm4.v(ii)
               zmm5.v(ii)   =  src(i+80+ii)
               dst(i+80+ii) =  zmm5.v(ii)
               zmm6.v(ii)   =  src(i+96+ii)
               dst(i+96+ii) =  zmm6.v(ii)
               zmm7.v(ii)   =  src(i+112+ii)
               dst(i+112+ii)=  zmm7.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine zmm16r4_memcpy_unroll8x




    subroutine zmm16r4_memcpy_unroll16x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: zmm16r4_memcpy_unroll16x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: zmm16r4_memcpy_unroll16x
#endif
      real(kind=sp), allocatable, dimension(:), intent(in)  :: src
      real(kind=sp), allocatable, dimension(:), intent(out) :: dst
      integer(kind=i4),               intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 64 :: src
      !DIR$ ASSUME_ALIGNED : 64 :: dst
#endif
      type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3,zmm4, &
                                    zmm5,zmm6,zmm7
      type(ZMM16r4_t), automatic :: zmm8,zmm9,zmm10,zmm11,    &
                                   zmm12,zmm13,zmm14,zmm15
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm9
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm10
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm11
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm12
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm13
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm14
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm15
#endif
      integer(kind=i4), automatic :: i,ii,j
      !Executable code!!
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(ZMM_LEN-1)) , ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(ZMM_LEN-1)) , ZMM_LEN
            call mm_prefetch(src(i+ZMM_LEN*2),FOR_K_PREFETCH_T1)
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN*16
              call mm_prefetch(src(i+ZMM_LEN*16),FOR_K_PREFETCH_T1)
            !$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)              
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)   =  src(i+0+ii)
               dst(i+0+ii)  =  zmm0.v(ii)
               zmm1.v(ii)   =  src(i+16+ii)
               dst(i+16+ii) =  zmm1.v(ii)
               zmm2.v(ii)   =  src(i+32+ii)
               dst(i+32+ii) =  zmm2.v(ii)
               zmm3.v(ii)   =  src(i+48+ii)
               dst(i+48+ii) =  zmm3.v(ii)
               zmm4.v(ii)   =  src(i+64+ii)
               dst(i+64+ii) =  zmm4.v(ii)
               zmm5.v(ii)   =  src(i+80+ii)
               dst(i+80+ii) =  zmm5.v(ii)
               zmm6.v(ii)   =  src(i+96+ii)
               dst(i+96+ii) =  zmm6.v(ii)
               zmm7.v(ii)   =  src(i+112+ii)
               dst(i+112+ii)=  zmm7.v(ii)
               zmm8.v(ii)   =  src(i+128+ii)
               dst(i+128+ii)=  zmm8.v(ii)
               zmm9.v(ii)   =  src(i+144+ii)
               dst(i+144+ii)=  zmm9.v(ii)
               zmm10.v(ii)  =  src(i+160+ii)
               dst(i+160+ii)=  zmm10.v(ii)
               zmm11.v(ii)  =  src(i+176+ii)
               dst(i+176+ii)=  zmm11.v(ii)
               zmm12.v(ii)  =  src(i+192+ii)
               dst(i+192+ii)=  zmm12.v(ii)
               zmm13.v(ii)  =  src(i+208+ii)
               dst(i+208+ii)=  zmm13.v(ii)
               zmm14.v(ii)  =  src(i+224+ii)
               dst(i+224+ii)=  zmm14.v(ii)
               zmm15.v(ii)  =  src(i+240+ii)
               dst(i+240+ii)=  zmm15.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine zmm16r4_memcpy_unroll16x
    
    
    subroutine zmm16r4_memcpy_unroll32x(src,dst,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: zmm16r4_memcpy_unroll32x
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: zmm16r4_memcpy_unroll32x
#endif
      real(kind=sp), allocatable, dimension(:), intent(in)  :: src
      real(kind=sp), allocatable, dimension(:), intent(out) :: dst
      integer(kind=i4),               intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 64 :: src
      !DIR$ ASSUME_ALIGNED : 64 :: dst
#endif
      type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3,zmm4, &
                                    zmm5,zmm6,zmm7
      type(ZMM16r4_t), automatic :: zmm8,zmm9,zmm10,zmm11,    &
                                   zmm12,zmm13,zmm14,zmm15
      type(ZMM16r4_t), automatic :: zmm16,zmm17,zmm18,zmm19,  &
                                    zmm20,zmm21,zmm22,zmm23
      type(ZMM16r4_t), automatic :: zmm24,zmm25,zmm26,zmm27,  &
      type(ZMM16r4_t), automatic :: zmm28,zmm29,zmm30,zmm31                          
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm9
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm10
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm11
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm12
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm13
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm14
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm15
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm16
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm17
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm18
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm19
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm20
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm21
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm22
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm23
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm24
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm25
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm26
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm27
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm28
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm29
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm30
      !DIR$ ATTRIBUTES ALIGN : 64 :: zmm31
#endif
      integer(kind=i4), automatic :: i,ii,j
      !Executable code!!
      if(n <= MEMMOVE_1ELEM) then
         return
      else if(n <= MEMMOVE_16ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_32ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_64ELEMS) then
          do i = 1,iand(n,inot(ZMM_LEN-1)) , ZMM_LEN
!$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n <= MEMMOVE_128ELEMS) then
         do i = 1,iand(n,inot(ZMM_LEN-1)) , ZMM_LEN
            call mm_prefetch(src(i+ZMM_LEN*2),FOR_K_PREFETCH_T1)
           !$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)
            do ii = 0, ZMM_LEN-1 
               zmm0.v(ii)  = src(i+ii)
               dst(i+ii)   = zmm0.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i,n
            dst(j) = src(j)
         end do
         return
      else if(n > MEMMOVE_128ELEMS) then
         do i = 1, iand(n,inot(ZMM_LEN-1)), ZMM_LEN*32
              call mm_prefetch(src(i+ZMM_LEN*32),FOR_K_PREFETCH_T1)
            !$omp simd aligned(src:64) aligned(dst:64) linear(ii:1)              
            do ii = 0, ZMM_LEN-1
               zmm0.v(ii)   =  src(i+0+ii)
               dst(i+0+ii)  =  zmm0.v(ii)
               zmm1.v(ii)   =  src(i+16+ii)
               dst(i+16+ii) =  zmm1.v(ii)
               zmm2.v(ii)   =  src(i+32+ii)
               dst(i+32+ii) =  zmm2.v(ii)
               zmm3.v(ii)   =  src(i+48+ii)
               dst(i+48+ii) =  zmm3.v(ii)
               zmm4.v(ii)   =  src(i+64+ii)
               dst(i+64+ii) =  zmm4.v(ii)
               zmm5.v(ii)   =  src(i+80+ii)
               dst(i+80+ii) =  zmm5.v(ii)
               zmm6.v(ii)   =  src(i+96+ii)
               dst(i+96+ii) =  zmm6.v(ii)
               zmm7.v(ii)   =  src(i+112+ii)
               dst(i+112+ii)=  zmm7.v(ii)
               zmm8.v(ii)   =  src(i+128+ii)
               dst(i+128+ii)=  zmm8.v(ii)
               zmm9.v(ii)   =  src(i+144+ii)
               dst(i+144+ii)=  zmm9.v(ii)
               zmm10.v(ii)  =  src(i+160+ii)
               dst(i+160+ii)=  zmm10.v(ii)
               zmm11.v(ii)  =  src(i+176+ii)
               dst(i+176+ii)=  zmm11.v(ii)
               zmm12.v(ii)  =  src(i+192+ii)
               dst(i+192+ii)=  zmm12.v(ii)
               zmm13.v(ii)  =  src(i+208+ii)
               dst(i+208+ii)=  zmm13.v(ii)
               zmm14.v(ii)  =  src(i+224+ii)
               dst(i+224+ii)=  zmm14.v(ii)
               zmm15.v(ii)  =  src(i+240+ii)
               dst(i+240+ii)=  zmm15.v(ii)
               !!///////////////////////////////
               zmm16.v(ii)  =  src(i+256+ii)
               dst(i+256+ii)=  zmm16.v(ii)
               zmm17.v(ii)  =  src(i+272+ii)
               dst(i+272+ii) = zmm17.v(ii)
               zmm18.v(ii)   = src(i+288+ii)
               dst(i+288+ii) = zmm18.v(ii)
               zmm19.v(ii)   = src(i+304+ii)
               dst(i+304+ii) =  zmm19.v(ii)
               zmm20.v(ii)   = src(i+320+ii)
               dst(i+304+ii) =  zmm20.v(ii)
               zmm21.v(ii)   =  src(i+320+ii)
               dst(i+320+ii) =  zmm21.v(ii)
               zmm22.v(ii)   =  src(i+336+ii)
               dst(i+336+ii) =  zmm22.v(ii)
               zmm23.v(ii)   =  src(i+352+ii)
               dst(i+352+ii)=  zmm23.v(ii)
               zmm24.v(ii)   =  src(i+368+ii)
               dst(i+368+ii)=  zmm24.v(ii)
               zmm25.v(ii)   =  src(i+384+ii)
               dst(i+384+ii)=  zmm25.v(ii)
               zmm26.v(ii)  =  src(i+400+ii)
               dst(i+400+ii)=  zmm26.v(ii)
               zmm27.v(ii)  =  src(i+416+ii)
               dst(i+416+ii)=  zmm27.v(ii)
               zmm28.v(ii)  =  src(i+432+ii)
               dst(i+432+ii)=  zmm28.v(ii)
               zmm29.v(ii)  =  src(i+448+ii)
               dst(i+448+ii)=  zmm29.v(ii)
               zmm30.v(ii)  =  src(i+464+ii)
               dst(i+464+ii)=  zmm30.v(ii)
               zmm31.v(ii)  =  src(i+480+ii)
               dst(i+480+ii)=  zmm31.v(ii)
            end do
         end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
         do j = i, n
            dst(j) = src(j)
         end do
         return
      end if
      
    end subroutine zmm16r4_memcpy_unroll32x
    







end module simd_memops
