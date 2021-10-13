
#ifndef __GMS_DERIVATIVE_5P4P_AVX2_H__
#define __GMS_DERIVATIVE_5P4P_AVX2_H__ 121320211702



    const unsigned int gGMS_DERIVATIVE_5P4P_AVX2_MAJOR = 1U;
    const unsigned int gGMS_DERIVATIVE_5P4P_AVX2_MINOR = 0U;
    const unsigned int gGMS_DERIVATIVE_5P4P_AVX2_MICRO = 0U;
    const unsigned int gGMS_DERIVATIVE_5P4P_AVX2_FULLVER =
      1000U*gGMS_DERIVATIVE_5P4P_AVX2_MAJOR+
      100U*gGMS_DERIVATIVE_5P4P_AVX2_MINOR+
      10U*gGMS_DERIVATIVE_5P4P_AVX2_MICRO;
    const char * const pgGMS_DERIVATIVE_5P4P_AVX2_CREATION_DATE = "10-13-2021 17:02  +00200 (WED 13 OCT 2021 GMT+2)";
    const char * const pgGMS_DERIVATIVE_5P4P_AVX2_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const pgGMS_DERIVATIVE_5P4P_AVX2_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const pgGMS_DERIVATIVE_5P4P_AVX2_DESCRIPTION   = "Vectorized (AVX/AVX2) derivative implementation (kernel)."


#include <immintrin.h>

__attribute__((always_inline))
__attribute__((aligned(32)))
 static inline
 __m256d  abs_ymm4r8(const __m256d x){
              const __m256d mask = _mm256_set1_pd(0x7FFFFFFFFFFFFFFF);
	      return (_mm256_and_pd(x,mask));

 }


 __m256d
 stencil_5P_central_ymm4r8(__m256d (*) (__m256d),
			   __m256d,
			   __m256d,
			   __m256d * __restrict,
			   __m256d * __restrict) __attribute__((noinline))
			                         __attribute__((hot))
				                 __attribute__((aligned(32)));


__m256d
stencil_4P_forward_ymm4r8(__m256d (*)(__m256d),
			  __m256d vx,
			  __m256d vh,
			  __m256d * __restrict,
			  __m256d * __restrict)  __attribute__((noinline))
			                         __attribute__((hot))
				                 __attribute__((aligned(32)));


__m256d
stencil_4P_backward_ymm4r8(__m256d (*)(__m256d),
			  __m256d vx,
			  __m256d vh,
			  __m256d * __restrict,
			  __m256d * __restrict)  __attribute__((noinline))
			                         __attribute__((hot))
				                 __attribute__((aligned(32)));

















#endif /*__GMS_DERIVATIVE_5P4P_H__*/
