

#ifndef __GMS_STENCILS_5P_4P_AVX512_H__
#define __GMS_STENCILS_5P_4P_AVX512_H__ 101020210944

/*

    const unsigned int gGMS_DERIVATIVE_AVX512_MAJOR = 1U;
    const unsigned int gGMS_DERIVATIVE_AVX512_MINOR = 0U;
    const unsigned int gGMS_DERIVATIVE_AVX512_MICRO = 0U;
    const unsigned int gGMS_DERIVATIVE_AVX512_FULLVER =
      1000U*gGMS_DERIVATIVE_AVX512_MAJOR+
      100U*gGMS_DERIVATIVE_AVX512_MINOR+
      10U*gGMS_DERIVATIVE_AVX512_MICRO;
    const char * const pgGMS_DERIVATIVE_AVX512_CREATION_DATE = "10-10-2021 09:44 AM +00200 (SUN 10 OCT 2021 GMT+2)";
    const char * const pgGMS_DERIVATIVE_AVX512_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const pgGMS_DERIVATIVE_AVX512_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const pgGMS_DERIVATIVE_AVX512_DESCRIPTION   = "Vectorized (AVX512) derivative implementation."
*/


#include <immintrin.h>



__m512d
stencil_5P_central_zmm8r8(__m512d (*) (__m512d),
                  const __m512d,
		  const __m512d,
                  __m512d * __restrict,
		  __m512d * __restrict) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((aligned(32)));



/*__m512d
stencil_5P_central_zmm8r8_optim(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));*/



__m512d
stencil_4P_forward_zmm8r8(__m512d (*) (__m512d),
                          const __m512d,
		          const __m512d,
                          __m512d * __restrict,
		          __m512d * __restrict) __attribute__((noinline))
			                        __attribute__((hot))
				                __attribute__((aligned(32)));


/*__m512d
stencil_4P_forward_zmm8r8_optim(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));*/


__m512d
stencil_4P_backward_zmm8r8(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict,
			        __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));




















#endif /*__GMS_STENCILS_5P_4P_AVX512_H__*/
