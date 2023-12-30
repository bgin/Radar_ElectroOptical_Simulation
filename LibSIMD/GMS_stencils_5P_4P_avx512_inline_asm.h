

#ifndef __GMS_STENCILS_5P_4P_AVX512_INLINE_ASM_H__
#define __GMS_STENCILS_5P_4P_AVX512_INLINE_ASM_H__ 101020210944




#include <immintrin.h>



__m512d
stencil_5P_zmm8r8(__m512d (*) (__m512d),
                  const __m512d,
		  const __m512d,
                  __m512d * __restrict,
		  __m512d * __restrict) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((aligned(32)));



__m512d
stencil_5P_central_zmm8r8_optim(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));



__m512d
stencil_4P_zmm8r8(__m512d (*) (__m512d),
                  const __m512d,
		  const __m512d,
                  __m512d * __restrict,
		  __m512d * __restrict) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((aligned(32)));


__m512d
stencil_4P_forward_zmm8r8_optim(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));


__m512d
stencil_4P_backward_zmm8r8_optim(__m512d (*) (__m512d),
                                const __m512d,
		                const __m512d,
                                __m512d * __restrict)
		                                     __attribute__((noinline))
			                             __attribute__((hot))
				                     __attribute__((aligned(32)));




















#endif /*__GMS_STENCILS_5P_4P_AVX512_INLINE_ASM_H__*/
