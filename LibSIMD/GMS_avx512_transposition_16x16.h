
#ifndef __GMS_AVX512_TRANSPOSITION_16X16_H__
#define __GMS_AVX512_TRANSPOSITION_16X16_H__



#include <immintrin.h>


         //In-place transformation.
         void
	 transpose_zmm16r4_16x16_ip(__m512 * __restrict,
		                    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict,
				    __m512 * __restrict) __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));



	 void
	 transpose_u_zmm16r4_16x16_ip(float * __restrict,
		                      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict)  __attribute__((noinline))
							   __attribute__((hot))
							   __attribute__((regcall))
							   __attribute__((aligned(32)));


	 void
	 transpose_a_zmm16r4_16x16_ip(float * __restrict,
		                      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict,
				      float * __restrict)  __attribute__((noinline))
							   __attribute__((hot))
							   __attribute__((regcall))
							   __attribute__((aligned(32)));


	 void
	 transpose_u_zmm16r4_16x16_ip_v2(float * __restrict)   __attribute__((noinline))
							       __attribute__((hot))
							       __attribute__((aligned(32)));


	 void
	 transpose_a_zmm16r4_16x16_ip_v2(float * __restrict)   __attribute__((noinline))
							       __attribute__((hot))
							       __attribute__((aligned(32)));


	 void
	 transpose_u_zmm16r4_16x16(float * __restrict,
	                           float * __restrict)   __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((aligned(32)));


	 void
	 transpose_a_zmm16r4_16x16(float * __restrict,
	                           float * __restrict)   __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((aligned(32)));

        
#include <stdint.h>

         void
         transpose_u_zmm16r4_16x16_looped(float * __restrict,
	                                  float * __restrict,
					  const int32_t)  __attribute__((noinline))
							  __attribute__((hot))
							  __attribute__((aligned(32)));


	 void
         transpose_a_zmm16r4_16x16_looped(float * __restrict,
	                                  float * __restrict,
					  const int32_t)  __attribute__((noinline))
							  __attribute__((hot))
							  __attribute__((aligned(32)));


	 
















#endif /*__GMS_AVX512_TRANSPOSITION_16X16_H__*/
