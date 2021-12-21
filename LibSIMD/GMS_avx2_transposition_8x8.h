
#ifndef __GMS_AVX2_TRANSPOSITION_8X8_H__
#define __GMS_AVX2_TRANSPOSITION_8X8_H__


#include <immintrin.h>


      // In-place
      void
      transpose_ymm8r4_8x8_ip(__m256 * __restrict,
		              __m256 * __restrict,
			      __m256 * __restrict,
			      __m256 * __restrict,
			      __m256 * __restrict,
			      __m256 * __restrict,
			      __m256 * __restrict,
			      __m256 * __restrict)      __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_u_ymm8r4_8x8_ip(float * __restrict,
		                float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict)      __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_a_ymm8r4_8x8_ip(float * __restrict,
		                float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict,
			        float * __restrict)      __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_u_ymm8r4_8x8_ip_v2(float * __restrict)   __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_a_ymm8r4_8x8_ip_v2(float * __restrict)   __attribute__((noinline))
							 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_u_ymm8r4_8x8(float * __restrict,
                             float * __restrict)        __attribute__((noinline))
						  	 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_a_ymm8r4_8x8(float * __restrict,
                             float * __restrict)        __attribute__((noinline))
						  	 __attribute__((hot))
							 __attribute__((regcall))
							 __attribute__((aligned(32)));


      void
      transpose_u_ymm8r4_8x8_v2(float * __restrict,
		                float * __restrict,
				const int32_t)           __attribute__((noinline))
						  	 __attribute__((hot))
							 __attribute__((aligned(32)));


      void
      transpose_a_ymm8r4_8x8_v2(float * __restrict,
		                float * __restrict,
				const int32_t)           __attribute__((noinline))
						  	 __attribute__((hot))
							 __attribute__((aligned(32)));



			                           







#endif /* __GMS_AVX2_TRANSPOSITION_8X8_H__*/
