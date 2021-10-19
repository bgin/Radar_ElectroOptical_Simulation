

#ifndef __GMS_GEODESY_AVX512_PS_H__
#define __GMS_GEODESY_AVX512_PS_H__ 191020211552


    const unsigned int gGMS_GEODESY_AVX512_PS_MAJOR = 1U;
    const unsigned int gGMS_GEODESY_AVX512_PS_MINOR = 0U;
    const unsigned int gGMS_GEODESY_AVX512_PS_MICRO = 0U;
    const unsigned int gGMS_GEODESY_AVX512_PS_FULLVER =
      1000U*gGMS_GEODESY_AVX512_PS_MAJOR+
      100U*gGMS_GEODESY_AVX512_PS_MINOR+
      10U*gGMS_GEODESY_AVX512_PS_MICRO;
    const char * const pgGMS_GEODESY_AVX512_PS_CREATION_DATE = "19-10-2021 15:52  +00200 (TUE 19 OCT 2021 GMT+2)";
    const char * const pgGMS_GEODESY_AVX512_PS_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const pgGMS_GEODESY_AVX512_PS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const pgGMS_GEODESY_AVX512_PS_DESCRIPTION   = "Vectorized (AVX512 single-precision) geodesic computation implementation."


#include <immintrin.h>
#include <math.h>
#include <stdint.h>



void
cart_to_geodetic_zmm16r4( const __m512,
                          const __m512,
			  const __m512,
			  const __m512,
			  const __m512,
			  __m512 * __restrict,
			  __m512 * __restrict) __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));

void
cart_to_geodetic_u_zmm16r4_looped(const float * __restrict,
			          const float * __restrict,
				  const float * __restrict ,
				  const float,
				  const float,
				  float * __restrict,
				  float * __restrict,
				  float * __restrict,
				  const int32_t n) __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));

void
cart_to_geodetic_a_zmm16r4_looped(const float * __restrict,
			         const float * __restrict,
				 const float * __restrict ,
				 const float,
				 const float,
				 float * __restrict,
				 float * __restrict,
				 float * __restrict,
				 const int32_t n) __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));


void
geodetic_to_cart_zmm16r4( const __m512,
                         const __m512,
			 const __m512,
			 const __m512,
			 const __m512,
			 __m512 * __restrict,
			 __m512 * __restrict) __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
geodetic_to_cart_u_zmm16r4_looped(const float,
			         const float,
				 const float * __restrict,
				 const float * __restrict,
				 const float * __restrict,
				 float * __restrict,
				 float * __restrict,
				 float * __restrict,
				 const int32_t)   __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));


void
geodetic_to_cart_a_zmm16r4_looped(const float,
			         const float,
				 const float * __restrict,
				 const float * __restrict,
				 const float * __restrict,
				 float * __restrict,
				 float * __restrict,
				 float * __restrict,
				 const int32_t)   __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));

						  















#endif /*__GMS_GEODESY_AVX512_PS_H__*/
