

#ifndef __GMS_GEODESY_AVX512_H__
#define __GMS_GEODESY_AVX512_H__ 171020210952


    const unsigned int gGMS_GEODESY_AVX512_MAJOR = 1U;
    const unsigned int gGMS_GEODESY_AVX512_MINOR = 0U;
    const unsigned int gGMS_GEODESY_AVX512_MICRO = 0U;
    const unsigned int gGMS_GEODESY_AVX512_FULLVER =
      1000U*gGMS_GEODESY_AVX512_MAJOR+
      100U*gGMS_GEODESY_AVX512_MINOR+
      10U*gGMS_GEODESY_AVX512_MICRO;
    const char * const pgGMS_GEODESY_AVX512_CREATION_DATE = "17-10-2021 09:52 AM +00200 (SUN 17 OCT 2021 GMT+2)";
    const char * const pgGMS_GEODESY_AVX512_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const pgGMS_GEODESY_AVX512_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const pgGMS_GEODESY_AVX512_DESCRIPTION   = "Vectorized (AVX512) geodesic computation implementation."


#include <immintrin.h>
#include <math.h>
#include <stdint.h>



void
cart_to_geodetic_zmm8r8( const __m512d,
                         const __m512d,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 __m512d * __restrict,
			 __m512d * __restrict) __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));

void
cart_to_geodetic_u_zmm8r8_looped(const double * __restrict,
			         const double * __restrict,
				 const double * __restrict ,
				 const double,
				 const double,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t n) __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));

void
cart_to_geodetic_a_zmm8r8_looped(const double * __restrict,
			         const double * __restrict,
				 const double * __restrict ,
				 const double,
				 const double,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t n) __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));


void
geodetic_to_cart_zmm8r8( const __m512d,
                         const __m512d,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 __m512d * __restrict,
			 __m512d * __restrict) __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
geodetic_to_cart_u_zmm8r8_looped(const double,
			         const double,
				 const double * __restrict,
				 const double * __restrict,
				 const double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t)   __attribute__((noinline))
			                          __attribute__((hot))
					          __attribute__((aligned(32)));

						  















#endif /*__GMS_GEODESY_AVX512_H__*/
