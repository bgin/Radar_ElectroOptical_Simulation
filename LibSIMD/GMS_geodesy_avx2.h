
#ifndef __GMS_GEODESY_AVX2_H__
#define __GMS_GEODESY_AVX2_H__ 171020211522



    const unsigned int gGMS_GEODESY_AVX2_MAJOR = 1U;
    const unsigned int gGMS_GEODESY_AVX2_MINOR = 0U;
    const unsigned int gGMS_GEODESY_AVX2_MICRO = 0U;
    const unsigned int gGMS_GEODESY_AVX2_FULLVER =
      1000U*gGMS_GEODESY_AVX2_MAJOR+
      100U*gGMS_GEODESY_AVX2_MINOR+
      10U*gGMS_GEODESY_AVX2_MICRO;
    const char * const pgGMS_GEODESY_AVX2_CREATION_DATE = "17-10-2021 15:22  +00200 (SUN 17 OCT 2021 GMT+2)";
    const char * const pgGMS_GEODESY_AVX2_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const pgGMS_GEODESY_AVX2_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const pgGMS_GEODESY_AVX2_DESCRIPTION   = "Vectorized (AVX/AVX2) geodesic computation implementation."


#include <immintrin.h>
#include <stdint.h>



void
cart_to_geodetic_ymm4r8( const __m256d, //input position x [km]
			 const __m256d, //input position y [km]
			 const __m256d, //input position z [km]
			 const __m256d, // semi-minor axis [km]
			 const __m256d, // semi-major axis [km]
			 __m256d * __restrict, //output altitude [km]
			 __m256d * __restrict, //output longtitude [rad]
			 __m256d * __restrict) __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
cart_to_geodetic_u_ymm4r8_looped(const double * __restrict,
			         const double * __restrict,
				 const double * __restrict,
				 const double,
				 const double,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
cart_to_geodetic_a_ymm4r8_looped(const double * __restrict,
			         const double * __restrict,
				 const double * __restrict,
				 const double,
				 const double,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
geodetic_to_cart_ymm4r8(const __m256d,
		        const __m256d,
			const __m256d,
			const __m256d,
			const __m256d,
			__m256d * __restrict,
			__m256d * __restrict,
			__m256d * __restrict)  __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
geodetic_to_cart_u_ymm4r8_looped(const double,
			         const double,
				 const double * __restrict,
				 const double * __restrict,
				 const double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
geodetic_to_cart_a_ymm4r8_looped(const double,
			         const double,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 double * __restrict,
				 const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
forward_method_ymm4r8(const __m256d,      //ellipsoid semi-maxjor axis
		      const __m256d,      //elipsoid flattening [dimensionless]
		      const __m256d,    //vector of 4 starting-points latitude [rad]
		      const __m256d,    //vector of 4 starting-points longtitude [rad]
		      const __m256d,      //vector of 4 forward azimutes [rad]
		      const __m256d,      //vector of 4 distances vp1-to-vp2 [m]
		      __m256d * __restrict,         //vector of 4 endpoints latitude [rad]
		      __m256d * __restrict,         //vector of 4 endpoints longtitude [rad]
		      __m256d * __restrict)        //backward facing vector of 4 azimutes vp2-to-vp1 [rad]
			                       __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void 
forward_method_u_ymm4r8_looped(const double,
			       const double,
			       const double * __restrict,
			       const double * __restrict,
			       const double * __restrict,
			       const double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       const int32_t)    __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));



void 
forward_method_a_ymm4r8_looped(const double,
			       const double,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       double * __restrict,
			       const int32_t)    __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));

	/*
                           @Reference:
                                          http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
       */
__m256d
spheroid_distance_ymm4r8(const __m256d,
			 const __m256d,
			 const __m256d,
			 const __m256d,
			 const __m256d)        __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));



void
spheroid_distance_u_ymm4r8_looped(const double,
			          double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  const int32_t)   __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
spheroid_distance_a_ymm4r8_looped(const double,
			          double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  const int32_t)   __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


__m256d
geocentric_radius_ymm4r8(const __m256d,
			 const __m256d,
			 const __m256d)        __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
geocentric_radius_u_ymm4r8_looped(const double,
			          const double * __restrict,
				  const double * __restrict,
				  double * __restrict,
				  const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));


void
geocentric_radius_a_ymm4r8_looped(const double,
			          double * __restrict,
				  double * __restrict,
				  double * __restrict,
				  const int32_t)  __attribute__((noinline))
			                         __attribute__((hot))
					         __attribute__((aligned(32)));






						 

						 





















#endif /*__GMS_GEODESY_AVX2_H__*/
