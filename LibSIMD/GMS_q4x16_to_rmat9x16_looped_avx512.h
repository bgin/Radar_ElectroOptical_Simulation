
#ifndef __GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_H__
#define __GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_H__ 171120211518


   const unsigned int GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MAJOR = 1;
   const unsigned int GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MINOR = 0;
   const unsigned int GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MICRO = 0;
   const unsigned int GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_FULLVER =
    1000*GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MAJOR+
    100*GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MINOR+
    10*GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_MICRO;
   const char * const GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_CREATE_DATE = "17-11-2021 15:18 +00200 (WED 17 NOV 2021 GMT+2)";
   const char * const GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_BUILD_DATE  = __DATE__":"__TIME__;
   const char * const GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
   const char * const GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_DESCRIPTION = "Optimized kernels (AVX512) for basic rotation operations";



#include <cstdint>



                      /* Non-aligned macro-kernel
                         Quaternion to Orientation Matrix
                       */
                      void
                      q4x16_rm9x16_looped_u_nonunroll_zmm16r4(float * __restrict,
		                                              float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const int32_t) __attribute__((noinline))
							                     __attribute__((hot))
									     __attribute__((aligned(32)));
                      /*Aligned macro-kernel*/
                      void
                      q4x16_rm9x16_looped_a_nonunroll_zmm16r4(float * __restrict,
		                                              float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const int32_t) __attribute__((noinline))
							                     __attribute__((hot))
									     __attribute__((aligned(32)));


		        /* Non-aligned macro-kernel
                         Quaternion to Orientation Matrix
                       */
                      void
                      q4x16_rm9x16_looped_u_unroll4x_zmm16r4( float * __restrict,
		                                              float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const int32_t) __attribute__((noinline))
							                     __attribute__((hot))
									     __attribute__((aligned(32)));
                      /*Aligned macro-kernel*/
                      void
                      q4x16_rm9x16_looped_a_unroll4x_zmm16r4( float * __restrict,
		                                              float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const float * __restrict,
							      const int32_t) __attribute__((noinline))
							                     __attribute__((hot))
									     __attribute__((aligned(32)));



                      

		       



#endif /*__GMS_Q4X16_TO_RMAT9X16_LOOPED_AVX512_H__*/
