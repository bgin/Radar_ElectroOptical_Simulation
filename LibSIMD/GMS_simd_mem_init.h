

#ifndef __GMS_SIMD_MEM_INIT_H__
#define __GMS_SIMD_MEM_INIT_H__ 260920200347


   const unsigned int GMS_SIMD_MEM_INIT_MAJOR = 1U;
   const unsigned int GMS_SIMD_MEM_INIT_MINOR = 0U;
   const unsigned int GMS_SIMD_MEM_INIT_MICRO = 0U;
   const unsigned int GMS_SIMD_MEM_INIT_FULLVER =
         1000U*GMS_SIMD_MEM_INIT_MAJOR+100U*GMS_SIMD_MEM_INIT_MINOR+10U*GMS_SIMD_MEM_INIT_MICRO;
   const char * const GMS_SIMD_MEM_INIT_CREATE_DATE = "26-09-2020 3:47PM +00200 (SAT 26 SEP 2020 GMT+2)";
   const char * const GMS_SIMD_MEM_INIT_BUILD_DATE  = __DATE__ ":" __TIME__;
   const char * const GMS_SIMD_MEM_INIT_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";






#include <stdint.h>


             void zmm16r4_init_unroll8x(float *   __restrict,
			                const int32_t,
			                const float) __attribute__((noinline))
                                                     __attribute__((hot))
                                                     __attribute__(aligned(32));

	     void ymm8r4_init_unroll8x(float * __restrict,
			               const int32_t,
			               const float)  __attribute__((noinline))
                                                     __attribute__((hot))
                                                     __attribute__(aligned(32));


	     












#endif /*__GMS_SIMD_MEM_INIT_H__*/
