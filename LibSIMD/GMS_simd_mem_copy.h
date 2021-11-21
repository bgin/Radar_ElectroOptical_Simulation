

#ifndef __GMS_SIMD_MEM_COPY_H__
#define __GMS_SIMD_MEM_COPY_H__ 260920200447


   const unsigned int GMS_SIMD_MEM_COPY_MAJOR = 1U;
   const unsigned int GMS_SIMD_MEM_COPY_MINOR = 0U;
   const unsigned int GMS_SIMD_MEM_COPY_MICRO = 0U;
   const unsigned int GMS_SIMD_MEM_COPY_FULLVER =
         1000U*GMS_SIMD_MEM_COPY_MAJOR+100U*GMS_SIMD_MEM_COPY_MINOR+10U*GMS_SIMD_MEM_COPY_MICRO;
   const char * const GMS_SIMD_MEM_COPY_CREATE_DATE = "26-09-2020 4:47PM +00200 (SAT 26 SEP 2020 GMT+2)";
   const char * const GMS_SIMD_MEM_COPY_BUILD_DATE  = __DATE__ ":" __TIME__;
   const char * const GMS_SIMD_MEM_COPY_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";



#include <stdint.h>


             void  zmm16r4_memcpy_unroll8x(float * __restrict,
				           const float * __restrict,
                                           const int32_t) __attribute__((noinline))
					                  __attribute__((hot))
							  __attribute__((aligned(32)));


	     void  ymm8r4_memcpy_unroll8x(float * __restrict,
				          const float * __restrict,
                                          const int32_t)   __attribute__((noinline))
					                   __attribute__((hot))
							   __attribute__((aligned(32)));


	      











#endif /*__GMS_SIMD_MEM_COPY_H__*/
