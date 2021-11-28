

#ifndef __GMS_SIMD_MEM_MOVE_H__
#define __GMS_SIMD_MEM_MOVE_H__ 26092020447


   const unsigned int GMS_SIMD_MEM_MOVE_MAJOR = 1U;
   const unsigned int GMS_SIMD_MEM_MOVE_MINOR = 0U;
   const unsigned int GMS_SIMD_MEM_MOVE_MICRO = 0U;
   const unsigned int GMS_SIMD_MEM_MOVE_FULLVER =
         1000U*GMS_SIMD_MEM_MOVE_MAJOR+100U*GMS_SIMD_MEM_MOVE_MINOR+10U*GMS_SIMD_MEM_MOVE_MICRO;
   const char * const GMS_SIMD_MEM_MOVE_CREATE_DATE = "26-09-2020 4:47PM +00200 (SAT 26 SEP 2020 GMT+2)";
   const char * const GMS_SIMD_MEM_MOVE_BUILD_DATE  = __DATE__ ":" __TIME__;
   const char * const GMS_SIMD_MEM_MOVE_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";


#include <stdint.h>


            void ymm8r4_cached_u_memmove(void * __restrict,
	                                 const void * __restrict,
					 const int32_t)   __attribute__((noinline))
					                  __attribute__((hot))
							  __attribute__((aligned(32)));


	    void ymm8r4_uncached_memove(void * __restrict,
	                                const void * __restrict,
					const int32_t)    __attribute__((noinline))
					                  __attribute__((hot))
							  __attribute__((aligned(32)));
	    


            void zmm16r4_cached_u_memmove(void * __restrict,
	                                  const void * __restrict,
					  const int32_t)   __attribute__((noinline))
					                   __attribute__((hot))
							   __attribute__((aligned(32)));


	    void zmm16r4_uncached_memmove(void * __restrict,
	                                  const void * __restrict,
					  const int32_t)   __attribute__((noinline))
					                   __attribute__((hot))
							   __attribute__((aligned(32)));
            




#endif /*__GMS_SIMD_MEM_MOVE_H__*/
