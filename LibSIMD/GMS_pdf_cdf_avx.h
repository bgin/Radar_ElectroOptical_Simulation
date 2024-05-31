
#ifndef __GMS_PDF_CDF_AVX_H__
#define __GMS_PDF_CDF_AVX_H__ 310520240713


/*MIT License
Copyright (c) 2020 Bernard Gingold
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/



    const unsigned int GMS_PDF_CDF_SSE_MAJOR = 1U;
    const unsigned int GMS_PDF_CDF_SSE_MINOR = 0U;
    const unsigned int GMS_PDF_CDF_SSE_MICRO = 0U;
    const unsigned int GMS_PDF_CDF_SSE_FULLVER =
      1000U*GMS_PDF_CDF_SSE_MAJOR+
      100U*GMS_PDF_CDF_SSE_MINOR+
      10U*GMS_PDF_CDF_SSE_MICRO;
    const char * const GMS_PDF_CDF_SSE_CREATION_DATE = "31-05-2024 07:13 PM +00200 (FRI 31 MAY 2024 GMT+2)";
    const char * const GMS_PDF_CDF_SSE_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_PDF_CDF_SSE_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_PDF_CDF_SSE_DESCRIPTION   = "AVX-optimized PDF and CDF functions.";




#include <immintrin.h>
#include <stdint.h>
#include "GMS_kernel_config.h"



 __m256d gamma_log_ymm4r8(const __m256d x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


__m256 gamma_log_ymm8r4(const __m256 x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));

__m256d
gamma_incomplete_ymm4r8(const __m256d p,
                        const __m256d x) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
__m256d  
gamma_incomplete_ymm8r4(const __m256 p,
                        const __m256 x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
__m256d  
vpoly_eval_ymm4r8(const int32_t n,
		  const __m256d * __restrict __attribute__((aligned(32))) a,
		  const __m256d x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
 __m256  
vpoly_eval_ymm8r4(const int32_t n,
		  const __m256 * __restrict __attribute__((aligned(32))) a,
		  const __m256 x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
 __m256d    
normal_01_cdf_inv_ymm4r8(const __m256d p)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
 __m256    
normal_01_cdf_inv_ymm8r4(const __m256 p)
                                  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));







#endif /*__GMS_PDF_CDF_AVX_H__*/
