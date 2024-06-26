

 #ifndef __GMS_PDF_CDF_AVX512_H__
 #define __GMS_PDF_CDF_AVX512_H__
 
 
 
 
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



    const unsigned int GMS_PDF_CDF_AVX512_MAJOR = 1U;
    const unsigned int GMS_PDF_CDF_AVX512_MINOR = 0U;
    const unsigned int GMS_PDF_CDF_AVX512_MICRO = 0U;
    const unsigned int GMS_PDF_CDF_AVX512_FULLVER =
      1000U*GMS_PDF_CDF_AVX512_MAJOR+
      100U*GMS_PDF_CDF_AVX512_MINOR+
      10U*GMS_PDF_CDF_AVX512_MICRO;
    const char * const GMS_PDF_CDF_AVX512_CREATION_DATE = "25-06-2024 08:03 PM +00200 (TUE 25 JUN 2024 GMT+2)";
    const char * const GMS_PDF_CDF_AVX512_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_PDF_CDF_AVX512_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_PDF_CDF_AVX512_DESCRIPTION   = "AVX512-optimized PDF and CDF functions.";




#include <immintrin.h>
#include <stdint.h>
#include "GMS_kernel_config.h"

 
 
 
 __m512d gamma_log_zmm8r8(const __m512d x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512 gamma_log_zmm16r4(const __m512 x) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
   __m512d  bessesl_i0_zmm8r8(const __m512d arg)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512  bessesl_i0_zmm16r4(const __m512 arg)  
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512d bessel_i1_zmm8r8(const __m512d arg) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				 
  __m512 bessel_i1_zmm16r4(const __m512 arg) 
                                    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512d  
  normal_01_cdf_zmm8r8(const __m512d x)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512  
  normal_01_cdf_zmm16r4(const __m512 x) 
                                    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m512d   	    
  normal_01_sample_zmm8r8(__m512i * __restrict seed)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
   __m512  	    
   normal_01_sample_zmm16r4(__m512i * __restrict seed)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512d 
    reciprocal_cdf_zmm8r8(const __m512d x,
		          const __m512d a,
		          const __m512d b)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
				   
    __m512 
    reciprocal_cdf_zmm16r4(const __m512 x,
                           const __m512 a,
		           const __m512 b)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512d 		     
   reciprocal_cdf_inv_zmm8r8(const __m512d cdf,
		             const __m512d a,
		             const __m512d b)
                                    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
     __m512		     
     reciprocal_cdf_inv_zmm16r4(const __m512 cdf,
		                const __m512 a,
		                const __m512 b)
                                     __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
     __m512d 
     reciprocal_mean_zmm8r8(const __m512d a,
		            const __m512d b)
                                    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512 
    reciprocal_mean_zmm16r4(const __m512 a,
		            const __m512 b) 
                                  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512d
   reciprocal_pdf_zmm8r8( const __m512d x,
		          const __m512d a,
		          const __m512d b) 
                                  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512
    reciprocal_pdf_zmm16r4( const __m512 x,
		            const __m512 a,
		            const __m512 b) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
     __m512d
     reciprocal_sample_zmm8r8( __m512i * __restrict seed,
                               const __m512d a,
                                const __m512d b)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512
    reciprocal_sample_zmm16r4( __m512i * __restrict seed,
                               const __m512 a,
                               const __m512 b)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512d  
    reciprocal_var_zmm8r8(const __m512d a,
                          const __m512d b) 
                                __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m512  
    reciprocal_var_zmm16r4(const __m512 a,
                           const __m512 b)
                                  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
   __m512d
   sech_zmm8r8(const __m512d x) 
                                  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
     __m512
     sech_zmm16r4(const __m512 x)
				  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
     __m512d                     
     sech_cdf_zmm8r8(const __m512d x,
                     const __m512d a,
                     const __m512d b)    
				 __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
     sech_cdf_zmm16r4(const __m512 x,
                      const __m512 a,
                      const __m512 b)  
				  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
      __m512d  
     sech_cdf_inv_zmm8r8(const __m512d cdf,
		         const __m512d a,
		         const __m512d b) 
				    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
      __m512  
      sech_cdf_inv_zmm16r4(const __m512 cdf,
		           const __m512 a,
		           const __m512 b) 
				    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
       __m512d  
       sech_pdf_zmm8r8(const __m512d x
                       const __m512d a,
                       const __m512d b)
				    __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 
				   
				   
       __m512  
       sech_pdf_zmm16r4(const __m512 x
                        const __m512 a,
                        const __m512 b)
				  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 
				   
				   
        __m512d 
        sech_sample_zmm8r8(const __m512d a,
                           const __m512d b,
                           const __m512i * __restrict seed) 
				     __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
				   
 
 #endif /*__GMS_PDF_CDF_AVX512_H__*/
