
#ifndef __GMS_PDF_CDF_SSE_H__
#define __GMS_PDF_CDF_SSE_H__


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
    const char * const GMS_PDF_CDF_SSE_CREATION_DATE = "25-03-2024 06:20 PM +00200 (SUN 25 MAR 2024 GMT+2)";
    const char * const GMS_PDF_CDF_SSE_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_PDF_CDF_SSE_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_PDF_CDF_SSE_DESCRIPTION   = "SSE-optimized PDF and CDF functions.";




#include <immintrin.h>
#include <stdint.h>
#include "GMS_kernel_config.h"



 __m128d 
 gamma_log_xmm2r8(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
	
 __m128 
 gamma_log_xmm4r4(const __m128) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
 __m128d				   
gamma_incomplete_xmm2r8(const __m128d,
                        const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
 __m128				   
 gamma_incomplete_xmm4r4(const __m128,
                         const __m128)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
  __m128d  
  vpoly_eval_xmm2r8(const int ,
		    const __m128d * __restrict __attribute__((aligned(16))),
		    const __m128d )
		                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
   __m128  
   vpoly_eval_xmm4r4(const int ,
		     const __m128 * __restrict __attribute__((aligned(16))),
		     const __m128 ) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));

    
   __m128d
   normal_01_cdf_inv_xmm2r8(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				 
				 
    __m128
   normal_01_cdf_inv_xmm4r4(const __m128)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
   __m128d 
   reciprocal_cdf_xmm2r8(const __m128d ,
		         const __m128d ,
		         const __m128d )
				   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m128 
   reciprocal_cdf_xmm4r4(const __m128 ,
		         const __m128 ,
		         const __m128 )
				   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
    __m128d 		     
    reciprocal_cdf_inv_xmm2r8(const __m128d,
		              const __m128d,
                              const __m128d) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


    __m128		     
    reciprocal_cdf_inv_xmm4r4(const __m128,
		              const __m128,
                              const __m128) 
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
      __m128d 
      reciprocal_mean_xmm2r8(const __m128d ,
		             const __m128d )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
      __m128 
      reciprocal_mean_xmm4r4(const __m128 ,
		             const __m128 )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
      __m128d
      reciprocal_pdf_xmm2r8(const __m128d ,
		            const __m128d ,
                            const __m128d )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
       __m128
      reciprocal_pdf_xmm4r4(const __m128 ,
		            const __m128 ,
                            const __m128 )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
       __m128d  bessesl_i0_xmm2r8(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	__m128  bessesl_i0_xmm4r4(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
        __m128d bessel_i1_xmm2r8(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	__m128 bessel_i1_xmm4r4(const __m128)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	 __m128d beta_xmm2r8(const __m128d ,
		             const __m128d )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	 __m128  beta_xmm4r4(const __m128 ,
		             const __m128 )
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	  __m128d arcsin_cdf_xmm2r8(const __m128d,
		                    const __m128d)
		                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	  __m128 arcsin_cdf_xmm4r4(const __m128,
		                   const __m128)
		                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	   __m128d arcsin_cdf_inv_xmm2r8(const __m128d,
		                         const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	   __m128  arcsin_cdf_inv_xmm4r4(const __m128,
		                         const __m128)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	   __m128d arcsin_pdf_xmm2r8(const __m128d,
		                     const __m128d)
		                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	    __m128 arcsin_pdf_xmm4r4(const __m128,
		                     const __m128)
		                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
	    __m128d arcsin_variance_xmm2r8(const __m128d)
	                           __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  


             __m128 arcsin_variance_xmm4r4(const __m128)
	                           __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
	      __m128d arcsin_sample_xmm2r8()
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));  
				   
				   
	      __m128d  
              normal_01_cdf_xmm2r8(const __m128d)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));    
				   
				   
	      __m128  
              normal_01_cdf_xmm4r4(const __m128)
                                   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));    
				   
				   
	      __m128d beta_binomial_cdf_xmm2r8(const int32_t,
		                               const int32_t,
					       const __m128d,
					       const __m128d)
			           __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));
				   
				   
	       __m128 beta_binomial_cdf_xmm4r4(const int32_t,
		                               const int32_t,
					       const __m128,
					       const __m128)
			           __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 
				   
				   
	       __m128d beta_pdf_xmm2r8(const __m128d,
		                       const __m128d,
				       const __m128d)
				   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 
				   
				   
	      __m128 beta_pdf_xmm4r(const __m128,
		                    const __m128,
				    const __m128)
				   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32))); 



#endif /*__GMS_PDF_CDF_SSE_H__*/
