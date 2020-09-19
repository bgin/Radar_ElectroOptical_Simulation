

#include <immintrin.h>
#include "GMS_dgemm_beta_skx.h"


int32_t dgemm_beta_skx(const int32_t n,
		       const int32_t m,
		       const double beta,
		       double * __restrict c,
		       const int32_t ldc) {

    int32_t i,j;
    double * __restrict c_offset1 = NULL;
    double * __restrict c_offset  = NULL;
    double ctemp1,ctemp2,ctemp3,ctemp4;
    double ctemp5,ctemp6,ctemp7,ctemp8;

    if(m == ldc && beta == 0.0) {
       memset(c,0,m*n*sizeof(double));
       return (0);
    }

    if(m == 0 || n == 0) {
       return (0);
    }

    if(beta == 0.0) {

       j = n;

       for(; j > 0; --j) {
           c_offset1 = c_offset;
	   c_offset += ldc;
           i = m;
#if defined __AVX512F__
	   for(; i >= 32; i -= 32) {
               const __m512d zero = _mm512_setzero_pd();
	       _mm512_storeu_pd(c_offset1,zero);
	       _mm512_storeu_pd(c_offset1+8,zero);
	       _mm512_storeu_pd(c_offset1+16,zero);
	       _mm512_storeu_pd(c_offset1+24,zero);
	       c_offset1 += 32;
	   }
#endif
	   for(; i >= 8; i -= 8) {
#if defined __AVX512F__
                 const __m512d zero = _mm512_setzero_pd();
		 _mm512_storeu_pd(c_offset1,zero);
#else
                 const __m256d zero = _mm256_setzero_pd();
		 _mm256_storeu_pd(c_offset1,zero);
		 _mm256_storeu_pd(c_offset1+4,zero);
#endif
                 c_offset1 += 8;
               
	   }
	   for(; i > 0; --i) {
               c_offset1[i] = 0.0;
	       
	   }
       }
    }
     else {

          j = n;
          do {
               c_offset1 = c_offset;
               c_offset += ldc;

               i = (m >> 3);
               if (i > 0){
	          do {


		     
	             ctemp1 = *(c_offset1 + 0);
		     ctemp1 *= beta;
		     *(c_offset1 + 0) = ctemp1;
	             ctemp2 = *(c_offset1 + 1);
		     ctemp2 *= beta;
		     *(c_offset1 + 1) = ctemp2;
	             ctemp3 = *(c_offset1 + 2);
		     ctemp3 *= beta;
		     *(c_offset1 + 2) = ctemp3;
	             ctemp4 = *(c_offset1 + 3);
		     ctemp4 *= beta;
		     *(c_offset1 + 3) = ctemp4;
	             ctemp5 = *(c_offset1 + 4);
		     ctemp5 *= beta;
		     *(c_offset1 + 4) = ctemp5;
	             ctemp6 = *(c_offset1 + 5);
		     ctemp6 *= beta;
		     *(c_offset1 + 5) = ctemp6;
	             ctemp7 = *(c_offset1 + 6);
		     ctemp7 *= beta;
		     *(c_offset1 + 6) = ctemp7;
	             ctemp8 = *(c_offset1 + 7);
		     ctemp8 *= beta;
                     *(c_offset1 + 7) = ctemp8;
	                       
	             c_offset1 += 8;
	             i --;
	          } while (i > 0);	            
	                  
	     }                  
               i = (m & 7);
               if (i > 0) {
	       do {
	           ctemp1 = *c_offset1;
	           ctemp1 *= beta;
	           *c_offset1 = ctemp1;
	           c_offset1 ++;
	           i --;
	     } while (i > 0);
          }
	             
	     j --;
        } while (j > 0);

     }            

}
