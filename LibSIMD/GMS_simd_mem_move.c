

#include <immintrin.h>
#include "GMS_simd_mem_move.h"
#include "GMS_simd_memops_defs.h"


                void ymm8r4_cached_u_memmove(void * __restrict _Dst,
	                                     const void * __restrict _Src,
					     const int32_t _nelems) {

                      if (MEMMOVE_1ELEM <= _nelems) { return;}
	              char * __restrict dst = (char *)_Dst;
	              const char * __restrict src = (const char *)_Src;
	              if ( _nelems <= MEMMOVE_16ELEMS) {
		           const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			   _mm256_storeu_ps((float*)&dst[0],  ymm0);
		           const __m256 ymm1(_mm256_loadu_ps((float*)&src[1*YMM_LEN]));
		           _mm256_storeu_ps((float*)&dst[1*YMM_LEN], ymm1);
		           return;
	              }
	              else if ( _nelems <= MEMMOVE_32ELEMS) {
		           const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			   _mm256_storeu_ps((float*)&dst[0], ymm0);
		           const __m256 ymm1(_mm256_loadu_ps((float*)&src[1*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[1*YMM_LEN],ymm1);
		           const __m256 ymm2(_mm256_loadu_ps((float*)&src[2*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[2*YMM_LEN],ymm2);
		           const __m256 ymm3(_mm256_loadu_ps((float*)&src[3*YMM_LEN]));
		           _mm256_storeu_ps((float*)&dst[3*YMM_LEN],ymm3);
		           return;
	             }
	             else if ( _nelems <= MEMMOVE_64ELEMS){
		           const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			   _mm256_storeu_ps((float*)&dst[0], ymm0);
		           const __m256 ymm1(_mm256_loadu_ps((float*)&src[1*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[1*YMM_LEN], ymm1);
		           const __m256 ymm2(_mm256_loadu_ps((float*)&src[2*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[2*YMM_LEN], ymm2);
		           const __m256 ymm3(_mm256_loadu_ps((float*)&src[3*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[3*YMM_LEN], ymm3);
		           const __m256 ymm4(_mm256_loadu_ps((float*)&src[4*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[4*YMM_LEN], ymm4);
		           const __m256 ymm5(_mm256_loadu_ps((float*)&src[5*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[5*YMM_LEN], ymm5);
		           const __m256 ymm6(_mm256_loadu_ps((float*)&src[6*YMM_LEN]));
			   _mm256_storeu_ps((float*)&dst[6*YMM_LEN], ymm6);
		           const __m256 ymm7(_mm256_loadu_ps((float*)&src[7*YMM_LEN]));
		           _mm256_storeu_ps((float*)&dst[7*YMM_LEN], ymm7);
	 	           return;
	              }
	              else if ( _nelems <= MEMMOVE_128ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
		         _mm_prefetch((const char*)&src[0], _MM_HINT_T0);
		         _mm_prefetch((const char*)&src[2*YMM_LEN],_MM_HINT_T0);
		         _mm_prefetch((const char*)&src[4*YMM_LEN],_MM_HINT_T0);
		         _mm_prefetch((const char*)&src[6*YMM_LEN],_MM_HINT_T0);
		         _mm_prefetch((const char*)&src[8*YMM_LEN],_MM_HINT_T0);
#endif
		         const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			 _mm256_storeu_ps((float*)&dst[0], ymm0);
		         const __m256 ymm1(_mm256_loadu_ps((float*)&src[1*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[1*YMM_LEN], ymm1);
		         const __m256 ymm2(_mm256_loadu_ps((float*)&src[2*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[2*YMM_LEN], ymm2);
		         const __m256 ymm3(_mm256_loadu_ps((float*)&src[3*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[3*YMM_LEN], ymm3);
		         const __m256 ymm4(_mm256_loadu_ps((float*)&src[4*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[4*YMM_LEN], ymm4);
		         const __m256 ymm5(_mm256_loadu_ps((float*)&src[5*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[5*YMM_LEN], ymm5);
		         const __m256 ymm6(_mm256_loadu_ps((float*)&src[6*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[6*YMM_LEN], ymm6);
		         const __m256 ymm7(_mm256_loadu_ps((float*)&src[7*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[7*YMM_LEN], ymm7);
		         const __m256 ymm8(_mm256_loadu_ps((float*)&src[8*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[8*YMM_LEN], ymm8);
		         const __m256 ymm9(_mm256_loadu_ps((float*)&src[9*YMM_LEN]));
			  _mm256_storeu_ps((float*)&dst[9*YMM_LEN], ymm9);
		         const __m256 ymm10(_mm256_loadu_ps((float*)&src[10*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[10*YMM_LEN],ymm10);
		         const __m256 ymm11(_mm256_loadu_ps((float*)&src[11*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[11*YMM_LEN],ymm11);
		         const __m256 ymm12(_mm256_loadu_ps((float*)&src[12*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[12*YMM_LEN],ymm12);
		         const __m256 ymm13(_mm256_loadu_ps((float*)&src[13*YMM_LEN]));
			 _mm256_storeu_ps((float*)&dst[13*YMM_LEN],ymm13);
		         const __m256 ymm14(_mm256_loadu_ps((float*)&src[14*YMM_LEN]));
			  _mm256_storeu_ps((float*)&dst[14*YMM_LEN],ymm14);
		         const __m256 ymm15(_mm256_loadu_ps((float*)&src[15*YMM_LEN]));
		         _mm256_storeu_ps((float*)&dst[15*YMM_LEN],ymm15);
		                         	  	       	                 	         
		       	  
		         return;
	            }
	            else if (_nelems <= MAXFLOATSPERPAGE4KiB){
		            int32_t i;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count(1024)
#endif
		             for (i = 0; i != ROUND_TO_EIGHT(_nelems, 8); i += 64) {
#if (GMS_MAN_PREFETCH) == 1
			         _mm_prefetch((const char*)&src[i], _MM_HINT_T0);
#endif
			         const __m256 ymm0(_mm256_loadu_ps((float*)&src[i+0]));
				 _mm256_storeu_ps((float*)&dst[i+0], ymm0);
			         const __m256 ymm1(_mm256_loadu_ps((float*)&src[i+1*YMM_LEN]));
				 _mm256_storeu_ps((float*)&dst[i+1*YMM_LEN], ymm1);
			         const __m256 ymm2(_mm256_loadu_ps((float*)&src[i+2*YMM_LEN]));
				 _mm256_storeu_ps((float*)&dst[i+2*YMM_LEN], ymm2);
			         const __m256 ymm3(_mm256_loadu_ps((float*)&src[i+3*YMM_LEN]));
				 _mm256_storeu_ps((float*)&dst[i+3*YMM_LEN], ymm3);
			         const __m256 ymm4(_mm256_loadu_ps((float*)&src[i+4*YMM_LEN]));
				  _mm256_storeu_ps((float*)&dst[i+4*YMM_LEN], ymm4);
			         const __m256 ymm5(_mm256_loadu_ps((float*)&src[i+5*YMM_LEN]));
				  _mm256_storeu_ps((float*)&dst[i+5*YMM_LEN], ymm5);
			         const __m256 ymm6(_mm256_loadu_ps((float*)&src[i+6*YMM_LEN]));
				 _mm256_storeu_ps((float*)&dst[i+6*YMM_LEN], ymm6);
			         const __m256 ymm7(_mm256_loadu_ps((float*)&src[i+7*YMM_LEN]));
			         _mm256_storeu_ps((float*)&dst[i+7*YMM_LEN], ymm7);
			        			            	                   
		              }
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
		              for (; i != _nelems; ++i) {
			           dst[i] = src[i];
		                }
	 	                return;
	           }
	           else if (_nelems > MAXFLOATSPERPAGE4KiB) {
		            int32_t j;
		

		            for (int32_t k = 0; k != _nelems; k += MAXFLOATSPERPAGE4KiB) {
			         volatile float t = src[k + MAXFLOATSPERPAGE4KiB];
			         for (j = k + 128; j != k + MAXFLOATSPERPAGE4KiB; j += 64) {
				      _mm_prefetch((const char*)&src[j], _MM_HINT_T0);
			         }
			         for (j = k; j != ROUND_TO_EIGHT(k + MAXFLOATSPERPAGE4KiB, 8); j += 64) {
				       const __m256 ymm0(_mm256_loadu_ps((float*)&src[j+0]));
				        _mm256_storeu_ps((float*)&dst[j+0], ymm0);
				       const __m256 ymm1(_mm256_loadu_ps((float*)&src[j+1*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+1*YMM_LEN], ymm1);
				       const __m256 ymm2(_mm256_loadu_ps((float*)&src[j+2*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+2*YMM_LEN], ymm2);
				       const __m256 ymm3(_mm256_loadu_ps((float*)&src[j+3*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+3*YMM_LEN], ymm3);
				       const __m256 ymm4(_mm256_loadu_ps((float*)&src[j+4*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+4*YMM_LEN], ymm4);
				       const __m256 ymm5(_mm256_loadu_ps((float*)&src[j+5*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+5*YMM_LEN], ymm5);
				       const __m256 ymm6(_mm256_loadu_ps((float*)&src[j+6*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+6*YMM_LEN], ymm6);
				       const __m256 ymm7(_mm256_loadu_ps((float*)&src[j+7*YMM_LEN]));
				        _mm256_storeu_ps((float*)&dst[j+7*YMM_LEN], ymm7);
				       	      
				        
				  }
				    for (; j != _nelems; ++j) {
				       dst[j] = src[j];
			         }
		           }
		               return;
	                }
		}


		void ymm8r4_uncached_memove(void * __restrict _Dst,
	                                    const void * __restrict _Src,
					    const int32_t _nelems) {

                       if (_nelems <= MEMMOVE_1ELEM) { return;}
	               char * __restrict dst = (char*)_Dst;
	               const char * __restrict src = (const char*)_Src;
	               uintptr_t dst_len = (uintptr_t)dst;
	               int32_t _nbytes = 4*_nelems;
	               int32_t misalign = 0;
	           if (dst_len & 0x1F) {
		        misalign = min_val(0x20 - (dst_len & 0x1F),_nbytes);
		        dst += misalign;
		        dst_len += misalign;
		        _nbytes -= misalign;
	              }
	           if (_nelems <= MEMMOVE_16ELEMS) {
		       const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
		       _mm256_stream_ps((float*)&dst[0], ymm0);
		       const __m256 ymm1(_mm256_loadu_ps((float*)&src[1 * YMM_LEN]));
		       _mm256_stream_ps((float*)&dst[1 * YMM_LEN], ymm1);
		       _mm_sfence();
		       return;
	           }
	           else if (_nelems <= MEMMOVE_32ELEMS) {
		          const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			  _mm256_stream_ps((float*)&dst[0], ymm0);
		          const __m256 ymm1(_mm256_loadu_ps((float*)&src[1 * YMM_LEN]));
			  _mm256_stream_ps((float*)&dst[1 * YMM_LEN], ymm1);
		          const __m256 ymm2(_mm256_loadu_ps((float*)&src[2 * YMM_LEN]));
			  _mm256_stream_ps((float*)&dst[2 * YMM_LEN], ymm2);
		          const __m256 ymm3(_mm256_loadu_ps((float*)&src[3 * YMM_LEN]));
		          _mm256_stream_ps((float*)&dst[3 * YMM_LEN], ymm3);
		          _mm_sfence();
		          return;
	           }
	           else if (_nelems <= MEMMOVE_64ELEMS){
		         const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
			 _mm256_stream_ps((float*)&dst[0], ymm0);
		         const __m256 ymm1(_mm256_loadu_ps((float*)&src[1 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[1 * YMM_LEN], ymm1);
		         const __m256 ymm2(_mm256_loadu_ps((float*)&src[2 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[2 * YMM_LEN], ymm2);
		         const __m256 ymm3(_mm256_loadu_ps((float*)&src[3 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[3 * YMM_LEN], ymm3);
		         const __m256 ymm4(_mm256_loadu_ps((float*)&src[4 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[4 * YMM_LEN], ymm4);
		         const __m256 ymm5(_mm256_loadu_ps((float*)&src[5 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[5 * YMM_LEN], ymm5);
		         const __m256 ymm6(_mm256_loadu_ps((float*)&src[6 * YMM_LEN]));
			 _mm256_stream_ps((float*)&dst[6 * YMM_LEN], ymm6);
		         const __m256 ymm7(_mm256_loadu_ps((float*)&src[7 * YMM_LEN]));
	                 _mm256_stream_ps((float*)&dst[7 * YMM_LEN], ymm7);
			 _mm_sfence();
		         return;
	      }
	      else if (_nelems <= MEMMOVE_128ELEMS) {
#if (GMS_MAN_PREFETCH) == 
		_mm_prefetch((const char*)&src[0], _MM_HINT_T0);
		_mm_prefetch((const char*)&src[2 * YMM_LEN], _MM_HINT_T0);
		_mm_prefetch((const char*)&src[4 * YMM_LEN], _MM_HINT_T0);
		_mm_prefetch((const char*)&src[6 * YMM_LEN], _MM_HINT_T0);
		_mm_prefetch((const char*)&src[8 * YMM_LEN], _MM_HINT_T0);
#endif
		const __m256 ymm0(_mm256_loadu_ps((float*)&src[0]));
		_mm256_stream_ps((float*)&dst[0], ymm0);
		const __m256 ymm1(_mm256_loadu_ps((float*)&src[1 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[1 * YMM_LEN], ymm1);
		const __m256 ymm2(_mm256_loadu_ps((float*)&src[2 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[2 * YMM_LEN], ymm2);
		const __m256 ymm3(_mm256_loadu_ps((float*)&src[3 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[3 * YMM_LEN], ymm3);
		const __m256 ymm4(_mm256_loadu_ps((float*)&src[4 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[4 * YMM_LEN], ymm4);
		const __m256 ymm5(_mm256_loadu_ps((float*)&src[5 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[5 * YMM_LEN], ymm5);
		const __m256 ymm6(_mm256_loadu_ps((float*)&src[6 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[6 * YMM_LEN], ymm6);
		const __m256 ymm7(_mm256_loadu_ps((float*)&src[7 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[7 * YMM_LEN], ymm7);
		const __m256 ymm8(_mm256_loadu_ps((float*)&src[8 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[8 * YMM_LEN], ymm8);
		const __m256 ymm9(_mm256_loadu_ps((float*)&src[9 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[9 * YMM_LEN], ymm9);
		const __m256 ymm10(_mm256_loadu_ps((float*)&src[10 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[10 * YMM_LEN], ymm10);
		const __m256 ymm11(_mm256_loadu_ps((float*)&src[11 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[11 * YMM_LEN], ymm11);
		const __m256 ymm12(_mm256_loadu_ps((float*)&src[12 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[12 * YMM_LEN], ymm12);
		const __m256 ymm13(_mm256_loadu_ps((float*)&src[13 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[13 * YMM_LEN], ymm13);
		const __m256 ymm14(_mm256_loadu_ps((float*)&src[14 * YMM_LEN]));
		_mm256_stream_ps((float*)&dst[14 * YMM_LEN], ymm14);
		const __m256 ymm15(_mm256_loadu_ps((float*)&src[15 * YMM_LEN]));
	        _mm256_stream_ps((float*)&dst[15 * YMM_LEN], ymm15);
		_mm_sfence();
		return;
	  }
	  else if (_nelems <= MAXFLOATSPERPAGE4KiB){
		   int32_t i;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count(1024)
#endif
		for (i = 0; i != ROUND_TO_EIGHT(_nelems, 8); i += 64) {
			_mm_prefetch((const char*)&src[i], _MM_HINT_T0);
			const __m256 ymm0(_mm256_loadu_ps((float*)&src[i + 0]));
			_mm256_stream_ps((float*)&dst[i + 0], ymm0);
			const __m256 ymm1(_mm256_loadu_ps((float*)&src[i + 1 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 1 * YMM_LEN], ymm1);
			const __m256 ymm2(_mm256_loadu_ps((float*)&src[i + 2 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 2 * YMM_LEN], ymm2);
			const __m256 ymm3(_mm256_loadu_ps((float*)&src[i + 3 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 3 * YMM_LEN], ymm3);
			const __m256 ymm4(_mm256_loadu_ps((float*)&src[i + 4 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 4 * YMM_LEN], ymm4);
			const __m256 ymm5(_mm256_loadu_ps((float*)&src[i + 5 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 5 * YMM_LEN], ymm5);
			const __m256 ymm6(_mm256_loadu_ps((float*)&src[i + 6 * YMM_LEN]));
			_mm256_stream_ps((float*)&dst[i + 6 * YMM_LEN], ymm6);
			const __m256 ymm7(_mm256_loadu_ps((float*)&src[i + 7 * YMM_LEN]));
		        _mm256_stream_ps((float*)&dst[i + 7 * YMM_LEN], ymm7);
		  }
		        _mm_sfence();
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
		   for (; i != _nelems; ++i) {
			dst[i] = src[i];
		      }
		      return;
	   }
	   else if (_nelems > MAXFLOATSPERPAGE4KiB) {
		int32_t j;


		for (int32_t k = 0; k != _nelems; k += MAXFLOATSPERPAGE4KiB) {
			volatile float t = src[k + MAXFLOATSPERPAGE4KiB];
			for (j = k + 128; j != k + MAXFLOATSPERPAGE4KiB; j += 64) {
				_mm_prefetch((const char*)&src[j], _MM_HINT_T0);
			}
			for (j = k; j != k + MAXFLOATSPERPAGE4KiB; j += 64) {
				const __m256 ymm0(_mm256_loadu_ps((float*)&src[j + 0]));
				_mm256_stream_ps((float*)&dst[j + 0], ymm0);
				const __m256 ymm1(_mm256_loadu_ps((float*)&src[j + 1 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 1 * YMM_LEN], ymm1);
				const __m256 ymm2(_mm256_loadu_ps((float*)&src[j + 2 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 2 * YMM_LEN], ymm2);
				const __m256 ymm3(_mm256_loadu_ps((float*)&src[j + 3 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 3 * YMM_LEN], ymm3);
				const __m256 ymm4(_mm256_loadu_ps((float*)&src[j + 4 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 4 * YMM_LEN], ymm4);
				const __m256 ymm5(_mm256_loadu_ps((float*)&src[j + 5 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 5 * YMM_LEN], ymm5);
				const __m256 ymm6(_mm256_loadu_ps((float*)&src[j + 6 * YMM_LEN]));
				_mm256_stream_ps((float*)&dst[j + 6 * YMM_LEN], ymm6);
				const __m256 ymm7(_mm256_loadu_ps((float*)&src[j + 7 * YMM_LEN]));
			        _mm256_stream_ps((float*)&dst[j + 7 * YMM_LEN], ymm7);
			   }
			
		      }
		        _mm_sfence();
		        return;
	           }
		}


		 void zmm16r4_cached_u_memmove(void * __restrict _Dst,
	                                       const void * __restrict _Src,
					       const int32_t _nelems) {

                          if (MEMMOVE_1ELEM <= _nelems) { return; }
                  char * __restrict dst = (char *)_Dst;
	          const char * __restrict src = (char *)_Src;
	
	          if (_nelems <= MEMMOVE_16ELEMS) {
		      const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
		      _mm512_storeu_ps((float*)&dst[0],zmm0);
		      return;
	          }
	          else if ( _nelems <= MEMMOVE_32ELEMS) {
		         const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			 _mm512_storeu_ps((float*)&dst[0], zmm0);
		         const __m512 zmm1(_mm512_loadu_ps((float*)&src[1*ZMM_LEN]));
		         _mm512_storeu_ps((float*)&dst[1*ZMM_LEN], zmm1);
		         return;
	          }	
	          else if ( _nelems <= MEMMOVE_64ELEMS) {
		        const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			_mm512_storeu_ps((float*)&dst[0],zmm0);
		        const __m512 zmm1(_mm512_loadu_ps((float*)&src[1*ZMM_LEN]));
			_mm512_storeu_ps((float*)&dst[1*ZMM_LEN],zmm1);
		        const __m512 zmm2(_mm512_loadu_ps((float*)&src[2*ZMM_LEN]));
			_mm512_storeu_ps((float*)&dst[2*ZMM_LEN],zmm2);
		        const __m512 zmm3(_mm512_loadu_ps((float*)&src[3*ZMM_LEN]));
		        _mm512_storeu_ps((float*)&dst[3*ZMM_LEN],zmm3);
		       return;
	          }
	          else if ( _nelems <= MEMMOVE_128ELEMS) {
		        const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			 _mm512_storeu_ps((float*)&dst[0],   zmm0);
		        const __m512 zmm1(_mm512_loadu_ps((float*)&src[1*ZMM_LEN]));
			 _mm512_storeu_ps((float*)&dst[1*ZMM_LEN],  zmm1);
		        const __m512 zmm2(_mm512_loadu_ps((float*)&src[2*ZMM_LEN]));
			 _mm512_storeu_ps((float*)&dst[2*ZMM_LEN],  zmm2);
		        const __m512 zmm3(_mm512_loadu_ps((float*)&src[3*ZMM_LEN]));
			 _mm512_storeu_ps((float*)&dst[3*ZMM_LEN],  zmm3);
		        const __m512 zmm4(_mm512_loadu_ps((float*)&src[4*ZMM_LEN]));
			 _mm512_storeu_ps((float*)&dst[4*ZMM_LEN],  zmm4);
		        const __m512 zmm5(_mm512_loadu_ps((float*)&src[5*ZMM_LEN]));
			_mm512_storeu_ps((float*)&dst[5*ZMM_LEN],  zmm5);
		        const __m512 zmm6(_mm512_loadu_ps((float*)&src[6*ZMM_LEN]));
			 _mm512_storeu_ps((float*)&dst[6*ZMM_LEN],  zmm6);
		        const __m512 zmm7(_mm512_loadu_ps((float*)&src[7*ZMM_LEN]));
		         _mm512_storeu_ps((float*)&dst[7*ZMM_LEN],  zmm7);
			return;
	           }
	           else if ( _nelems <= MEMMOVE_256ELEMS) {
#if (GMS_MAN_PREFETCH)
		         _mm_prefetch((const char *)&src[0],          _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[1*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[2*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[3*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[4*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[5*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[6*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[7*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[8*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[9*ZMM_LEN],  _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[10*ZMM_LEN], _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[11*ZMM_LEN], _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[12*ZMM_LEN], _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[13*ZMM_LEN], _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[14*ZMM_LEN], _MM_HINT_T0);
		         _mm_prefetch((const char *)&src[15*ZMM_LEN], _MM_HINT_T0);
#endif
		         const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			 _mm512_storeu_ps((float*)&dst[0],	 zmm0);
		         const __m512 zmm1(_mm512_loadu_ps((float*)&src[1*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[1*ZMM_LEN],  zmm1);
		         const __m512 zmm2(_mm512_loadu_ps((float*)&src[2*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[2*ZMM_LEN],  zmm2);
		         const __m512 zmm3(_mm512_loadu_ps((float*)&src[3*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[3*ZMM_LEN],  zmm3);
		         const __m512 zmm4(_mm512_loadu_ps((float*)&src[4*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[4*ZMM_LEN],  zmm4);
		         const __m512 zmm5(_mm512_loadu_ps((float*)&src[5*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[5*ZMM_LEN],  zmm5);
		         const __m512 zmm6(_mm512_loadu_ps((float*)&src[6*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[6*ZMM_LEN],  zmm6);
		         const __m512 zmm7(_mm512_loadu_ps((float*)&src[7*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[7*ZMM_LEN],  zmm7);
		         const __m512 zmm8(_mm512_loadu_ps((float*)&src[8*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[8*ZMM_LEN],  zmm8);
		         const __m512 zmm9(_mm512_loadu_ps((float*)&src[9*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[9*ZMM_LEN],  zmm9);
		         const __m512 zmm10(_mm512_loadu_ps((float*)&src[10*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[10*ZMM_LEN], zmm10);
		         const __m512 zmm11(_mm512_loadu_ps((float*)&src[11*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[11*ZMM_LEN], zmm11);
		         const __m512 zmm12(_mm512_loadu_ps((float*)&src[12*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[12*ZMM_LEN], zmm12);
		         const __m512 zmm13(_mm512_loadu_ps((float*)&src[13*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[13*ZMM_LEN], zmm13);
		         const __m512 zmm14(_mm512_loadu_ps((float*)&src[14*ZMM_LEN]));
			  _mm512_storeu_ps((float*)&dst[14*ZMM_LEN], zmm14);
		         const __m512 zmm15(_mm512_loadu_ps((float*)&src[15*ZMM_LEN]));
	                  _mm512_storeu_ps((float*)&dst[15*ZMM_LEN], zmm15);
		 
		          return;
	           }
	           else if ( _nelems <= PAGE4KiB) {
		            int32_t i;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count(1024)
#endif
		           for (i = 0; i != ROUND_TO_SIXTEEN(_nelems,16); i += 128) {
			       _mm_prefetch((const char *)&src[i+0], _MM_HINT_T0);
			       const __m512 zmm0(_mm512_loadu_ps((float*)&src[i+0]));
			        _mm512_storeu_ps((float*)&dst[i+0],   zmm0);
			       const __m512 zmm1(_mm512_loadu_ps((float*)&src[i+1*ZMM_LEN]));
			       _mm512_storeu_ps((float*)&dst[i+1*ZMM_LEN],  zmm1);
			       const __m512 zmm2(_mm512_loadu_ps((float*)&src[i+2*ZMM_LEN]));
			       _mm512_storeu_ps((float*)&dst[i+2*ZMM_LEN],  zmm2);
			       const __m512 zmm3(_mm512_loadu_ps((float*)&src[i+3*ZMM_LEN]));
			       _mm512_storeu_ps((float*)&dst[i+3*ZMM_LEN],  zmm3);
			       const __m512 zmm4(_mm512_loadu_ps((float*)&src[i+4*ZMM_LEN]));
			       _mm512_storeu_ps((float*)&dst[i+4*ZMM_LEN],  zmm4);
			       const __m512 zmm5(_mm512_loadu_ps((float*)&src[i+5*ZMM_LEN]));
			        _mm512_storeu_ps((float*)&dst[i+5*ZMM_LEN],  zmm5);
			       const __m512 zmm6(_mm512_loadu_ps((float*)&src[i+6*ZMM_LEN]));
			        _mm512_storeu_ps((float*)&dst[i+6*ZMM_LEN],  zmm6);
			       const __m512 zmm7(_mm512_loadu_ps((float*)&src[i+7*ZMM_LEN]));
			        _mm512_storeu_ps((float*)&dst[i+7*ZMM_LEN],  zmm7);
			   }
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(8),max(15)
#endif
		           for (; i != _nelems; ++i) {
			       dst[i] = src[i];
		           }
		            return;
	             }
	             else if (_nelems > MAXFLOATSPERPAGE4KiB) {
		              int32_t j;
		              for (int32_t k = 0; k != _nelems; k += MAXFLOATSPERPAGE4KiB) {
			            volatile float t = src[k + MAXFLOATSPERPAGE4KiB];
			            for ( j = k + 128; j != k + MAXFLOATSPERPAGE4KiB; j += 128) {
				         _mm_prefetch((const char*)&src[j], _MM_HINT_T0);
			            }
			            for (j = k; j != k + MAXFLOATSPERPAGE4KiB; j += 128) {
				          const __m512 zmm0(_mm512_loadu_ps((float*)&src[j+0]));
					   _mm512_storeu_ps((float*)&dst[j+0], zmm0);
				          const __m512 zmm1(_mm512_loadu_ps((float*)&src[j+1*ZMM_LEN]));
					   _mm512_storeu_ps((float*)&dst[j+1*ZMM_LEN], zmm1);
				          const __m512 zmm2(_mm512_loadu_ps((float*)&src[j+2*ZMM_LEN]));
					   _mm512_storeu_ps((float*)&dst[j+2*ZMM_LEN], zmm2);
				          const __m512 zmm3(_mm512_loadu_ps((float*)&src[j+3*ZMM_LEN]));
					   _mm512_storeu_ps((float*)&dst[j+3*ZMM_LEN], zmm3);
				          const __m512 zmm4(_mm512_loadu_ps((float*)&src[j+4*ZMM_LEN]));
					   _mm512_storeu_ps((float*)&dst[j+4*ZMM_LEN], zmm4);
				          const __m512 zmm5(_mm512_loadu_ps((float*)&src[j+5*ZMM_LEN]));
					   _mm512_storeu_ps((float*)&dst[j+5*ZMM_LEN], zmm5);
				          const __m512 zmm6(_mm512_loadu_ps((float*)&src[j+6*ZMM_LEN]));
					  _mm512_storeu_ps((float*)&dst[j+6*ZMM_LEN], zmm6);
				          const __m512 zmm7(_mm512_loadu_ps((float*)&src[j+7*ZMM_LEN]));
				          _mm512_storeu_ps((float*)&dst[j+7*ZMM_LEN], zmm7);
				     }
			
		             }
		              return;
	                 }

		 }



		  void zmm16r4_uncached_memmove(void * __restrict _Dst,
	                                        const void * __restrict _Src,
					        const int32_t _nelems) {

                         if (MEMMOVE_1ELEM <= _nelems) { return; }
	              char * __restrict dst = (char*)_Dst;
	              const char * __restrict src = (char*)_Src;
	              uintptr_t dst_val = (uintptr_t)dst;
	              int32_t misalign = 0;
	              int32_t nbytes = 4*_nelems;
	              if (dst_val & 0x3F) {
	                  misalign = min_val(0x40 - (dst_val & 0x3F), nbytes);
		          dst += misalign;
		          dst_val += misalign;
		          nbytes -= misalign;
	              }
	              if (_nelems <= MEMMOVE_16ELEMS) {
		          const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
		          _mm512_stream_ps((float*)&dst[0], zmm0);
		          _mm_sfence();
		          return;
	              }
	              else if (_nelems <= MEMMOVE_32ELEMS) {
		           const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			    _mm512_stream_ps((float*)&dst[0], zmm0);
		           const __m512 zmm1(_mm512_loadu_ps((float*)&src[1 * ZMM_LEN]));
		           _mm512_stream_ps((float*)&dst[1 * ZMM_LEN], zmm1);
		           _mm_sfence();
		           return;
	              }
	              else if (_nelems <= MEMMOVE_64ELEMS) {
		               const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			       	_mm512_stream_ps((float*)&dst[0], zmm0);
		               const __m512 zmm1(_mm512_loadu_ps((float*)&src[1 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[1 * ZMM_LEN], zmm1);
		               const __m512 zmm2(_mm512_loadu_ps((float*)&src[2 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[2 * ZMM_LEN], zmm2);
		               const __m512 zmm3(_mm512_loadu_ps((float*)&src[3 * ZMM_LEN]));
	                       _mm512_stream_ps((float*)&dst[3 * ZMM_LEN], zmm3);
			       _mm_sfence();
		               return;
	              }
	              else if (_nelems <= MEMMOVE_128ELEMS) {
		               const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			        _mm512_stream_ps((float*)&dst[0], zmm0);
		               const __m512 zmm1(_mm512_loadu_ps((float*)&src[1 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[1 * ZMM_LEN], zmm1);
		               const __m512 zmm2(_mm512_loadu_ps((float*)&src[2 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[2 * ZMM_LEN], zmm2);
		               const __m512 zmm3(_mm512_loadu_ps((float*)&src[3 * ZMM_LEN]));
			       _mm512_stream_ps((float*)&dst[3 * ZMM_LEN], zmm3);
		               const __m512 zmm4(_mm512_loadu_ps((float*)&src[4 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[4 * ZMM_LEN], zmm4);
		               const __m512 zmm5(_mm512_loadu_ps((float*)&src[5 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[5 * ZMM_LEN], zmm5);
		               const __m512 zmm6(_mm512_loadu_ps((float*)&src[6 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[6 * ZMM_LEN], zmm6);
		               const __m512 zmm7(_mm512_loadu_ps((float*)&src[7 * ZMM_LEN]));
		               _mm512_stream_ps((float*)&dst[7 * ZMM_LEN], zmm7);
		    
			       _mm_sfence();
		               return;
	              }
	              else if (_nelems <= MEMMOVE_256ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
		               _mm_prefetch((const char *)&src[0], _MM_HINT_T0);
	 	               _mm_prefetch((const char *)&src[1 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[2 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[3 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[4 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[5 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[6 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[7 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[8 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[9 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[10 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[11 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[12 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[13 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[14 * ZMM_LEN], _MM_HINT_T0);
		               _mm_prefetch((const char *)&src[15 * ZMM_LEN], _MM_HINT_T0);
#endif
		               const __m512 zmm0(_mm512_loadu_ps((float*)&src[0]));
			       	_mm512_stream_ps((float*)&dst[0], zmm0);
		               const __m512 zmm1(_mm512_loadu_ps((float*)&src[1 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[1 * ZMM_LEN], zmm1);
		               const __m512 zmm2(_mm512_loadu_ps((float*)&src[2 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[2 * ZMM_LEN], zmm2);
		               const __m512 zmm3(_mm512_loadu_ps((float*)&src[3 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[3 * ZMM_LEN], zmm3);
		               const __m512 zmm4(_mm512_loadu_ps((float*)&src[4 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[4 * ZMM_LEN], zmm4);
		               const __m512 zmm5(_mm512_loadu_ps((float*)&src[5 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[5 * ZMM_LEN], zmm5);
		               const __m512 zmm6(_mm512_loadu_ps((float*)&src[6 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[6 * ZMM_LEN], zmm6);
		               const __m512 zmm7(_mm512_loadu_ps((float*)&src[7 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[7 * ZMM_LEN], zmm7);
		               const __m512 zmm8(_mm512_loadu_ps((float*)&src[8 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[8 * ZMM_LEN], zmm8);
		               const __m512 zmm9(_mm512_loadu_ps((float*)&src[9 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[9 * ZMM_LEN], zmm9);
		               const __m512 zmm10(_mm512_loadu_ps((float*)&src[10 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[10 * ZMM_LEN], zmm10);
		               const __m512 zmm11(_mm512_loadu_ps((float*)&src[11 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[11 * ZMM_LEN], zmm11);
		               const __m512 zmm12(_mm512_loadu_ps((float*)&src[12 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[12 * ZMM_LEN], zmm12);
		               const __m512 zmm13(_mm512_loadu_ps((float*)&src[13 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[13 * ZMM_LEN], zmm13);
		               const __m512 zmm14(_mm512_loadu_ps((float*)&src[14 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[14 * ZMM_LEN], zmm14);
		               const __m512 zmm15(_mm512_loadu_ps((float*)&src[15 * ZMM_LEN]));
	                        _mm512_stream_ps((float*)&dst[15 * ZMM_LEN], zmm15);
			        _mm_sfence();
		                return;
	             }
	             else if (_nelems <= PAGE4KiB) {
		              int32_t i;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count(1024)
#endif
		              for (i = 0; i != ROUND_TO_SIXTEEN(_nelems, 16); i += 128) {
			           _mm_prefetch((const char *)&src[i + 0], _MM_HINT_T0);
			           const __m512 zmm0(_mm512_loadu_ps((float*)&src[i + 0]));
				    _mm512_stream_ps((float*)&dst[i + 0], zmm0);
			           const __m512 zmm1(_mm512_loadu_ps((float*)&src[i + 1 * ZMM_LEN]));
				    _mm512_stream_ps((float*)&dst[i + 1 * ZMM_LEN], zmm1);
			           const __m512 zmm2(_mm512_loadu_ps((float*)&src[i + 2 * ZMM_LEN]));
				    _mm512_stream_ps((float*)&dst[i + 2 * ZMM_LEN], zmm2);
			           const __m512 zmm3(_mm512_loadu_ps((float*)&src[i + 3 * ZMM_LEN]));
				    _mm512_stream_ps((float*)&dst[i + 3 * ZMM_LEN], zmm3);
			           const __m512 zmm4(_mm512_loadu_ps((float*)&src[i + 4 * ZMM_LEN]));
				   _mm512_stream_ps((float*)&dst[i + 4 * ZMM_LEN], zmm4);
			           const __m512 zmm5(_mm512_loadu_ps((float*)&src[i + 5 * ZMM_LEN]));
				    _mm512_stream_ps((float*)&dst[i + 5 * ZMM_LEN], zmm5);
			           const __m512 zmm6(_mm512_loadu_ps((float*)&src[i + 6 * ZMM_LEN]));
				    _mm512_stream_ps((float*)&dst[i + 6 * ZMM_LEN], zmm6);
			           const __m512 zmm7(_mm512_loadu_ps((float*)&src[i + 7 * ZMM_LEN]));
			           _mm512_stream_ps((float*)&dst[i + 7 * ZMM_LEN], zmm7);
		
		               }
		                _mm_sfence();
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(8),max(15)
#endif
		                for (; i != _nelems; ++i) {
			             dst[i] = src[i];
		                 }
		                 return;
	              }
	               else if (_nelems > MAXFLOATSPERPAGE4KiB) {
		          int32_t j;
		          for (int32_t k = 0; k != _nelems; k += MAXFLOATSPERPAGE4KiB) {
			        volatile float t = src[k + MAXFLOATSPERPAGE4KiB];

			         for (j = k + 128; j != k + MAXFLOATSPERPAGE4KiB; j += 128) {
				      _mm_prefetch((const char*)&src[j], _MM_HINT_T0);
			         }

			for (j = k; j != k + MAXFLOATSPERPAGE4KiB; j += 128) {
				const __m512 zmm0(_mm512_loadu_ps((float*)&src[j + 0]));
				_mm512_stream_ps((float*)&dst[j + 0], zmm0);
				const __m512 zmm1(_mm512_loadu_ps((float*)&src[j + 1 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 1 * ZMM_LEN], zmm1);
				const __m512 zmm2(_mm512_loadu_ps((float*)&src[j + 2 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 2 * ZMM_LEN], zmm2);
				const __m512 zmm3(_mm512_loadu_ps((float*)&src[j + 3 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 3 * ZMM_LEN], zmm3);
				const __m512 zmm4(_mm512_loadu_ps((float*)&src[j + 4 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 4 * ZMM_LEN], zmm4);
				const __m512 zmm5(_mm512_loadu_ps((float*)&src[j + 5 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 5 * ZMM_LEN], zmm5);
				const __m512 zmm6(_mm512_loadu_ps((float*)&src[j + 6 * ZMM_LEN]));
				_mm512_stream_ps((float*)&dst[j + 6 * ZMM_LEN], zmm6);
				const __m512 zmm7(_mm512_loadu_ps((float*)&src[j + 7 * ZMM_LEN]));
			        _mm512_stream_ps((float*)&dst[j + 7 * ZMM_LEN], zmm7);
			  }

		       }
		        _mm_sfence();
		        return;
	            }

		  }
