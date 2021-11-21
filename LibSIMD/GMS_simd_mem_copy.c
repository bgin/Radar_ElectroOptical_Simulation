

#include <immintrin.h>
#include "GMS_simd_mem_copy.h"
#include "GMS_simd_memops_defs.h"



       void zmm16r4_memcpy_unroll8x(float * __restrict __attribute__((aligned(64))) dst,
				    const float * __restrict __attribute__((aligned(64))) src,
                                    const int32_t len) {

               #if defined __ICC || defined __INTEL_COMPILER
                     if(len <= MEMMOVE_1ELEM) {
                        return;
		     }
		     else if(len <= MEMMOVE_16ELEMS) {
                        const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			_mm512_storeu_ps(&dst[0],zmm0);
			return;
		     }
		     else if(len <= MEMMOVE_32ELEMS) {
                        const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			_mm512_storeu_ps(&dst[0],zmm0);
			const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			_mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			return;
		     }
		     else if(len <= MEMMOVE_64ELEMS) {
                        const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			_mm512_storeu_ps(&dst[0],zmm0);
			const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			_mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			_mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			_mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			return;
		     }
		     else if(len <= MEMMOVE_128ELEMS) {
                         const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
		 	 _mm512_storeu_ps(&dst[0],zmm0);
			 const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			 const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			 const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			 const __m512 zmm4 = _mm512_loadu_ps(&src[4*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			 const __m512 zmm5 = _mm512_loadu_ps(&src[5*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			 const __m512 zmm6 = _mm512_loadu_ps(&src[6*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			 const __m512 zmm7 = _m512_loadu_ps(&src[7*ZMM_LEN]);
			 _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
			return;
		     }
		     else if(len <= MEMMOVE_256ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
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
                                 const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
		 	         _mm512_storeu_ps(&dst[0],zmm0);
			         const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			         const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			         const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			         const __m512 zmm4 = _mm512_loadu_ps(&src[4*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			         const __m512 zmm5 = _mm512_loadu_ps(&src[5*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			         const __m512 zmm6 = _mm512_loadu_ps(&src[6*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			         const __m512 zmm7 = _mm512_loadu_ps(&src[7*ZMM_LEN]);
			         _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
				 const __m512 zmm8 = _mm512_loadu_ps(&src[8*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[8*ZMM_LEN],zmm8);
				 const __m512 zmm9 = _mm512_loadu_ps(&src[9*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[9*ZMM_LEN],zmm9);
				 const __m512 zmm10 = _mm512_loadu_ps(&src[10*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[10*ZMM_LEN],zmm10);
				 const __m512 zmm11 = _mm512_loadu_ps(&src[11*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[11*ZMM_LEN],zmm11);
				 const __m512 zmm12 = _mm512_loadu_ps(&src[12*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[12*ZMM_LEN],zmm12);
				 const __m512 zmm13 = _mm512_loadu_ps(&src[13*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[13*ZMM_LEN],zmm13);
				 const __m512 zmm14 = _mm512_loadu_ps(&src[14*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[14*ZMM_LEN],zmm14);
				 const __m512 zmm15 = _mm512_loadu_ps(&src[15*ZMM_LEN]);
				 _mm512_storeu_ps(&dst[15*ZMM_LEN],zmm15);
			         return;
		     }
		     else if(len > MEMMOVE_256ELEMS) {
                                 int32_t i;
#pragma code_align(32)
				 for(i = 0; i != ROUND_TO_SIXTEEN(len,16); i += 128) {
#if (GMS_MAN_PREFETCH) == 1
                                           _mm_prefetch((const char *)&src[i+0],          _MM_HINT_T0);
		                           _mm_prefetch((const char *)&src[i+1*ZMM_LEN],  _MM_HINT_T0);
		                           _mm_prefetch((const char *)&src[i+2*ZMM_LEN],  _MM_HINT_T0);
		                           _mm_prefetch((const char *)&src[i+3*ZMM_LEN],  _MM_HINT_T0);           
#endif
                                           const __m512 zmm0 = _mm512_loadu_ps(&src[i+0]);
		 	                   _mm512_storeu_ps(&dst[0],zmm0);
			                   const __m512 zmm1 = _mm512_loadu_ps(&src[i+1*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                   const __m512 zmm2 = _mm512_loadu_ps(&src[i+2*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			                   const __m512 zmm3 = _mm512_loadu_ps(&src[i+3*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			                   const __m512 zmm4 = _mm512_loadu_ps(&src[i+4*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			                   const __m512 zmm5 = _mm512_loadu_ps(&src[i+5*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			                   const __m512 zmm6 = _mm512_loadu_ps(&src[i+6*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			                   const __m512 zmm7 = _mm512_loadu_ps(&src[i+7*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
				 }
#pragma loop_count min(1),avg(8),max(15)
                                 for(; i != len; ++i) {
                                     dst[i] = src[i];
				 }
				 return;
		     }
#elif defined __GNUC__ && !defined __INTEL_COMPILER
                                 if ((reinterpret_cast<uintptr_t>(dst)& 0x3F) != 0ULL &&
		                      (reinterpret_cast<uintptr_t>(src)& 0x3F) != 0ULL) {

				      if(len <= MEMMOVE_1ELEM) {
                                         return;
		                      }
		                      else if(len <= MEMMOVE_16ELEMS) {
                                            const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			                    _mm512_storeu_ps(&dst[0],zmm0);
			                    return;
		                      }
		                      else if(len <= MEMMOVE_32ELEMS) {
                                           const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			                   _mm512_storeu_ps(&dst[0],zmm0);
			                   const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                   return;
		                      }
		                      else if(len <= MEMMOVE_64ELEMS) {
                                           const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
			                   _mm512_storeu_ps(&dst[0],zmm0);
			                   const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                   const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			                   const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			                   return;
		                     }
		                     else if(len <= MEMMOVE_128ELEMS) {
                                           const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
		 	                   _mm512_storeu_ps(&dst[0],zmm0);
			                   const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                   const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			                   const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			                   const __m512 zmm4 = _mm512_loadu_ps(&src[4*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			                   const __m512 zmm5 = _mm512_loadu_ps(&src[5*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			                   const __m512 zmm6 = _mm512_loadu_ps(&src[6*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			                    const __m512 zmm7 = _m512_loadu_ps(&src[7*ZMM_LEN]);
			                   _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
			                   return;
		                     }
		                     else if(len <= MEMMOVE_256ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
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
                                            const __m512 zmm0 = _mm512_loadu_ps(&src[0]);
		 	                    _mm512_storeu_ps(&dst[0],zmm0);
			                    const __m512 zmm1 = _mm512_loadu_ps(&src[1*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                    const __m512 zmm2 = _mm512_loadu_ps(&src[2*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			                    const __m512 zmm3 = _mm512_loadu_ps(&src[3*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			                    const __m512 zmm4 = _mm512_loadu_ps(&src[4*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			                    const __m512 zmm5 = _mm512_loadu_ps(&src[5*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			                    const __m512 zmm6 = _mm512_loadu_ps(&src[6*ZMM_LEN]);
			                    _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			                     const __m512 zmm7 = _mm512_loadu_ps(&src[7*ZMM_LEN]);
			                     _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
				            const __m512 zmm8 = _mm512_loadu_ps(&src[8*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[8*ZMM_LEN],zmm8);
				            const __m512 zmm9 = _mm512_loadu_ps(&src[9*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[9*ZMM_LEN],zmm9);
				            const __m512 zmm10 = _mm512_loadu_ps(&src[10*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[10*ZMM_LEN],zmm10);
				            const __m512 zmm11 = _mm512_loadu_ps(&src[11*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[11*ZMM_LEN],zmm11);
				            const __m512 zmm12 = _mm512_loadu_ps(&src[12*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[12*ZMM_LEN],zmm12);
				            const __m512 zmm13 = _mm512_loadu_ps(&src[13*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[13*ZMM_LEN],zmm13);
				            const __m512 zmm14 = _mm512_loadu_ps(&src[14*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[14*ZMM_LEN],zmm14);
				            const __m512 zmm15 = _mm512_loadu_ps(&src[15*ZMM_LEN]);
				            _mm512_storeu_ps(&dst[15*ZMM_LEN],zmm15);
			                    return;
		                   }
		                   else if(len > MEMMOVE_256ELEMS) {
                                          int32_t i;

				          for(i = 0; i != ROUND_TO_SIXTEEN(len,16); i += 128) {
#if (GMS_MAN_PREFETCH) == 1
                                                  _mm_prefetch((const char *)&src[i+0],          _MM_HINT_T0);
		                                  _mm_prefetch((const char *)&src[i+1*ZMM_LEN],  _MM_HINT_T0);
		                                  _mm_prefetch((const char *)&src[i+2*ZMM_LEN],  _MM_HINT_T0);
		                                  _mm_prefetch((const char *)&src[i+3*ZMM_LEN],  _MM_HINT_T0);           
#endif
                                                  const __m512 zmm0 = _mm512_loadu_ps(&src[i+0]);
		 	                          _mm512_storeu_ps(&dst[0],zmm0);
			                          const __m512 zmm1 = _mm512_loadu_ps(&src[i+1*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[1*ZMM_LEN],zmm1);
			                          const __m512 zmm2 = _mm512_loadu_ps(&src[i+2*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[2*ZMM_LEN],zmm2);
			                          const __m512 zmm3 = _mm512_loadu_ps(&src[i+3*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[3*ZMM_LEN],zmm3);
			                          const __m512 zmm4 = _mm512_loadu_ps(&src[i+4*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[4*ZMM_LEN],zmm4);
			                          const __m512 zmm5 = _mm512_loadu_ps(&src[i+5*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[5*ZMM_LEN],zmm5);
			                          const __m512 zmm6 = _mm512_loadu_ps(&src[i+6*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[6*ZMM_LEN],zmm6);
			                          const __m512 zmm7 = _mm512_loadu_ps(&src[i+7*ZMM_LEN]);
			                          _mm512_storeu_ps(&dst[7*ZMM_LEN],zmm7);
				            }
                                           for(; i != len; ++i) {
                                                dst[i] = src[i];
				           }
				            return;   
				    }
				    else {

				                 if(len <= MEMMOVE_1ELEM) {
                                                    return;
		                               }
		                               else if(len <= MEMMOVE_16ELEMS) {
                                                       const __m512 zmm0 = _mm512_load_ps(&src[0]);
			                               _mm512_store_ps(&dst[0],zmm0);
			                               return;
		                               }
		                               else if(len <= MEMMOVE_32ELEMS) {
                                                      const __m512 zmm0 = _mm512_load_ps(&src[0]);
			                              _mm512_store_ps(&dst[0],zmm0);
			                              const __m512 zmm1 = _mm512_load_ps(&src[1*ZMM_LEN]);
			                              _mm512_store_ps(&dst[1*ZMM_LEN],zmm1);
			                              return;
		                               }
		                               else if(len <= MEMMOVE_64ELEMS) {
                                                      const __m512 zmm0 = _mm512_load_ps(&src[0]);
			                              _mm512_store_ps(&dst[0],zmm0);
			                              const __m512 zmm1 = _mm512_load_ps(&src[1*ZMM_LEN]);
			                              _mm512_store_ps(&dst[1*ZMM_LEN],zmm1);
			                              const __m512 zmm2 = _mm512_load_ps(&src[2*ZMM_LEN]);
			                              _mm512_store_ps(&dst[2*ZMM_LEN],zmm2);
			                              const __m512 zmm3 = _mm512_load_ps(&src[3*ZMM_LEN]);
			                              _mm512_store_ps(&dst[3*ZMM_LEN],zmm3);
			                              return;
		                               }
		                               else if(len <= MEMMOVE_128ELEMS) {
                                                      const __m512 zmm0 = _mm512_load_ps(&src[0]);
		 	                              _mm512_store_ps(&dst[0],zmm0);
			                              const __m512 zmm1 = _mm512_load_ps(&src[1*ZMM_LEN]);
			                              _mm512_store_ps(&dst[1*ZMM_LEN],zmm1);
			                              const __m512 zmm2 = _mm512_load_ps(&src[2*ZMM_LEN]);
			                              _mm512_store_ps(&dst[2*ZMM_LEN],zmm2);
			                              const __m512 zmm3 = _mm512_load_ps(&src[3*ZMM_LEN]);
			                              _mm512_store_ps(&dst[3*ZMM_LEN],zmm3);
			                              const __m512 zmm4 = _mm512_load_ps(&src[4*ZMM_LEN]);
			                              _mm512_store_ps(&dst[4*ZMM_LEN],zmm4);
			                              const __m512 zmm5 = _mm512_load_ps(&src[5*ZMM_LEN]);
			                              _mm512_store_ps(&dst[5*ZMM_LEN],zmm5);
			                              const __m512 zmm6 = _mm512_load_ps(&src[6*ZMM_LEN]);
			                              _mm512_store_ps(&dst[6*ZMM_LEN],zmm6);
			                              const __m512 zmm7 = _m512_load_ps(&src[7*ZMM_LEN]);
			                              _mm512_store_ps(&dst[7*ZMM_LEN],zmm7);
			                              return;
		                               }
		                               else if(len <= MEMMOVE_256ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
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
                                                     const __m512 zmm0 = _mm512_load_ps(&src[0]);
		 	                             _mm512_store_ps(&dst[0],zmm0);
			                             const __m512 zmm1 = _mm512_load_ps(&src[1*ZMM_LEN]);
			                             _mm512_store_ps(&dst[1*ZMM_LEN],zmm1);
			                             const __m512 zmm2 = _mm512_load_ps(&src[2*ZMM_LEN]);
			                             _mm512_store_ps(&dst[2*ZMM_LEN],zmm2);
			                             const __m512 zmm3 = _mm512_load_ps(&src[3*ZMM_LEN]);
			                             _mm512_store_ps(&dst[3*ZMM_LEN],zmm3);
			                             const __m512 zmm4 = _mm512_load_ps(&src[4*ZMM_LEN]);
			                             _mm512_store_ps(&dst[4*ZMM_LEN],zmm4);
			                             const __m512 zmm5 = _mm512_load_ps(&src[5*ZMM_LEN]);
			                             _mm512_store_ps(&dst[5*ZMM_LEN],zmm5);
			                             const __m512 zmm6 = _mm512_load_ps(&src[6*ZMM_LEN]);
			                             _mm512_store_ps(&dst[6*ZMM_LEN],zmm6);
			                             const __m512 zmm7 = _mm512_load_ps(&src[7*ZMM_LEN]);
			                             _mm512_store_ps(&dst[7*ZMM_LEN],zmm7);
				                     const __m512 zmm8 = _mm512_load_ps(&src[8*ZMM_LEN]);
				                     _mm512_store_ps(&dst[8*ZMM_LEN],zmm8);
				                     const __m512 zmm9 = _mm512_load_ps(&src[9*ZMM_LEN]);
				                     _mm512_store_ps(&dst[9*ZMM_LEN],zmm9);
				                     const __m512 zmm10 = _mm512_load_ps(&src[10*ZMM_LEN]);
				                     _mm512_store_ps(&dst[10*ZMM_LEN],zmm10);
				                     const __m512 zmm11 = _mm512_load_ps(&src[11*ZMM_LEN]);
				                     _mm512_store_ps(&dst[11*ZMM_LEN],zmm11);
				                     const __m512 zmm12 = _mm512_load_ps(&src[12*ZMM_LEN]);
				                     _mm512_store_ps(&dst[12*ZMM_LEN],zmm12);
				                     const __m512 zmm13 = _mm512_load_ps(&src[13*ZMM_LEN]);
				                     _mm512_store_ps(&dst[13*ZMM_LEN],zmm13);
				                     const __m512 zmm14 = _mm512_load_ps(&src[14*ZMM_LEN]);
				                     _mm512_store_ps(&dst[14*ZMM_LEN],zmm14);
				                     const __m512 zmm15 = _mm512_load_ps(&src[15*ZMM_LEN]);
				                     _mm512_store_ps(&dst[15*ZMM_LEN],zmm15);
			                             return;
		                           }
		                           else if(len > MEMMOVE_256ELEMS) {
                                                   int32_t i;
                                                    src = (float*)__builtin_assume_aligned(src,64);
				                    dst = (const float*)__builtin_assume_aligned(dst,64);
				                   for(i = 0; i != ROUND_TO_SIXTEEN(len,16); i += 128) {
#if (GMS_MAN_PREFETCH) == 1
                                                          _mm_prefetch((const char *)&src[i+0],          _MM_HINT_T0);
		                                          _mm_prefetch((const char *)&src[i+1*ZMM_LEN],  _MM_HINT_T0);
		                                          _mm_prefetch((const char *)&src[i+2*ZMM_LEN],  _MM_HINT_T0);
		                                          _mm_prefetch((const char *)&src[i+3*ZMM_LEN],  _MM_HINT_T0);           
#endif
                                                          const __m512 zmm0 = _mm512_load_ps(&src[i+0]);
		 	                                  _mm512_store_ps(&dst[0],zmm0);
			                                  const __m512 zmm1 = _mm512_load_ps(&src[i+1*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[1*ZMM_LEN],zmm1);
			                                  const __m512 zmm2 = _mm512_load_ps(&src[i+2*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[2*ZMM_LEN],zmm2);
			                                  const __m512 zmm3 = _mm512_load_ps(&src[i+3*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[3*ZMM_LEN],zmm3);
			                                  const __m512 zmm4 = _mm512_load_ps(&src[i+4*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[4*ZMM_LEN],zmm4);
			                                  const __m512 zmm5 = _mm512_load_ps(&src[i+5*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[5*ZMM_LEN],zmm5);
			                                  const __m512 zmm6 = _mm512_load_ps(&src[i+6*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[6*ZMM_LEN],zmm6);
			                                  const __m512 zmm7 = _mm512_load_ps(&src[i+7*ZMM_LEN]);
			                                  _mm512_store_ps(&dst[7*ZMM_LEN],zmm7);
				                  }
                                                  for(; i != len; ++i) {
                                                       dst[i] = src[i];
				                  }
				                  return;   
				      }
#endif

                              }


	 void  ymm8r4_memcpy_unroll8x(float * __restrict __attribute__((aligned(32))) dst,
				      const float * __restrict __attribute__((aligned(32))) src,
                                      const int32_t len) {

#if defined __ICC || defined __INTEL_COMPILER
                    if(len <= MEMMOVE_1ELEM) {
                       return;
		    }
		    else if(len <= MEMMOVE_16ELEMS) {
                            const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			    _mm256_storeu_ps(&dst[0],ymm0);
			    const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			    _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			    return;
		    }
		    else if(len <= MEMMOVE_32ELEMS) {
                            const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			    _mm256_storeu_ps(&dst[0],ymm0);
			    const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			    _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			    const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			    _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			    const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			    _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			    return;
		    }
		    else if(len <= MEMMOVE_64ELEMS) {
                            const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			    _mm256_storeu_ps(&dst[0],ymm0);
			    const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			    _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			    const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			    _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			    const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			    _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			    const __m256 ymm4 = _mm256_loadu_ps(&src[4*YMM_LEN]);
			    _mm256_storeu_ps(&dst[4*YMM_LEN],ymm4);
			    const __m256 ymm5 = _mm256_loadu_ps(&src[5*YMM_LEN]);
			    _mm256_storeu_ps(&dst[5*YMM_LEN],ymm5);
			    const __m256 ymm6 = _mm256_loadu_ps(&src[6*YMM_LEN]);
			    _mm256_storeu_ps(&dst[6*YMM_LEN],ymm6);
			    const __m256 ymm7 = _mm256_loadu_ps(&src[7*YMM_LEN]);
			    _mm256_storeu_ps(&dst[7*YMM_LEN],ymm7);
			    return;
		    }
		    else if(len <= MEMMOVE_128ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
		            _mm_prefetch((const char*)&src[0], _MM_HINT_T0);
		            _mm_prefetch((const char*)&src[2*YMM_LEN],_MM_HINT_T0);
		            _mm_prefetch((const char*)&src[4*YMM_LEN],_MM_HINT_T0);
		            _mm_prefetch((const char*)&src[6*YMM_LEN],_MM_HINT_T0);
		            _mm_prefetch((const char*)&src[8*YMM_LEN],_MM_HINT_T0);
#endif
                            const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			    _mm256_storeu_ps(&dst[0],ymm0);
			    const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			    _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			    const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			    _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			    const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			    _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			    const __m256 ymm4 = _mm256_loadu_ps(&src[4*YMM_LEN]);
			    _mm256_storeu_ps(&dst[4*YMM_LEN],ymm4);
			    const __m256 ymm5 = _mm256_loadu_ps(&src[5*YMM_LEN]);
			    _mm256_storeu_ps(&dst[5*YMM_LEN],ymm5);
			    const __m256 ymm6 = _mm256_loadu_ps(&src[6*YMM_LEN]);
			    _mm256_storeu_ps(&dst[6*YMM_LEN],ymm6);
			    const __m256 ymm7 = _mm256_loadu_ps(&src[7*YMM_LEN]);
			    _mm256_storeu_ps(&dst[7*YMM_LEN],ymm7);
			    const __m256 ymm8 = _mm256_loadu_ps(&src[8*YMM_LEN]);
			    _mm256_storeu_ps(&dst[8*YMM_LEN],ymm8);
			    const __m256 ymm9 = _mm256_loadu_ps(&src[9*YMM_LEN]);
			    _mm256_storeu_ps(&dst[9*YMM_LEN],ymm9);
			    const __m256 ymm10 = _mm256_loadu_ps(&src[10*YMM_LEN]);
			    _mm256_storeu_ps(&dst[10*YMM_LEN],ymm10);
			    const __m256 ymm11 = _mm256_loadu_ps(&src[11*YMM_LEN]);
			    _mm256_storeu_ps(&dst[11*YMM_LEN],ymm11);
			    const __m256 ymm12 = _mm256_loadu_ps(&src[12*YMM_LEN]);
			    _mm256_storeu_ps(&dst[12*YMM_LEN],ymm12);
			    const __m256 ymm13 = _mm256_loadu_ps(&src[13*YMM_LEN]);
			    _mm256_storeu_ps(&dst[13*YMM_LEN],ymm13);
			    const __m256 ymm14 = _mm256_loadu_ps(&src[14*YMM_LEN]);
			    _mm256_storeu_ps(&dst[14*YMM_LEN],ymm14);
			    const __m256 ymm15 = _mm256_loadu_ps(&src[15*YMM_LEN]);
			    _mm256_storeu_ps(&dst[15*YMM_LEN],ymm155);
			    return;
		    }
		    else if(len > MEMMOVE_128ELEMS) {
		            int32_t i;
#pragma code_align(32)
                            for(i = 0; i != ROUND_TO_EIGHT(len,8); i += 64) {
#if (GMS_MAN_PREFETCH) == 1
                            _mm_prefetch((const char*)&src[i+0], _MM_HINT_T0);
		            _mm_prefetch((const char*)&src[i+2*YMM_LEN],_MM_HINT_T0);
		            _mm_prefetch((const char*)&src[i+4*YMM_LEN],_MM_HINT_T0);
#endif
                            const __m256 ymm0 = _mm256_loadu_ps(&src[i+0]);
			    _mm256_storeu_ps(&dst[i+0],ymm0);
			    const __m256 ymm1 = _mm256_loadu_ps(&src[i+1*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+1*YMM_LEN],ymm1);
			    const __m256 ymm2 = _mm256_loadu_ps(&src[i+2*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+2*YMM_LEN],ymm2);
			    const _mm256 ymm3 = _mm256_loadu_ps(&src[i+3*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+3*YMM_LEN],ymm3);
			    const __m256 ymm4 = _mm256_loadu_ps(&src[i+4*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+4*YMM_LEN],ymm4);
			    const __m256 ymm5 = _mm256_loadu_ps(&src[i+5*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+5*YMM_LEN],ymm5);
			    const __m256 ymm6 = _mm256_loadu_ps(&src[i+6*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+6*YMM_LEN],ymm6);
			    const __m256 ymm7 = _mm256_loadu_ps(&src[i+7*YMM_LEN]);
			    _mm256_storeu_ps(&dst[i+7*YMM_LEN],ymm7);  
			    }
#pragma loop_count min(1),avg(4),max(7)
                           for(; i != len; ++i) {
                               dst[i] = src[i];
			   }
			   return;
		    }
		    
 
#elif defined __GNUC__ && !defined __INTEL_COMPILER
                   	if ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0ULL &&
	                    (reinterpret_cast<uintptr_t>(src)& 0x1F) != 0ULL) {

			        if(len <= MEMMOVE_1ELEM) {
                                   return;
		                }
		                else if(len <= MEMMOVE_16ELEMS) {
                                   const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			           _mm256_storeu_ps(&dst[0],ymm0);
			           const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			           _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			           return;
		                }
		                else if(len <= MEMMOVE_32ELEMS) {
                                  const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			          _mm256_storeu_ps(&dst[0],ymm0);
			          const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			          _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			          const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			          _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			          const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			          _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			          return;
		                }
		                else if(len <= MEMMOVE_64ELEMS) {
                                    const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			            _mm256_storeu_ps(&dst[0],ymm0);
			            const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			            _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			            const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			            _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			            const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			            _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			            const __m256 ymm4 = _mm256_loadu_ps(&src[4*YMM_LEN]);
			            _mm256_storeu_ps(&dst[4*YMM_LEN],ymm4);
			            const __m256 ymm5 = _mm256_loadu_ps(&src[5*YMM_LEN]);
			            _mm256_storeu_ps(&dst[5*YMM_LEN],ymm5);
			            const __m256 ymm6 = _mm256_loadu_ps(&src[6*YMM_LEN]);
			            _mm256_storeu_ps(&dst[6*YMM_LEN],ymm6);
			            const __m256 ymm7 = _mm256_loadu_ps(&src[7*YMM_LEN]);
			            _mm256_storeu_ps(&dst[7*YMM_LEN],ymm7);
			            return;
		                }
		                else if(len <= MEMMOVE_128ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
		                    _mm_prefetch((const char*)&src[0], _MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[2*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[4*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[6*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[8*YMM_LEN],_MM_HINT_T0);
#endif
                                    const __m256 ymm0 = _mm256_loadu_ps(&src[0]);
			            _mm256_storeu_ps(&dst[0],ymm0);
			            const __m256 ymm1 = _mm256_loadu_ps(&src[1*YMM_LEN]);
			            _mm256_storeu_ps(&dst[1*YMM_LEN],ymm1);
			            const __m256 ymm2 = _mm256_loadu_ps(&src[2*YMM_LEN]);
			            _mm256_storeu_ps(&dst[2*YMM_LEN],ymm2);
			            const _mm256 ymm3 = _mm256_loadu_ps(&src[3*YMM_LEN]);
			            _mm256_storeu_ps(&dst[3*YMM_LEN],ymm3);
			            const __m256 ymm4 = _mm256_loadu_ps(&src[4*YMM_LEN]);
			            _mm256_storeu_ps(&dst[4*YMM_LEN],ymm4);
			            const __m256 ymm5 = _mm256_loadu_ps(&src[5*YMM_LEN]);
			            _mm256_storeu_ps(&dst[5*YMM_LEN],ymm5);
			            const __m256 ymm6 = _mm256_loadu_ps(&src[6*YMM_LEN]);
			            _mm256_storeu_ps(&dst[6*YMM_LEN],ymm6);
			            const __m256 ymm7 = _mm256_loadu_ps(&src[7*YMM_LEN]);
			            _mm256_storeu_ps(&dst[7*YMM_LEN],ymm7);
			            const __m256 ymm8 = _mm256_loadu_ps(&src[8*YMM_LEN]);
			            _mm256_storeu_ps(&dst[8*YMM_LEN],ymm8);
			            const __m256 ymm9 = _mm256_loadu_ps(&src[9*YMM_LEN]);
			            _mm256_storeu_ps(&dst[9*YMM_LEN],ymm9);
			            const __m256 ymm10 = _mm256_loadu_ps(&src[10*YMM_LEN]);
			            _mm256_storeu_ps(&dst[10*YMM_LEN],ymm10);
			            const __m256 ymm11 = _mm256_loadu_ps(&src[11*YMM_LEN]);
			            _mm256_storeu_ps(&dst[11*YMM_LEN],ymm11);
			            const __m256 ymm12 = _mm256_loadu_ps(&src[12*YMM_LEN]);
			            _mm256_storeu_ps(&dst[12*YMM_LEN],ymm12);
			            const __m256 ymm13 = _mm256_loadu_ps(&src[13*YMM_LEN]);
			            _mm256_storeu_ps(&dst[13*YMM_LEN],ymm13);
			             const __m256 ymm14 = _mm256_loadu_ps(&src[14*YMM_LEN]);
			            _mm256_storeu_ps(&dst[14*YMM_LEN],ymm14);
			            const __m256 ymm15 = _mm256_loadu_ps(&src[15*YMM_LEN]);
			            _mm256_storeu_ps(&dst[15*YMM_LEN],ymm155);
			             return;
		             }
		             else if(len > MEMMOVE_128ELEMS) {
		                     int32_t i;

                                      for(i = 0; i != ROUND_TO_EIGHT(len,8); i += 64) {
#if (GMS_MAN_PREFETCH) == 1
                                        _mm_prefetch((const char*)&src[i+0], _MM_HINT_T0);
		                        _mm_prefetch((const char*)&src[i+2*YMM_LEN],_MM_HINT_T0);
		                        _mm_prefetch((const char*)&src[i+4*YMM_LEN],_MM_HINT_T0);
#endif
                                        const __m256 ymm0 = _mm256_loadu_ps(&src[i+0]);
			                _mm256_storeu_ps(&dst[i+0],ymm0);
			                const __m256 ymm1 = _mm256_loadu_ps(&src[i+1*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+1*YMM_LEN],ymm1);
			                const __m256 ymm2 = _mm256_loadu_ps(&src[i+2*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+2*YMM_LEN],ymm2);
			                const _mm256 ymm3 = _mm256_loadu_ps(&src[i+3*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+3*YMM_LEN],ymm3);
			                const __m256 ymm4 = _mm256_loadu_ps(&src[i+4*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+4*YMM_LEN],ymm4);
			                const __m256 ymm5 = _mm256_loadu_ps(&src[i+5*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+5*YMM_LEN],ymm5);
			                const __m256 ymm6 = _mm256_loadu_ps(&src[i+6*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+6*YMM_LEN],ymm6);
			                const __m256 ymm7 = _mm256_loadu_ps(&src[i+7*YMM_LEN]);
			                _mm256_storeu_ps(&dst[i+7*YMM_LEN],ymm7);  
			               }
 
                                       for(; i != len; ++i) {
                                           dst[i] = src[i];
			                }
			                return;
		                    }
			      }
			       else {

			        if(len <= MEMMOVE_1ELEM) {
                                         return;
		                }
		                else if(len <= MEMMOVE_16ELEMS) {
                                   const __m256 ymm0 = _mm256_load_ps(&src[0]);
			           _mm256_store_ps(&dst[0],ymm0);
			           const __m256 ymm1 = _mm256_load_ps(&src[1*YMM_LEN]);
			           _mm256_store_ps(&dst[1*YMM_LEN],ymm1);
			           return;
		                }
		                else if(len <= MEMMOVE_32ELEMS) {
                                  const __m256 ymm0 = _mm256_load_ps(&src[0]);
			          _mm256_store_ps(&dst[0],ymm0);
			          const __m256 ymm1 = _mm256_load_ps(&src[1*YMM_LEN]);
			          _mm256_store_ps(&dst[1*YMM_LEN],ymm1);
			          const __m256 ymm2 = _mm256_load_ps(&src[2*YMM_LEN]);
			          _mm256_store_ps(&dst[2*YMM_LEN],ymm2);
			          const _mm256 ymm3 = _mm256_load_ps(&src[3*YMM_LEN]);
			          _mm256_store_ps(&dst[3*YMM_LEN],ymm3);
			          return;
		                }
		                else if(len <= MEMMOVE_64ELEMS) {
                                    const __m256 ymm0 = _mm256_load_ps(&src[0]);
			            _mm256_store_ps(&dst[0],ymm0);
			            const __m256 ymm1 = _mm256_load_ps(&src[1*YMM_LEN]);
			            _mm256_store_ps(&dst[1*YMM_LEN],ymm1);
			            const __m256 ymm2 = _mm256_load_ps(&src[2*YMM_LEN]);
			            _mm256_store_ps(&dst[2*YMM_LEN],ymm2);
			            const _mm256 ymm3 = _mm256_load_ps(&src[3*YMM_LEN]);
			            _mm256_store_ps(&dst[3*YMM_LEN],ymm3);
			            const __m256 ymm4 = _mm256_load_ps(&src[4*YMM_LEN]);
			            _mm256_store_ps(&dst[4*YMM_LEN],ymm4);
			            const __m256 ymm5 = _mm256_load_ps(&src[5*YMM_LEN]);
			            _mm256_store_ps(&dst[5*YMM_LEN],ymm5);
			            const __m256 ymm6 = _mm256_load_ps(&src[6*YMM_LEN]);
			            _mm256_store_ps(&dst[6*YMM_LEN],ymm6);
			            const __m256 ymm7 = _mm256_load_ps(&src[7*YMM_LEN]);
			            _mm256_store_ps(&dst[7*YMM_LEN],ymm7);
			            return;
		                }
		                else if(len <= MEMMOVE_128ELEMS) {
#if (GMS_MAN_PREFETCH) == 1
		                    _mm_prefetch((const char*)&src[0], _MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[2*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[4*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[6*YMM_LEN],_MM_HINT_T0);
		                    _mm_prefetch((const char*)&src[8*YMM_LEN],_MM_HINT_T0);
#endif
                                    const __m256 ymm0 = _mm256_load_ps(&src[0]);
			            _mm256_store_ps(&dst[0],ymm0);
			            const __m256 ymm1 = _mm256_load_ps(&src[1*YMM_LEN]);
			            _mm256_store_ps(&dst[1*YMM_LEN],ymm1);
			            const __m256 ymm2 = _mm256_load_ps(&src[2*YMM_LEN]);
			            _mm256_store_ps(&dst[2*YMM_LEN],ymm2);
			            const _mm256 ymm3 = _mm256_load_ps(&src[3*YMM_LEN]);
			            _mm256_store_ps(&dst[3*YMM_LEN],ymm3);
			            const __m256 ymm4 = _mm256_load_ps(&src[4*YMM_LEN]);
			            _mm256_store_ps(&dst[4*YMM_LEN],ymm4);
			            const __m256 ymm5 = _mm256_load_ps(&src[5*YMM_LEN]);
			            _mm256_store_ps(&dst[5*YMM_LEN],ymm5);
			            const __m256 ymm6 = _mm256_load_ps(&src[6*YMM_LEN]);
			            _mm256_store_ps(&dst[6*YMM_LEN],ymm6);
			            const __m256 ymm7 = _mm256_load_ps(&src[7*YMM_LEN]);
			            _mm256_store_ps(&dst[7*YMM_LEN],ymm7);
			            const __m256 ymm8 = _mm256_load_ps(&src[8*YMM_LEN]);
			            _mm256_store_ps(&dst[8*YMM_LEN],ymm8);
			            const __m256 ymm9 = _mm256_load_ps(&src[9*YMM_LEN]);
			            _mm256_store_ps(&dst[9*YMM_LEN],ymm9);
			            const __m256 ymm10 = _mm256_load_ps(&src[10*YMM_LEN]);
			            _mm256_store_ps(&dst[10*YMM_LEN],ymm10);
			            const __m256 ymm11 = _mm256_load_ps(&src[11*YMM_LEN]);
			            _mm256_store_ps(&dst[11*YMM_LEN],ymm11);
			            const __m256 ymm12 = _mm256_load_ps(&src[12*YMM_LEN]);
			            _mm256_store_ps(&dst[12*YMM_LEN],ymm12);
			            const __m256 ymm13 = _mm256_load_ps(&src[13*YMM_LEN]);
			            _mm256_store_ps(&dst[13*YMM_LEN],ymm13);
			             const __m256 ymm14 = _mm256_load_ps(&src[14*YMM_LEN]);
			            _mm256_store_ps(&dst[14*YMM_LEN],ymm14);
			            const __m256 ymm15 = _mm256_load_ps(&src[15*YMM_LEN]);
			            _mm256_store_ps(&dst[15*YMM_LEN],ymm155);
			             return;
		             }
		             else if(len > MEMMOVE_128ELEMS) {
		                      int32_t i;
                                      src = (float*)__builtin_assume_aligned(src,32);
				      dst = (const float*)__builtin_assume_aligned(dst,32);
                                      for(i = 0; i != ROUND_TO_EIGHT(len,8); i += 64) {
#if (GMS_MAN_PREFETCH) == 1
                                        _mm_prefetch((const char*)&src[i+0], _MM_HINT_T0);
		                        _mm_prefetch((const char*)&src[i+2*YMM_LEN],_MM_HINT_T0);
		                        _mm_prefetch((const char*)&src[i+4*YMM_LEN],_MM_HINT_T0);
#endif
                                        const __m256 ymm0 = _mm256_load_ps(&src[i+0]);
			                _mm256_store_ps(&dst[i+0],ymm0);
			                const __m256 ymm1 = _mm256_load_ps(&src[i+1*YMM_LEN]);
			                _mm256_store_ps(&dst[i+1*YMM_LEN],ymm1);
			                const __m256 ymm2 = _mm256_load_ps(&src[i+2*YMM_LEN]);
			                _mm256_store_ps(&dst[i+2*YMM_LEN],ymm2);
			                const _mm256 ymm3 = _mm256_load_ps(&src[i+3*YMM_LEN]);
			                _mm256_store_ps(&dst[i+3*YMM_LEN],ymm3);
			                const __m256 ymm4 = _mm256_load_ps(&src[i+4*YMM_LEN]);
			                _mm256_store_ps(&dst[i+4*YMM_LEN],ymm4);
			                const __m256 ymm5 = _mm256_load_ps(&src[i+5*YMM_LEN]);
			                _mm256_store_ps(&dst[i+5*YMM_LEN],ymm5);
			                const __m256 ymm6 = _mm256_load_ps(&src[i+6*YMM_LEN]);
			                _mm256_store_ps(&dst[i+6*YMM_LEN],ymm6);
			                const __m256 ymm7 = _mm256_load_ps(&src[i+7*YMM_LEN]);
			                _mm256_storeups(&dst[i+7*YMM_LEN],ymm7);  
			               }
 
                                         for(; i != len; ++i) {
                                             dst[i] = src[i];
			                }
			                return;
			     }
#endif
		  

		     }
