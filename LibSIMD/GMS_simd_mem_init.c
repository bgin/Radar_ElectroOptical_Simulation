
#include <immintrin.h>
#include "GMS_simd_mem_init.h"
#include "GMS_simd_memops_defs.h"





              void zmm16r4_init_unroll8x(float *   __restrict __attribute__((aligned(64))) v,
			                 const int32_t vlen,
			                 const float val) {

                          __m512 zmm = _mm512_set1_ps(val);
#if defined __ICC || defined __INTEL_COMPILER
                    if(vlen <= MEMMOVE_1ELEM) {
                       return;
		    }
		    else if(vlen <= MEMMOVE_16ELEMS) {
                        _mm512_storeu_ps(&v[0],zmm);
			return;
		    }
		    else if(vlen <= MEMMOVE_32ELEMS) {
                        _mm512_storeu_ps(&v[0],zmm);
			_mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			return;
		    }
		    else if(vlen <= MEMMOVE_64ELEMS) {
                        _mm512_storeu_ps(&v[0],zmm);
			_mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			return;
		    }
		    else if(vlen <= MEMMOVE_128ELEMS) {
                        _mm512_storeu_ps(&v[0],zmm);
			_mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[4*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[5*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[6*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[7*ZMM_LEN],zmm);
			return;
		    }
		    else if(vlen <= MEMMOVE_256ELEMS) {
                        _mm512_storeu_ps(&v[0],zmm);
			_mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[4*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[5*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[6*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[7*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[8*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[9*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[10*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[11*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[12*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[13*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[14*ZMM_LEN],zmm);
			_mm512_storeu_ps(&v[15*ZMM_LEN],zmm);
			return;
		    }
		    else if(vlen > MEMMOVE_256ELEMS) {
                         int32_t i;
#pragma code_align(32)
                         for(i = 0; i != ROUND_TO_SIXTEEN(vlen,16); i += 128) {
	                      _mm512_storeu_ps(&v[i+0],  zmm);
	                      _mm512_storeu_ps(&v[i+16], zmm);
	                      _mm512_storeu_ps(&v[i+32], zmm);
	                      _mm512_storeu_ps(&v[i+48], zmm);
	                      _mm512_storeu_ps(&v[i+64], zmm);
	                      _mm512_storeu_ps(&v[i+80], zmm);
	                      _mm512_storeu_ps(&v[i+96], zmm);
	                      _mm512_storeu_ps(&v[i+112], zmm);
	                 }
#pragma loop_count min(1),avg(8),max(15)
                        for(; i != vlen; ++i) {
	                    v[i] = val;
                        }
			return;
		    }
#elif defined __GNUC__ && !defined __INTEL_COMPILER
                     if((reinterpret_cast<uintptr_t>(v) & 0x3F) != 0ULL) {

		           if(vlen <= MEMMOVE_1ELEM) {
                              return;
		           }
		           else if(vlen <= MEMMOVE_16ELEMS) {
                                 _mm512_storeu_ps(&v[0],zmm);
			         return;
		           }
		           else if(vlen <= MEMMOVE_32ELEMS) {
                                _mm512_storeu_ps(&v[0],zmm);
		 	        _mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			         return;
		           }
		           else if(vlen <= MEMMOVE_64ELEMS) {
                                _mm512_storeu_ps(&v[0],zmm);
		 	        _mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			        return;
		           }
		           else if(vlen <= MEMMOVE_128ELEMS) {
                                _mm512_storeu_ps(&v[0],zmm);
		 	        _mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[4*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[5*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[6*ZMM_LEN],zmm);
			        _mm512_storeu_ps(&v[7*ZMM_LEN],zmm);
			        return;
		           }
			   else if(vlen <= MEMMOVE_256ELEMS) {
                                 _mm512_storeu_ps(&v[0],zmm);
		         	 _mm512_storeu_ps(&v[1*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[2*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[3*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[4*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[5*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[6*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[7*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[8*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[9*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[10*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[11*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[12*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[13*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[14*ZMM_LEN],zmm);
			         _mm512_storeu_ps(&v[15*ZMM_LEN],zmm);
			         return;
		            }
			     else if(vlen > MEMMOVE_256ELEMS) {
                                  for(i = 0; i != ROUND_TO_SIXTEEN(vlen,16); i += 128) {
	                              _mm512_storeu_ps(&v[i+0],  zmm);
	                              _mm512_storeu_ps(&v[i+16], zmm);
	                              _mm512_storeu_ps(&v[i+32], zmm);
	                              _mm512_storeu_ps(&v[i+48], zmm);
	                              _mm512_storeu_ps(&v[i+64], zmm);
	                              _mm512_storeu_ps(&v[i+80], zmm);
	                              _mm512_storeu_ps(&v[i+96], zmm);
	                              _mm512_storeu_ps(&v[i+112], zmm);
	                          }
                                  for(; i != vlen; ++i) {
	                              v[i] = val;
                                  }
			          return;  
		    }
		     else {

		          if(vlen <= MEMMOVE_1ELEM) {
                              return;
		           }
		           else if(vlen <= MEMMOVE_16ELEMS) {
                                 _mm512_store_ps(&v[0],zmm);
			         return;
		           }
		           else if(vlen <= MEMMOVE_32ELEMS) {
                                _mm512_store_ps(&v[0],zmm);
		 	        _mm512_store_ps(&v[1*ZMM_LEN],zmm);
			         return;
		           }
		           else if(vlen <= MEMMOVE_64ELEMS) {
                                _mm512_store_ps(&v[0],zmm);
		 	        _mm512_store_ps(&v[1*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[2*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[3*ZMM_LEN],zmm);
			        return;
		           }
		           else if(vlen <= MEMMOVE_128ELEMS) {
                                _mm512_store_ps(&v[0],zmm);
		 	        _mm512_store_ps(&v[1*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[2*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[3*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[4*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[5*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[6*ZMM_LEN],zmm);
			        _mm512_store_ps(&v[7*ZMM_LEN],zmm);
			        return;
		           }
			   else if(vlen <= MEMMOVE_256ELEMS) {
                                 _mm512_store_ps(&v[0],zmm);
		         	 _mm512_store_ps(&v[1*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[2*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[3*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[4*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[5*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[6*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[7*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[8*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[9*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[10*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[11*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[12*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[13*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[14*ZMM_LEN],zmm);
			         _mm512_store_ps(&v[15*ZMM_LEN],zmm);
			         return;
		            }
			     else if(vlen > MEMMOVE_256ELEMS) {
			          v = (float*)__builtin_assume_aligned(v,64);
                                  for(i = 0; i != ROUND_TO_SIXTEEN(vlen,16); i += 128) {
	                              _mm512_store_ps(&v[i+0],  zmm);
	                              _mm512_store_ps(&v[i+16], zmm);
	                              _mm512_store_ps(&v[i+32], zmm);
	                              _mm512_store_ps(&v[i+48], zmm);
	                              _mm512_store_ps(&v[i+64], zmm);
	                              _mm512_store_ps(&v[i+80], zmm);
	                              _mm512_store_ps(&v[i+96], zmm);
	                              _mm512_store_ps(&v[i+112], zmm);
	                          }
                                  for(; i != vlen; ++i) {
	                              v[i] = val;
                                  }
			          return;  
		         }
#endif
	      }


	        void ymm8r4_init_unroll8x(float * __restrict __attribute__((aligned(32))) v,
			                  const int32_t vlen,
			                  const float val) {

                        __m256 ymm = _mm256_set1_ps(val);
#if defined __ICC || defined __INTEL_COMPILER
                 if(vlen <= MEMMOVE_1ELEM) {
                    return;
		 }
		 else if(vlen <= MEMMOVE_16ELEMS) {
                      _mm256_storeu_ps(&v[0],ymm);
		      _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		      return;
		 }
		 else if(vlen <= MEMMOVE_32ELEMS) {
                      _mm256_storeu_ps(&v[0],ymm);
		      _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		      _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		      return;
		 }
		 else if(vlen <= MEMMOVE_64ELEMS) {
                      _mm256_storeu_ps(&v[0],ymm);
		      _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		      _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[4*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[5*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[6*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[7*YMM_LEN],ymm);
		      return;
		 }
		 else if(vlen <= MEMMOVE_128ELEMS) {
                      _mm256_storeu_ps(&v[0],ymm);
		      _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		      _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[4*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[5*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[6*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[7*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[8*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[9*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[10*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[11*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[12*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[13*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[14*YMM_LEN],ymm);
		      _mm256_storeu_ps(&v[15*YMM_LEN],ymm);
		      return; 
		 }
		 else if(vlen > MEMMOVE_128ELEMS) {
                      int32_t i;
#pragma code_align(32)
                      for(i = 0; i != ROUND_TO_EIGHT(vlen,8); i += 64) {
	                    _mm256_storeu_ps(&v[i+0],  ymm0);
	                    _mm256_storeu_ps(&v[i+8],  ymm0);
	                    _mm256_storeu_ps(&v[i+16], ymm0);
	                    _mm256_storeu_ps(&v[i+24], ymm0);
	                    _mm256_storeu_ps(&v[i+32], ymm0);
	                    _mm256_storeu_ps(&v[i+40], ymm0);
	                    _mm256_storeu_ps(&v[i+48], ymm0);
	                    _mm256_storeu_ps(&v[i+56], ymm0);
	              }
#pragma loop_count min(1),avg(4),max(7)
                        for(; i != vlen; ++i) {
	                    v[i] = val;
                           }
		 }
#elif defined __GNUC__ && !defined __INTEL_COMPILER
                  if((reinterpret_cast<uintptr_t>(v) & 0x1F) != 0ULL) {

		      if(vlen <= MEMMOVE_1ELEM) {
                         return;
		      }
		      else if(vlen <= MEMMOVE_16ELEMS) {
                           _mm256_storeu_ps(&v[0],ymm);
		           _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		         return;
		      }
		      else if(vlen <= MEMMOVE_32ELEMS) {
                           _mm256_storeu_ps(&v[0],ymm);
		           _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		           _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		           _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		           return;
		      }
		      else if(vlen <= MEMMOVE_64ELEMS) {
                            _mm256_storeu_ps(&v[0],ymm);
		            _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		            _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[4*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[5*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[6*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[7*YMM_LEN],ymm);
		            return;
		      }
		      else if(vlen <= MEMMOVE_128ELEMS) {
                            _mm256_storeu_ps(&v[0],ymm);
		            _mm256_storeu_ps(&v[1*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[2*YMM_LEN],ymm)
		            _mm256_storeu_ps(&v[3*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[4*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[5*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[6*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[7*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[8*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[9*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[10*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[11*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[12*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[13*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[14*YMM_LEN],ymm);
		            _mm256_storeu_ps(&v[15*YMM_LEN],ymm);
		            return; 
		 }
		 else if(vlen > MEMMOVE_128ELEMS) {
                      int32_t i;
                        for(i = 0; i != ROUND_TO_EIGHT(vlen,8); i += 64) {
	                    _mm256_storeu_ps(&v[i+0],  ymm0);
	                    _mm256_storeu_ps(&v[i+8],  ymm0);
	                    _mm256_storeu_ps(&v[i+16], ymm0);
	                    _mm256_storeu_ps(&v[i+24], ymm0);
	                    _mm256_storeu_ps(&v[i+32], ymm0);
	                    _mm256_storeu_ps(&v[i+40], ymm0);
	                    _mm256_storeu_ps(&v[i+48], ymm0);
	                    _mm256_storeu_ps(&v[i+56], ymm0);
	                }
                        for(; i != vlen; ++i) {
	                    v[i] = val;
                           }
		  }else {

		         if(vlen <= MEMMOVE_1ELEM) {
                               return;
		         }
		         else if(vlen <= MEMMOVE_16ELEMS) {
                                 _mm256_store_ps(&v[0],ymm);
		                 _mm256_store_ps(&v[1*YMM_LEN],ymm);
		                 return;
		        }
		        else if(vlen <= MEMMOVE_32ELEMS) {
                                _mm256_store_ps(&v[0],ymm);
		                _mm256_store_ps(&v[1*YMM_LEN],ymm);
		                _mm256_store_ps(&v[2*YMM_LEN],ymm)
		                _mm256_store_ps(&v[3*YMM_LEN],ymm);
		                return;
		         }
		         else if(vlen <= MEMMOVE_64ELEMS) {
                                _mm256_store_ps(&v[0],ymm);
		                _mm256_store_ps(&v[1*YMM_LEN],ymm);
		                _mm256_store_ps(&v[2*YMM_LEN],ymm)
		                _mm256_store_ps(&v[3*YMM_LEN],ymm);
		                _mm256_store_ps(&v[4*YMM_LEN],ymm);
		                _mm256_store_ps(&v[5*YMM_LEN],ymm);
		                _mm256_store_ps(&v[6*YMM_LEN],ymm);
		                _mm256_store_ps(&v[7*YMM_LEN],ymm);
		                return;
		        }
		        else if(vlen <= MEMMOVE_128ELEMS) {
                                _mm256_store_ps(&v[0],ymm);
		                _mm256_store_ps(&v[1*YMM_LEN],ymm);
		                _mm256_store_ps(&v[2*YMM_LEN],ymm)
		                _mm256_store_ps(&v[3*YMM_LEN],ymm);
		                _mm256_store_ps(&v[4*YMM_LEN],ymm);
		                _mm256_store_ps(&v[5*YMM_LEN],ymm);
		                _mm256_store_ps(&v[6*YMM_LEN],ymm);
		                _mm256_store_ps(&v[7*YMM_LEN],ymm);
		                _mm256_store_ps(&v[8*YMM_LEN],ymm);
		                _mm256_store_ps(&v[9*YMM_LEN],ymm);
		                _mm256_store_ps(&v[10*YMM_LEN],ymm);
		                _mm256_store_ps(&v[11*YMM_LEN],ymm);
		                _mm256_store_ps(&v[12*YMM_LEN],ymm);
		                _mm256_store_ps(&v[13*YMM_LEN],ymm);
		                _mm256_store_ps(&v[14*YMM_LEN],ymm);
		                _mm256_store_ps(&v[15*YMM_LEN],ymm);
		                return; 
		       }
		       else if(vlen > MEMMOVE_128ELEMS) {
		              v = (float*)__builtin_assume_aligned(v,32);
                              int32_t i;
                              for(i = 0; i != ROUND_TO_EIGHT(vlen,8); i += 64) {
	                          _mm256_store_ps(&v[i+0],  ymm0);
	                          _mm256_store_ps(&v[i+8],  ymm0);
	                          _mm256_store_ps(&v[i+16], ymm0);
	                          _mm256_store_ps(&v[i+24], ymm0);
	                          _mm256_store_ps(&v[i+32], ymm0);
	                          _mm256_store_ps(&v[i+40], ymm0);
	                          _mm256_store_ps(&v[i+48], ymm0);
	                          _mm256_store_ps(&v[i+56], ymm0);
	                      }
                              for(; i != vlen; ++i) {
	                          v[i] = val;
                              }
		       }
#endif
		}
