
#ifndef __GMS_SIMD_OPS_H__
#define __GMS_SIMD_OPS_H__

#include <immintrin.h>



    // Interoperable with corresponding Fortran derived type

        typedef struct __attribute__((align(64))) v8f64 {
                 double v[8];
        }v8f64;

        typedef struct __attribute__((align(64))) v16f32 {
                 float v[16];
	}v16f32;
	
	typedef struct __attribute__((align(32))) v4f64 {
                 double v[4];
        }v4f64;
        
        typedef struct __attribute__((align(32))) v8f32 {
                 float v[8];
	}v8f32;

        typedef struct __attribute__((align(16))) v2f64 {
                 double v[2];
        }v2f64;
        
        typedef struct __attribute__((align(16))) v4f32 {
                 float v[4];
	}v4f32;
	
///////////////////////////////////////////////////////////////////////////////////
	
	v8f32 
	masked_2x32_to_8x32_v8f32_eq(v8f32,
	                             v8f32)	                             
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));
				  
        v8f32 
	masked_zero_2x32_to_8x32_v8f32_eq(v8f32,
	                                  v8f32)	                                  
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));

#if defined(__AVX512DQ__)				          
	v16f32
	masked_2x32_to_16x32_v16f32_eq(v16f32,
	                               v16f32)	                             
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
       v16f32
       masked_zero_2x32_to_16x32_v16f32_eq(v16f32,
                                           v16f32)
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));				  		       
#endif	

        			          
       v8f64
       masked_4x64_to_8x64_v8f64_eq(v8f64,
                                    v8f64)
                                   __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
      			          
       v8f64
       masked_zero_4x64_to_8x64_v8f64_eq(v8f64,
                                      v8f64)
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));			  

/////////////////////////////////////////////////////////////////////////////////////


       v8f32 
       masked_2x32_to_8x32_v8f32_neq(v8f32,
	                             v8f32)	                             
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));
				  
        v8f32 
	masked_zero_2x32_to_8x32_v8f32_neq(v8f32,
	                                  v8f32)	                                  
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));

#if defined(__AVX512DQ__)				          
	v16f32
	masked_2x32_to_16x32_v16f32_neq(v16f32,
	                               v16f32)	                             
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
       v16f32
       masked_zero_2x32_to_16x32_v16f32_neq(v16f32,
                                           v16f32)
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));				  		       
#endif	

        			          
       v8f64
       masked_4x64_to_8x64_v8f64_neq(v8f64,
                                    v8f64)
                                   __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
      			          
       v8f64
       masked_zero_4x64_to_8x64_v8f64_neq(v8f64,
                                          v8f64)
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));		


///////////////////////////////////////////////////////////////////////////////////////

  
       v8f32 
       masked_2x32_to_8x32_v8f32_lt(v8f32,
	                             v8f32)	                             
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));
				  
        v8f32 
	masked_zero_2x32_to_8x32_v8f32_lt(v8f32,
	                                  v8f32)	                                  
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));

#if defined(__AVX512DQ__)				          
	v16f32
	masked_2x32_to_16x32_v16f32_lt(v16f32,
	                               v16f32)	                             
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
       v16f32
       masked_zero_2x32_to_16x32_v16f32_lt(v16f32,
                                           v16f32)
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));				  		       
#endif	

        			          
       v8f64
       masked_4x64_to_8x64_v8f64_lt(v8f64,
                                    v8f64)
                                   __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
      			          
       v8f64
       masked_zero_4x64_to_8x64_v8f64_lt(v8f64,
                                          v8f64)
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	



//////////////////////////////////////////////////////////////////////////////////


       v8f32 
       masked_2x32_to_8x32_v8f32_gt(v8f32,
	                             v8f32)	                             
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));
				  
        v8f32 
	masked_zero_2x32_to_8x32_v8f32_gt(v8f32,
	                                  v8f32)	                                  
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));

#if defined(__AVX512DQ__)				          
	v16f32
	masked_2x32_to_16x32_v16f32_gt(v16f32,
	                               v16f32)	                             
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
       v16f32
       masked_zero_2x32_to_16x32_v16f32_gt(v16f32,
                                           v16f32)
	                          __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));				  		       
#endif	

        			          
       v8f64
       masked_4x64_to_8x64_v8f64_gt(v8f64,
                                    v8f64)
                                   __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	
				  
      			          
       v8f64
       masked_zero_4x64_to_8x64_v8f64_gt(v8f64,
                                          v8f64)
                                  __attribute__((hot))
				  __attribute__((regcall))
				  __attribute__((aligned(32)));	












#endif /*__GMS_SIMD_OPS_H__*/
