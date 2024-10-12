

#include "GMS_simd_ops.h"



v8f32 
masked_2x32_to_8x32_v8f32_eq( v8f32 a,
                           v8f32 b)                          
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_EQ_OQ);
    ymm0 = _mm256_mask_broadcast_f32x2(*(__m256*)&a, mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}

v8f32 
masked_zero_2x32_to_8x32_v8f32_eq( v8f32 a,
                                v8f32 b)
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_EQ_OQ);
    ymm0 = _mm256_maskz_broadcast_f32x2( mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}


#if defined(__AVX512DQ__)
v16f32 
masked_2x32_to_16x32_v16f32_eq( v16f32 a,
                             v16f32 b)
{                                      
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_EQ_OQ;
    zmm0 = _mm512_mask_broadcast_f32x2(*(__m512*)&a, mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}

v16f32 
masked_zero_2x32_to_16x32_v16f32_eq( v16f32 a,
                                  v16f32 b)
{
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_EQ_OQ);
    zmm0 = _mm512_maskz_broadcast_f32x2(mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}
#endif

v8f64
masked_4x64_8x64_v8f64_eq(v8f64 a,
                          v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_EQ_OQ);
     zmm0 = _mm512_mask_broadcast_f64x4(*(__m512d*)&a,mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}  

v8f64
masked_zero_4x64_8x64_v8f64_eq(v8f64 a,
                               v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_EQ_OQ);
     zmm0 = _mm512_maskz_broadcast_f64x4(mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}     


/////////////////////////////////////////////////////////////////////////////////////             
                       
v8f32 
masked_2x32_to_8x32_v8f32_eq( v8f32 a,
                           v8f32 b)                          
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_NEQ_OQ);
    ymm0 = _mm256_mask_broadcast_f32x2(*(__m256*)&a, mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}

v8f32 
masked_zero_2x32_to_8x32_v8f32_eq( v8f32 a,
                                v8f32 b)
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_NEQ_OQ);
    ymm0 = _mm256_maskz_broadcast_f32x2( mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}


#if defined(__AVX512DQ__)
v16f32 
masked_2x32_to_16x32_v16f32_eq( v16f32 a,
                             v16f32 b)
{                                      
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_NEQ_OQ;
    zmm0 = _mm512_mask_broadcast_f32x2(*(__m512*)&a, mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}

v16f32 
masked_zero_2x32_to_16x32_v16f32_eq( v16f32 a,
                                  v16f32 b)
{
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_NEQ_OQ);
    zmm0 = _mm512_maskz_broadcast_f32x2(mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}
#endif

v8f64
masked_4x64_8x64_v8f64_eq(v8f64 a,
                          v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_NEQ_OQ);
     zmm0 = _mm512_mask_broadcast_f64x4(*(__m512d*)&a,mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}  

v8f64
masked_zero_4x64_8x64_v8f64_eq(v8f64 a,
                               v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_NEQ_OQ);
     zmm0 = _mm512_maskz_broadcast_f64x4(mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}     
                       
                       
/////////////////////////////////////////////////////////////////////////////////////


v8f32 
masked_2x32_to_8x32_v8f32_lt( v8f32 a,
                           v8f32 b)                          
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_LT_OQ);
    ymm0 = _mm256_mask_broadcast_f32x2(*(__m256*)&a, mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}

v8f32 
masked_zero_2x32_to_8x32_v8f32_lt( v8f32 a,
                                v8f32 b)
{
    __m256 ymm0;
    __mmask8 mask;
    mask = _mm256_cmp_ps_mask(*(__m256*)&a,*(__m256*)&b,_CMP_LT_OQ);
    ymm0 = _mm256_maskz_broadcast_f32x2( mask,
                                   _mm256_castps256_ps128(*(__m256*)&b));
    return (*(__m256*)&ymm0);
}


#if defined(__AVX512DQ__)
v16f32 
masked_2x32_to_16x32_v16f32_lt( v16f32 a,
                             v16f32 b)
{                                      
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_LT_OQ;
    zmm0 = _mm512_mask_broadcast_f32x2(*(__m512*)&a, mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}

v16f32 
masked_zero_2x32_to_16x32_v16f32_lt( v16f32 a,
                                  v16f32 b)
{
    __m512 zmm0;
    __mmask16 mask;
    mask = _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,_CMP_LT_OQ);
    zmm0 = _mm512_maskz_broadcast_f32x2(mask,
                                   _mm512_castps512_ps128(*(__m512*)&b));
    return (*(__m512*)&zmm0);
}
#endif

v8f64
masked_4x64_8x64_v8f64_lt(v8f64 a,
                          v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_LT_OQ);
     zmm0 = _mm512_mask_broadcast_f64x4(*(__m512d*)&a,mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}  

v8f64
masked_zero_4x64_8x64_v8f64_lt(v8f64 a,
                               v8f64 b)
{                      
     __m512d zmm0;
     __mmask8 mask;
     mask = _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,_CMP_LT_OQ);
     zmm0 = _mm512_maskz_broadcast_f64x4(mask,
                                    _mm512_castpd512_pd256(*(__m512d*)&b));
     return (*(__m512d*)&zmm0);                      
}     
                                              
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       

