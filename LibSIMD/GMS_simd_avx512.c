

 #include <immintrin.h>
 #include "GMS_simd_avx512.h"



     v8f64 v8f64_add_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_add_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_add_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_add_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_sub_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_sub_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_sub_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_sub_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mul_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mul_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mul_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mul_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_div_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_div_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_div_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_div_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_ceil_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_ceil_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_ceil_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_ceil_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_ceil_pd(v8f64 a) {
        __m512d zmm = _mm512_ceil_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_ceil_ps(v16f32 a) {
        __m512 zmm = _mm512_ceil_ps(*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_abs_pd(v8f64 a) {
        __m512d zmm = _mm512_abs_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_abs_ps(v16f32 a) {
        __m512 zmm = _mm512_abs_ps(*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_abs_pd(v8f64 a, __mmask8 k, v8f64 b) {
        __m512d zmm = _mm512_mask_abs_pd(*(__m512d*)&a,k,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_abs_ps(v16f32 a, __mmask16 k, v16f32 b) {
        __m512 zmm = _mm512_mask_abs_ps(*(__m512*)&a,k,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_add_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_add_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_add_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_add_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_add_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_add_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_add_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_add_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_add_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_add_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_add_round_ps(v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_add_round_ps(*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_add_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_add_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_add_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_mask_add_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_add_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_add_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_add_round_ps(__mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_maskz_add_round_ps(k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_div_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_div_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_div_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_div_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_div_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_div_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_div_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_div_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_div_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_div_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_div_round_ps(v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_div_round_ps(*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_div_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_div_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_div_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_mask_div_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_div_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_div_round_pd(k,*(__m512d*)&,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_div_round_ps(__mmask16 k, v16f32 a, v16f32 b, rounding) {
        __m512 zmm = _mm512_maskz_div_round_ps(k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmadd_ps(v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_fmadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmadd_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fmadd_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmadd_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fmadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmadd_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmadd_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_maskz_fmadd_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmadd_round_ps(v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_fmadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmadd_round_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_mask_fmadd_round_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmadd_round_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k, int rounding){
        __m512 zmm = _mm512_mask3_fmadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k,rounding);
	return (*(v16f32*)&zmm);
     }
     

     v8f64 v8f64_maskz_fmadd_round_pd(_mmask8 k,v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmadd_round_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_maskz_fmadd_round_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmaddsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmaddsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmaddsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmaddsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmaddsub_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fmaddsub_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmaddsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmaddsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmaddsub_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fmaddsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmaddsub_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmaddsub_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmaddsub_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_maskz_fmaddsub_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmaddsub_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmaddsub_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmaddsub_round_ps(v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_fmaddsub_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmaddsub_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmaddsub_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmaddsub_round_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_mask_fmaddsub_round_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmaddsub_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __mm512d zmm = _mm512_mask3_fmaddsub_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmaddsub_round_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k, int rounding) {
        __m512 zmm = _mm512_mask3_fmaddsub_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmaddsub_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmaddsub_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmaddsub_round_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_maskz_fmaddsub_round_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmsub_ps(v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_fmsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmsub_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fmsub_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmsub_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fmsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_maskz_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmsub_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_maskz_fmsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmsubadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmsubadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmsubadd_ps(v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_fmsubadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmsubadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmsubadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmsubadd_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fmsubadd_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }


     v8f64 v8f64_mask3_fmsubadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmsubadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmsubadd_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fmsubadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmsubadd_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmsubadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmsubadd_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_maskz_fmsubadd_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fmsubadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmsubadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fmsubadd_round_ps(v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_fmsubadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fmsubadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmsubadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fmsubadd_round_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_mask_fmsubadd_round_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fmsubadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fmsubadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fmsubadd_round_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k, int rounding) {
        __m512 zmm = _mm512_mask3_fmsubadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fmusbadd_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmsubadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fmsubadd_round_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_maskz_fmsubadd_round_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fnmadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fnmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fnmadd_ps(v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_fnmadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fnmadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fnmadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fnmadd_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fnmadd_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fnmadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fnmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fnmadd_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fnmadd_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fnmadd_pd(__mmask8 k,v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fnmadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fnmadd_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_maskz_fnmadd_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fnmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fnmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fnmadd_round_ps(v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_fnmadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fnmadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fnmadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fnmadd_round_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_mask_fnmadd_round_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fnmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fnmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fnmadd_round_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k, int rounding) {
        __m512 zmm = _mm512_mask3_fnmadd_round_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_fnmadd_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fnmadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fnmadd_round_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c, int rounding) {
        __m512 zmm = _mm512_maskz_fnmadd_round_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_fnmsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fnmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_fnmsub_ps(v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_fnmsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_fnmsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fnmsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_fnmsub_ps(v16f32 a, __mmask16 k, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_mask_fnmsub_ps(*(__m512*)&a,k,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask3_fnmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fnmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask3_fnmsub_ps(v16f32 a, v16f32 b, v16f32 c, __mmask16 k) {
        __m512 zmm = _mm512_mask3_fnmsub_ps(*(__m512*)&a,*(__m512*)&b,*(__m512*)&c,k);
	return (*(v16f32*)&zmm);
	
     }

     v8f64 v8f64_maskz_fnmsub_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fnmsub_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_fnmsub_ps(__mmask16 k, v16f32 a, v16f32 b, v16f32 c) {
        __m512 zmm = _mm512_maskz_fnmsub_ps(k,*(__m512*)&a,*(__m512*)&b,*(__m512*)&c);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_mul_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_mul_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_mul_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_mul_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_mul_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_mul_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_mul_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_mul_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_mul_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_mul_round_pd(*(__m512d)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding)
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_mul_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_mask_mul_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_mul_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_mul_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_mul_round_ps(__mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_maskz_mul_round_ps(k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     double v8f64_mask_reduce_add_pd(__mmask8 k, v8f64 a) {
      return(  _mm512_mask_reduce_add_pd(k,*(__m512d*)&a));

     }

     float v16f32_mask_reduce_add_ps(__mmask16 k, v16f32 a) {
      return ( _mm512_mask_reduce_add_ps(k,*(__m512*)&a));
     }

     double v8f64_reduce_add_pd(v8f64 a) {
       return (  _mm512_reduce_add_pd(*(__m512d*)&a));

     }

     float v16f32_reduce_add_ps(v16f32 a) {
       return ( _mm512_reduce_add_ps(*(__m512*)&a));
     }

     double  v8f64_mask_reduce_mul_pd(__mmask8 k, v8f64 a) {
       return ( _mm512_mask_reduce_mul_pd(k,*(__m512d*)&a));

     }

     float  v16f32_mask_reduce_mul_ps(__mmask16 k, v16f32 a) {
       return ( _mm512_mask_reduce_mul_ps(k,*(__m512*)&a));
     }

     double  v8f64_reduce_mul_pd(v8f64 a) {
       return ( _mm512_reduce_mul_pd(*(__m512d*)&a));

     }

     float  v16f32_reduce_mul_ps(v16f32 a) {
       return ( _mm512_reduce_mul_ps(*(__m512*)&a));
     }

     v8f64 v8f64_mask_sub_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_sub_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_sub_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_sub_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_sub_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_sub_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_sub_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_sub_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     __mmask8  v8f64_cmp_pd_mask(v8f64 a, v8f64 b, int imm8) {
        return ( _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,imm8));
     }

     __mmask16 v16f32_cmp_ps_mask(v16f32 a, v16f32 b, int imm8) {
        return ( _mm512_cmp_ps_mask(*(__m512*)&a,*(__m512*)&b,imm8));
     }

     __mmask8 v8f64_mask_cmp_pd_mask(__mmask8 k1, v8f64 a, v8f64 b, int imm8) {
        return ( _mm512_mask_cmp_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b,imm8));
     }

     __mmask16 v16f32_mask_cmp_ps_mask(__mmask16 k1, v16f32 a, v16f32 b, int imm8) {
        return ( _mm512_mask_cmp_ps(k1,*(__m512*)&a,*(__m512*)&b,imm8));
     }

     __mmask8 v8f64_cmp_round_pd_mask(v8f64 a, v8f64 b, int imm8, int sae) {
        return ( _mm512_cmp_round_pd_mask(*(__m512d*)&a,*(__m512d*)&b,imm8,sae));
     }

     __mmask16 v16f32_cmp_round_ps_mask(v16f32 a, v16f32 b, int imm8, int sae) {
        return ( _mm512_cmp_round_ps_mask(*(__m512*)&a,*(__m512*)&b,imm8,sae));
     }

     __mmask8 v8f64_mask_cmp_round_pd_mask(__mmask8 k1, v8f64 a, v8f64 b, int imm8, int sae) {
        return ( _mm512_mask_cmp_round_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b,imm8,sae));
     }

     __mmask16 v16f32_mask_cmp_round_ps_mask(__mmask16 k1, v16f32 a, v16f32 b, int imm8, int sae) {
        return ( _mm512_mask_cmp_round_ps_mask(k1,*(__m512*)&a,*(__m512*)&b,imm8,sae));
     }

     __mmask8 v8f64_cmpeq_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpeq_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmpeq_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmpeq_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmpeq_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpeq_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmpeq_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpeq_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }
     

     __mmask8 v8f64_mask_cmpunord_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpunord_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmpunord_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpunord_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmpunord_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpunord_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmpunord_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmpunord_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmpord_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpord_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmpord_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmpord_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmpord_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpord_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmpord_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpord_ps_mask(k1, *(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmpnlt_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpnlt_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmpnlt_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmpnlt_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmpnlt_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpnlt_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&a));
     }

     __mmask16 v16f32_mask_cmpnlt_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpnlt_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmpnle_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpnle_pd_mask(*(__m512d*)&a,*(__m512d*)&));
     }

     __mmask16 v16f32_cmpnle_ps_mask(v16f32 a, v16f32 a) {
        return ( _mm512_cmpnle_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmpnle_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpnle_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmpnle_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpnle_ps_mask(k1, *(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmpneq_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpneq_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmpneq_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmpneq_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmpneq_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpneq_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmpneq_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmpneq_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmplt_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmplt_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmplt_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmplt_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmplt_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmplt_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmplt_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmplt_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_cmple_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmple_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_cmple_ps_mask(v16f32 a, v16f32 b) {
        return ( _mm512_cmple_ps_mask(*(__m512*)&a,*(__m512*)&b));
     }

     __mmask8 v8f64_mask_cmple_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmple_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask16 v16f32_mask_cmple_ps_mask(__mmask16 k1, v16f32 a, v16f32 b) {
        return ( _mm512_mask_cmple_ps_mask(k1,*(__m512*)&a,*(__m512*)&b));
     }

     v8f64 v8f64_mask_rcp14_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_rcp14_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_rcp14_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_rcp14_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_rcp14_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_rcp14_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_rcp14_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_rcp14_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_rcp14_pd(v8f64 a) {
        __m512d zmm = _mm512_rcp14_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_rcp14_ps(v16f32 a) {
        __m512 zmm = _mm512_rcp14_ps(*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_rsqrt14_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_rsqrt14_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_rsqrt14_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_rsqrt14_ps(*(__m512*)&a,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_rsqrt14_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_rsqrt14_pd
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_rsqrt14_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_rsqrt14_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_sqrt_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_sqrt_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_sqrt_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_sqrt_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_sqrt_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_sqrt_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_sqrt_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_sqrt_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_sqrt_round_pd(v8f64 src, __mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_mask_sqrt_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_sqrt_round_ps(v16f32 src, __mmask16 k, v16f32 a, int rounding) {
        __m512 zmm = _mm512_mask_sqrt_round_ps(*(__m512*)&src,k,*(__m512*)&a,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_sqrt_round_pd(__mmask8 k, v8f64 src, int rounding) {
        __m512d zmm = _mm512_maskz_sqrt_round_pd(k,*(__m512d*)&src,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_sqrt_round_ps(__mmask16 k, v16f32 src, int rounding) {
        __m512 zmm = _mm512_maskz_sqrt_round_ps(k,*(__m512*)&src,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_expandloadu_pd(v8f64 src, __mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_mask_expandloadu_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_expandloadu_ps(v16f32 src, __mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_mask_expandloadu_ps(*(__m512*)&src,k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_expandloadu_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_expandloadu_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_expandloadu_ps(__mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_maskz_expandloadu_ps(k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_load_pd(v8f64 src, __mmask8 k, void * mem_add) {
        __m512d zmm = _mm512_mask_load_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_load_ps(v16f32 src, __mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_mask_load_ps(*(__m512*)&src,k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_load_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_load_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_load_ps(__mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_maskz_load_ps(k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_loadu_pd(v8f64 src, __mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_mask_loadu_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_loadu_ps(v16f32 src, __mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_mask_loadu_ps(*(__m512*)&src,k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_loadu_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_loadu_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_loadu_ps(__mmask16 k, void * mem_addr) {
        __m512 zmm = _mm512_maskz_loadu_ps(k,mem_addr);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_getexp_pd(v8f64 a) {
        __m512d zmm = _mm512_getexp_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_getexp_ps(v16f32 a) {
        __m512 zmm = _mm512_getexp_ps(*(__m512*)&a);
	return(*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_getexp_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_getexp_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_getexp_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_getexp_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_getexp_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512maskz_getexp_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_getexp_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_getexp_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_getexp_round_pd(v8f64 a, int rounding) {
        __m512d zmm = _mm512_getexp_round_pd(*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_getexp_round_ps(v16f32 a, int rounding) {
        __m512 zmm = _mm512_getexp_round_ps(*(__m512*)&a,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_getexp_round_pd(v8f64 src, __mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_mask_getexp_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_getexp_round_ps(v16f32 src, __mmask16 k, v16f32 a, int rounding){
        __m512 zmm = _mm512_mask_getexp_round_ps(*(__m512*)&src,k,*(__m512*)&a,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_getexp_round_pd(__mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_maskz_getexp_round_pd(k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_getexp_round_ps(__mmask16 k, v16f32 a, int rounding) {
        __m512 zmm = _mm512_maskz_getexp_round_ps(k,*(__m512*)&a,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_getmant_pd(v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_getmant_pd(*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_getmant_ps(v16f32 a, int interv, int sc) {
        __m512 zmm = _mm512_getmant_ps(*(__m512*)&a,interv,sc);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_gemant_pd(v8f64 src, __mmask8 k, v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_mask_getmant_pd(*(__m512d*)&src,k,*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_getmant_ps(v16f32 src, __mmask16 k, v16f32 a, int interv, int sc) {
        __m512 zmm = _mm512_mask_getmant_ps(*(__m512*)&src,k,*(__m512*)&a,interv,sc);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_getmant_pd(__mmask8 k, v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_maskz_getmant_pd(k,*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_getmant_ps(__mmask16 k, v16f32 a, int interv, int sc) {
        __m512 zmm = _mm512_maskz_getmant_ps(k,*(__m512*)&a,interv,sc);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_getmant_round_pd(v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_getmant_round_pd(*(__m512d*)&a,interv,sc,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_getmant_round_ps(v16f32 a, int interv, int sc, int rounding) {
        __m512 zmm = _mm512_getmant_round_ps(*(__m512*)&a,interv,sc,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_getmant_round_pd(v8f64 src, __mmask8 k, v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_mask_getmant_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,sc,interv,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_getmant_round_ps(v16f32 src, __mmask16 k, v16f32 a, int interv, int sc, int rounding) {
        __m512 zmm = _mm512_mask_getmant_round_ps(*(__m512*)&src,k,*(__m512*)&a,interv,sc,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_getmant_round_pd(__mmask8 k, v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_maskz_getmant_round_pd(k,*(__m512d*)&a,interv,sc,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_getmant_round_ps(__mmask16 k, v16f32 a, int interv, int sc, int rounding) {
        __m512 zmm = _mm512_maskz_getmant_round_ps(k,*(__m512*)&a,interv,sc,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_roundscale_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_mask_roundscale_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8) 
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_roundscale_ps(v16f32 src, __mmask16 k, v16f32 a, int imm8) {
        __m512 zmm = _mm512_mask_roundscale_ps(*(__m512*)&src,k,*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_roundscale_pd(__mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_maskz_roundscale_pd(k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_roundscale_ps(__mmask8 k, v16f32 a, int imm8) {
        __m512 zmm = _mm512_maskz_roundscale_ps(k,*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_roundscale_pd(v8f64 a, int imm8) {
        __m512d zmm = _mm512_roundscale_pd(*(__512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_roundscale_ps(v16f32 a, int imm8) {
        __m512 zmm = _mm512_roundscale_ps(*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_roundscale_round_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_mask_roundscale_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_roundscale_round_ps(v16f32 src, __mmask16 k, v16f32 a, int imm8, int rounding) {
        __m512 zmm = _mm512_mask_roundscale_round_ps(*(__m512*)&src,k,*(__m512*)&a,imm8,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_roundscale_round_pd(__mask8 k, v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_maskz_roundscale_round_pd(k,*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_roundscale_round_ps(__mmask16 k, v16f32 a, int imm8, int rounding) {
        __m512 zmm = _mm512_maskz_roundscale_round_ps(k,*(__m512*)&a,imm8,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_roundscale_round_pd(v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_roundscale_round_pd(*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_roundscale_round_ps(v16f32 a, int imm8, int rounding) {
        __m512 zmm = _mm512_roundscale_round_ps(*(__m512*)&a,imm8,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_scalef_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_scalef_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_scalef_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_scalef_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_scalef_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_scalef_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_scalef_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_mask_scalef_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_scalef_round_pd(__mmask8 k ,v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_scalef_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_scalef_round_ps(__mmask16 k, v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_maskz_scalef_round_ps(k,*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_scalef_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_scalef_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_scalef_round_ps(v16f32 a, v16f32 b, int rounding) {
        __m512 zmm = _mm512_scalef_round_ps(*(__m512*)&a,*(__m512*)&b,rounding);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_mov_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_mov_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_mov_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_mov_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_mov_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_mov_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_mov_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_mov_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm;
     }

     v8f64 v8f64_mask_movedup_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_movedup_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_movedup_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_movedup_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_movedup_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_movedup_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_movedup_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_movedup_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_movedup_pd(v8f64 a) {
        __m512d zmm = _mm512_movedup_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_movedup_ps(v16f32 a) {
        __m512 zmm = _mm512_movedup_ps(*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_set4_pd(double a, double b, double c, double d) {
        __m512d zmm = _mm512_set4_pd(a,b,c,d);
	return (*(v8f64*)&zmm);
     }

     

     v8f64 v8f64_mask_max_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_max_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_max_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_max_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_max_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_max_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_max_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_max_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_max_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_max_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_max_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_max_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_max_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_mask_max_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_max_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_mask_max_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_max_round_pd(__mmask8 k, v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_maskz_max_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_max_round_ps(__mmask16 k, v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_maskz_max_round_ps(k,*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_max_round_pd(v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_max_round_pd(*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_max_round_ps(v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_max_round_ps(*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_min_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_min_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_min_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_min_ps(*(__m512)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_min_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_min_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_min_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_min_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_min_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_min_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_min_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_min_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_min_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_mask_min_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_min_round_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_mask_min_round_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_min_round_pd(__mmask8 k, v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_maskz_min_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_min_round_ps(__mmask16 k, v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_maskz_min_round_ps(k,*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_min_round_pd(v8f64 a, v8f64 b, int sae) {
        __m512d zmm = _mm512_min_round_pd(*(__m512d*)&a,*(__m512d*)&b,sae);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_min_round_ps(v16f32 a, v16f32 b, int sae) {
        __m512 zmm = _mm512_min_round_ps(*(__m512*)&a,*(__m512*)&b,sae);
	return (*(v16f32*)&zmm);
     }

     double  v8f64_mask_reduce_max_pd(__mmask8 k, v8f64 a) {
        return(  _mm512_mask_reduce_max_pd(k,*(__m512d*)&a));

     }

     float v16f32_mask_reduce_max_pd(__mmask16 k, v16f32 a) {
        return ( _mm512_mask_reduce_max_ps(k,*(__m512*)&a));
     }

     double  v8f64_reduce_max_pd(v8f64 a) {
        return ( _mm512_reduce_max_pd(*(__m512d*)&a));
	
     }

     float v16f32_reduce_max_ps(v16f32 a) {
        return ( _mm512_reduce_max_ps(*(__m512*)&a));
     }

     double  v8f64_mask_reduce_min_pd(__mmask8 k, v8f64 a) {
        return (  _mm512_mask_reduce_min_pd(k,*(__m512d*)&a));

     }

     float v16f32_mask_reduce_min_ps(__mmask16 k, v16f32 a) {
        return ( _mm512_mask_reduce_min_ps(k,*(__m512*)&a));
     }

     double  v8f64_reduce_min_pd(v8f64 a) {
        return ( _mm512_reduce_min_pd(*(__m512d*)&a));
	
     }

     float v16f32_reduce_min_ps(v16f32 a) {
        return ( _mm512_reduce_min_ps(*(__m512*)&a));
     }

     v8f64 v8f64_mask_unpacklo_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_unpacklo_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_unpacklo_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_unpacklo_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_unpacklo_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_unpacklo_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_unpacklo_pd(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_unpacklo_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_unpacklo_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_unpacklo_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_unpacklo_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_unpacklo_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_unpackhi_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_unpackhi_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_unpackhi_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_unpackhi_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_unpackhi_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_unpackhi_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_unpackhi_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_maskz_unpackhi_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_unpackhi_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_unpackhi_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_unpackhi_ps(v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_unpackhi_ps(*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_shuffle_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_mask_shuffle_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_shuffle_ps(v16f32 src, __mmask16 k, v16f32 a, v16f32 b, int imm8) {
        __m512 zmm = _mm512_mask_shuffle_ps(*(__m512*)&src,k,*(__m512*)&a,*(__m512*)&b,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_shuffle_pd(__mmask8 k, v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_maskz_shuffle_pd(k,*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_shuffle_ps(__mmask16 k, v16f32 a, v16f32 b, int imm8) {
        __m512 zmm = _mm512_maskz_shuffle_ps(k,*(__m512*)&a,*(__m512*)&b,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_shuffle_pd(v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_shuffle_pd(*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_shuffle_ps(v16f32 a, v16f32 b, int imm8) {
        __m512 zmm = _mm512_shuffle_ps(*(__m512*)&a,*(__m512*)&b,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_shuffle_f64x2(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_mask_shuffle_f64x2(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_shuffle_f64x2(__mmask8 k, v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_maskz_shuffle_f64x2(k,*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_shuffle_f64x2(v8f64 a, v8f64 b, int imm8) {
        __m512d zmm = _mm512_shuffle_f64x2(*(__m512d*)&a,*(__m512d*)&b,imm8);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_permutex_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_mask_permutex_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_permutex_ps(v16f32 src, __mmask16 k, v16f32 a, int imm8) {
        __m512 zmm = _mm512_mask_permutex_ps(*(__m512*)&src,k,*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_permutex_pd(__mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_maskz_permutex_pd(k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_permutex_ps(__mmask16 k, v16f32 a, int imm8) {
        __m512 zmm = _mm512_maskz_permutex_ps(k,*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_permutex_pd(v8f64 a, int imm8) {
        __m512d zmm = _mm512_permutex_pd(*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_permutex_ps(v16f32 a, int imm8) {
        __m512 zmm = _mm512_permutex_ps(*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_blend_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_blend_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_blend_ps(__mmask16 k, v16f32 a, v16f32 b) {
        __m512 zmm = _mm512_mask_blend_ps(k,*(__m512*)&a,*(__m512*)&b);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_compress_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_compress_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_compress_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_compress_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_compress_pd( __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_compress_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_compress_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_compress_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     void v8f64_mask_compressstoreu_pd(void * base_addr, __mmask8 k, v8f64 a) {
         _mm512_mask_compressstoreu_pd(base_addr,k,*(__m512d*)&a);
     }

     void v16f32_mask_compressstoreu_ps(void * base_addr, __mmask16 k, v16f32 a) {
         _mm512_mask_compressstoreu_ps(base_addr,k,*(__m512*)&a);
     }

     v8f64 v8f64_mask_expand_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_expand_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_expand_ps(v16f32 src, __mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_mask_expand_ps(*(__m512*)&src,k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_expand_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_expand_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_expand_ps(__mmask16 k, v16f32 a) {
        __m512 zmm = _mm512_maskz_expand_ps(k,*(__m512*)&a);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_mask_permute_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_mask_permute_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_mask_permute_ps(v16f32 src, __mmask16 k, v16f32 a, int imm8) {
         __m512 zmm = _mm512_mask_permute_ps(*(__m512*)&src,k,*(__m512*)&a,imm8);
	 return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_maskz_permute_pd( __mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_maskz_permute_pd(k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_maskz_permute_ps(__mmask16 k, v16f32 a, int imm8) {
        __m512 zmm = _mm512_maskz_permute_ps(k,*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     v8f64 v8f64_permute_pd(v8f64 a, int imm8) {
        __m512d zmm = _mm512_permute_pd(*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v16f32 v16f32_permute_ps(v16f32 a, int imm8) {
        __m512 zmm = _mm512_permute_ps(*(__m512*)&a,imm8);
	return (*(v16f32*)&zmm);
     }

     void avx512_add_pd(double   * __restrict c,
			const double * __restrict b,
			const double * __restrict a) {

       _mm512_store_pd(&c[0],_mm512_add_pd(*(__m512d*)&b[0],*(__m512d*)&a[0]));
     }

     void avx512_add_ps(float   * __restrict c,
			const float * __restrict b,
			const float * __restrict a) {

       _mm512_store_ps(&c[0],_mm512_add_ps(*(__m512*)&b[0],*(__m512*)&a[0]));
     }

     void avx512_sub_pd(double  * __restrict c,
			const double * __restrict b,
			const double * __restrict a) {

       _mm512_store_pd(&c[0],_mm512_sub_pd(*(__m512d*)&b[0],*(__m512*)&a[0]));
     }

     void avx512_sub_ps(float * __restrict c,
			const float * __restrict b,
			const float * __restrict a) {

        _mm512_store_ps(&c[0],_mm512_sub_ps(*(__m512*)&b[0],*(__m512*)&a[0]));
     }

     void avx512_mul_pd(double   * __restrict c,
			const double * __restrict b,
			const double * __restrict a) {

       _mm512_store_pd(&c[0],_mm512_mul_pd(*(__m512d*)&b[0],*(__m512d*)&a[0]));
     }

     void avx512_mul_ps(float  * __restrict c,
			const float * __restrict b,
			const float * __restrict a) {

       _mm512_store_ps(&c[0],_mm512_mul_ps(*(__m512*)&b[0],*(__m512*)&a[0]));
     }

     void avx512_div_pd(double  * __restrict c,
			const double * __restrict b,
			const double * __restrict a) {

       _mm512_store_pd(&c[0],_mm512_div_pd(*(__m512d*)&b[0],*(__m512*)&a[0]));
     }

     void avx512_div_ps(float  * __restrict c,
			const float * __restrict b,
			const float * __restrict a) {

       _mm512_store_ps(&c[0],_mm512_div_ps(*(__m512*)&b[0],*(__m51*)&a[0]));
     }

     void avx512_mask_ceil_pd(double * __restrict c,
			      const double * __restrict src,
			      const unsigned char k,
			      const double * __restrict a) {

       _mm512_store_pd(&c[0],_mm512_mask_ceil_pd(*(__m512d*)&src,k,*(__m512d*)&a[0]));
     }

     void avx512_mask_ceil_ps(float  * __restrict c,
			      const float * __restrict src,
			      const unsigned short k,
			      const float * __restrict a) {

       _mm512_store_ps(&c[0],_mm512_mask_ceil_ps(*(__m512*)&src,k,*(__m512*)&a[0]));
     }

     void avx512_ceil_pd(double * __restrict c,
			 const double * __restrict b) {

       _mm512_store_pd(&c[0], _mm512_ceil_pd(*(__m512d*)&b[0]));
     }

     void avx512_ceil_ps(float * __restrict c,
			 const float * __restrict b) {

       _mm512_store_ps(&c[0], _mm512_ceil_ps(*(__m512*)&b[0]));
     }

     void avx512_mask_abs_pd(double * __restrict c,
			      const double * __restrict src,
			      const unsigned char k,
			      const double * __restrict a) {

        _mm512_store_pd(&c[0], _mm512_mask_abs_pd(*(__m512d*)&src[0],k,*(__m512d*)&a[0]));
      }

     void avx512_mask_abs_ps(float  * __restrict c,
			      const float * __restrict src,
			      const unsigned short k,
			      const float * __restrict a) {

        _mm512_store_ps(&c[0], _mm512_mask_abs_ps(*(__m512*)&src[0],k,*(__m512*)&a[0]));
      }

     void avx512_mask_add_pd(double * __restrict c,
                             const double * __restrict src,
			     const unsigned char k,
			     const double * __restrict a,
			     const double * __restrict b) {

        _mm512_store_pd(&c[0], _mm512_mask_add_pd(*(__m512d*)&src[0],
	                                               k,*(__m512d*)&a[0],*(__m512d*)&b[0]));
      }

     void avx512_mask_add_ps(float * __restrict c,
                             const float * __restrict src,
			     const unsigned short k,
			     const float * __restrict a,
			     const float * __restrict b) {

        _mm512_store_ps(&c[0], _mm512_mask_add_ps(*(__m512*)&src[0],
	                                               k,*(__m512*)&a[0],*(__m512*)&b[0]));
      }

     void avx512_maskz_add_pd(double * __restrict c,
                              const unsigned char k,
			      const double * __restrict a,
			      const double * __restrict b) {

        _mm512_store_pd(&c[0], _mm512_maskz_add_pd(k,*(__m512d*)&a[0],*(__m512d*)&b[0]));
      }

     void avx512_maskz_add_ps(float * __restrict c,
                              const unsigned short k,
			      const float * __restrict a,
			      const float * __restrict b ) {

        _mm512_store_ps(&c[0], _mm512_maskz_add_ps(k,*(__m512*)&a[0],*(__m512*)&b[0]));
      }

      void avx512_add_round_pd(double * __restrict c,
                               const double * __restrict a,
			       const double * __restrict b,
			       const int rounding) {

        _mm512_store_pd(&c[0], _mm512_add_round_pd(*(__m512d*)&a[0],*(__m512d*)&b[0],rounding));
      }
     
      void avx512_add_round_ps(float * __restrict c,
                               const float * __restrict a,
			       const float * __restrict b,
			       const int rounding) {

        _mm512_store_ps(&c[0], _mm512_add_round_ps(*(__m512*)&a[0],*(__m512*)&b[0],rounding));
      }

      void avx512_maskz_add_round_pd(double * __restrict c,
                                     const unsigned char k,
				     const double * __restrict a,
				     const double * __restrict b,
				     const int rounding) {

        _mm512_store_pd(&c[0], _mm512_maskz_add_round_pd(k,*(__m512d*)&a[0],*(__m512d*)&b[0],rounding));
      }

      void avx512_maskz_add_round_ps(float * __restrict c,
                                     const unsigned short k,
				     const float * __restrict a,
				     const float * __restrict b,
				     const int rounding) {

        _mm512_store_ps(&c[0], _mm512_maskz_add_round_ps(k,*(__m512*)&a[0],*(__m512*)&b[0],rounding));
      }

      void avx512_mask_div_pd(double * __restrict c,
                              const double * __restrict src,
			      const unsigned char k,
			      const double * __restrict a,
			      const double * __restrict b) {

        _mm512_store_pd(&c[0], _mm512_mask_div_pd(*(__m512d*)&src[0],k,*(__m512d*)&a[0],*(__m512d*)&b[0]));
      }

      void avx512_mask_div_ps(float * __restrict c,
                              const float * __restrict src,
			      const unsigned short k,
			      const float * __restrict a,
			      const float * __restrict b) {

        _mm512_store_ps(&c[0], _mm512_mask_div_ps(*(__m512*)&src[0],k,*(__m512*)&a[0],*(__m512*)&b[0]));
      }

      void avx512_maskz_div_pd(double * __restrict c,
                              const unsigned char k,
			      const double * __restrict a,
			      const double * __restrict b) {

        _mm512_store_pd(&c[0], _mm512_maskz_div_pd(k,*(__m512d*)&a[0],*(__m512d*)&b[0]));
      }

      void avx512_maskz_div_ps(float * __restrict c,
                              const unsigned short k,
			      const float * __restrict a,
			      const float * __restrict b) {

        _mm512_store_ps(&c[0], _mm512_maskz_div_ps(k,*(__m512*)&a[0],*(__m512*)&b[0]));
      }

      void avx512_div_round_pd(double * __restrict c,
                               const double * __restrict a,
			       const double * __restrict b,
			       const int rounding) {

        _mm512_store_pd(&c[0], _mm512_div_round_pd(*(__m512d*)&a[0],*(__m512d*)&b[0],rounding));
      }

      void avx512_div_round_ps(float * __restrict c,
                               const float * __restrict a,
			       const float * __restrict b,
			       const int rounding) {

        _mm512_store_ps(&c[0], _mm512_div_round_ps(*(__m512*)&a[0],*(__m512*)&b[0],rounding));
      }

     
