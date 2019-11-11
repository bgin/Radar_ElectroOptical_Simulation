

 #include <immintrin.h>
 #include "GMS_simd_avx512.h"



     v8f64 v8f64_add_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_add_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_sub_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_sub_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mul_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mul_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_div_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_div_pd(*(__m512d*)&a,*(__m512d*)&b);
        return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_ceil_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_ceil_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_ceil_pd(v8f64 a) {
        __m512d zmm = _mm512_ceil_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_abs_pd(v8f64 a) {
        __m512d zmm = _mm512_abs_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_abs_pd(v8f64 a, __mmask8 k, v8f64 b) {
        __m512d zmm = _mm512_mask_abs_pd(*(__m512d*)&a,k,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_add_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_add_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_add_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_add_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_add_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_add_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_add_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_add_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_add_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_add_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_div_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_div_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_div_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_div_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_div_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_div_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_div_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_div_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_div_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_div_round_pd(k,*(__m512d*)&,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmadd_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmadd_round_pd(_mmask8 k,v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmaddsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmaddsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmaddsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmaddsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmaddsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmaddsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmaddsub_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmaddsub_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmaddsub_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmaddsub_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmaddsub_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmaddsub_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmaddsub_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __mm512d zmm = _mm512_mask3_fmaddsub_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmaddsub_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmaddsub_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_maskz_fmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmsubadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fmsubadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmsubadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fmsubadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmsubadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fmsubadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmsubadd_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fmsubadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fmsubadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fmsubadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fmsubadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fmsubadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fmsubadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fmsubadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fmusbadd_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fmsubadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fnmadd_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fnmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fnmadd_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fnmadd_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fnmadd_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fnmadd_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     
