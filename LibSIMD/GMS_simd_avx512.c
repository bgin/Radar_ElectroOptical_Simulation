

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

     v8f64 v8f64_maskz_fnmadd_pd(__mmask8 k,v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fnmadd_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fnmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_fnmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fnmadd_round_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_mask_fnmadd_round_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fnmadd_round_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k, int rounding) {
        __m512d zmm = _mm512_mask3_fnmadd_round_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fnmadd_round_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c, int rounding) {
        __m512d zmm = _mm512_maskz_fnmadd_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_fnmsub_pd(v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_fnmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_fnmsub_pd(v8f64 a, __mmask8 k, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_mask_fnmsub_pd(*(__m512d*)&a,k,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask3_fnmsub_pd(v8f64 a, v8f64 b, v8f64 c, __mmask8 k) {
        __m512d zmm = _mm512_mask3_fnmsub_pd(*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c,k);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_fnmsub_pd(__mmask8 k, v8f64 a, v8f64 b, v8f64 c) {
        __m512d zmm = _mm512_maskz_fnmsub_pd(k,*(__m512d*)&a,*(__m512d*)&b,*(__m512d*)&c);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_mul_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_mul_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_mul_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_mul_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_mul_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_mul_round_pd(*(__m512d)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding)
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_mul_round_pd(__mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_mul_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_reduce_add_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_reduce_add_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_reduce_add_pd(v8f64 a) {
        __m512d zmm = _mm512_reduce_add_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_reduce_mul_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_reduce_mul_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_reduce_mul_pd(v8f64 a) {
        __m512d zmm = _mm512_reduce_mul_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_sub_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_mask_sub_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_sub_pd(__mmask8 k, v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_maskz_sub_pd(k,*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     __mmask8  v8f64_cmp_pd_mask(v8f64 a, v8f64 b, int imm8) {
        return ( _mm512_cmp_pd_mask(*(__m512d*)&a,*(__m512d*)&b,imm8));
     }	

     __mmask8 v8f64_mask_cmp_pd_mask(__mmask8 k1, v8f64 a, v8f64 b, int imm8) {
        return ( _mm512_mask_cmp_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b,imm8));
     }

     __mmask8 v8f64_cmp_round_pd_mask(v8f64 a, v8f64 b, int imm8, int sae) {
        return ( _mm512_cmp_round_pd_mask(*(__m512d*)&a,*(__m512d*)&b,imm8,sae));
     }

     __mmask8 v8f64_mask_cmp_round_pd_mask(__mmask8 k1, v8f64 a, v8f64 b, int imm8, int sae) {
        return ( _mm512_mask_cmp_round_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b,imm8,sae));
     }

     __mmask8 v8f64_cmpeq_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpeq_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmpeq_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpeq_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmpunord_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpunord_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmpunord_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpunord_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmpord_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpord_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmpord_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpord_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmpnlt_pd_mask(v8f64 a, v8f64 b) {
        return ( __m512_cmpnlt_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmpnlt_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpnlt_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&a));
     }

     __mmask8 v8f64_cmpnle_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpnle_pd_mask(*(__m512d*)&a,*(__m512d*)&));
     }

     __mmask8 v8f64_mask_cmpnle_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpnle_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmpneq_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmpneq_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmpneq_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmpneq_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmplt_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmplt_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmplt_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmplt_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_cmple_pd_mask(v8f64 a, v8f64 b) {
        return ( _mm512_cmple_pd_mask(*(__m512d*)&a,*(__m512d*)&b));
     }

     __mmask8 v8f64_mask_cmple_pd_mask(__mmask8 k1, v8f64 a, v8f64 b) {
        return ( _mm512_mask_cmple_pd_mask(k1,*(__m512d*)&a,*(__m512d*)&b));
     }

     v8f64 v8f64_mask_rcp14_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_rcp14_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_rcp14_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_rcp14_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_rcp14_pd(v8f64 a) {
        __m512d zmm = _mm512_rcp14_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_rsqrt14_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_rsqrt14_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_rsqrt14_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_rsqrt14_pd
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_sqrt_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_sqrt_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_sqrt_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_sqrt_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_sqrt_round_pd(v8f64 src, __mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_mask_sqrt_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_sqrt_round_pd(__mmask8 k, v8f64 src, int rounding) {
        __m512d zmm = _mm512_maskz_sqrt_round_pd(k,*(__m512d*)&src,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_expandloadu_pd(v8f64 src, __mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_mask_expandloadu_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_expandloadu_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_expandloadu_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_load_pd(v8f64 src, __mmask8 k, void * mem_add) {
        __m512d zmm = _mm512_mask_load_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_load_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_load_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_loadu_pd(v8f64 src, __mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_mask_loadu_pd(*(__m512d*)&src,k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_loadu_pd(__mmask8 k, void * mem_addr) {
        __m512d zmm = _mm512_maskz_loadu_pd(k,mem_addr);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_getexp_pd(v8f64 a) {
        __m512d zmm = _mm512_getexp_pd(*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_getexp_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_getexp_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_getexp_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512maskz_getexp_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_getexp_round_pd(v8f64 a, int rounding) {
        __m512d zmm = _mm512_getexp_round_pd(*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_getexp_round_pd(v8f64 src, __mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_mask_getexp_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_getexp_round_pd(__mmask8 k, v8f64 a, int rounding) {
        __m512d zmm = _mm512_maskz_getexp_round_pd(k,*(__m512d*)&a,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_getmant_pd(v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_getmant_pd(*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_gemant_pd(v8f64 src, __mmask8 k, v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_mask_getmant_pd(*(__m512d*)&src,k,*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_getmant_pd(__mmask8 k, v8f64 a, int interv, int sc) {
        __m512d zmm = _mm512_maskz_getmant_pd(k,*(__m512d*)&a,interv,sc);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_getmant_round_pd(v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_getmant_round_pd(*(__m512d*)&a,interv,sc,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_getmant_round_pd(v8f64 src, __mmask8 k, v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_mask_getmant_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,sc,interv,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_getmant_round_pd(__mmask8 k, v8f64 a, int interv, int sc, int rounding) {
        __m512d zmm = _mm512_maskz_getmant_round_pd(k,*(__m512d*)&a,interv,sc,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_roundscale_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_mask_roundscale_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8) 
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_roundscale_pd(__mmask8 k, v8f64 a, int imm8) {
        __m512d zmm = _mm512_maskz_roundscale_pd(k,*(__m512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_roundscale_pd(v8f64 a, int imm8) {
        __m512d zmm = _mm512_roundscale_pd(*(__512d*)&a,imm8);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_roundscale_round_pd(v8f64 src, __mmask8 k, v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_mask_roundscale_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_roundscale_round_pd(__mask8 k, v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_maskz_roundscale_round_pd(k,*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_roundscale_round_pd(v8f64 a, int imm8, int rounding) {
        __m512d zmm = _mm512_roundscale_round_pd(*(__m512d*)&a,imm8,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_scalef_pd(v8f64 a, v8f64 b) {
        __m512d zmm = _mm512_scalef_pd(*(__m512d*)&a,*(__m512d*)&b);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_scalef_round_pd(v8f64 src, __mmask8 k, v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_mask_scalef_round_pd(*(__m512d*)&src,k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_scalef_round_pd(__mmask8 k ,v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_maskz_scalef_round_pd(k,*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_scalef_round_pd(v8f64 a, v8f64 b, int rounding) {
        __m512d zmm = _mm512_scalef_round_pd(*(__m512d*)&a,*(__m512d*)&b,rounding);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_mask_mov_pd(v8f64 src, __mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_mask_mov_pd(*(__m512d*)&src,k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }

     v8f64 v8f64_maskz_mov_pd(__mmask8 k, v8f64 a) {
        __m512d zmm = _mm512_maskz_mov_pd(k,*(__m512d*)&a);
	return (*(v8f64*)&zmm);
     }
