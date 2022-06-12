
#include <immintrin.h>
#include "GMS_simd_avx.h"

/*
	  Function returning v4f64 struct implementation
	  Machine code:

	  000007F6B6261105 48 8D 15 B4 40 00 00 lea         rdx,[__security_cookie_complement+1A8h (07F6B62651C0h)]
	  000007F6B626110C 49 8D 4D 00          lea         rcx,[r13]
	  000007F6B6261110 4C 8D 05 E9 40 00 00 lea         r8,[__security_cookie_complement+1E8h (07F6B6265200h)]
	  000007F6B6261117 E8 D4 00 00 00       call        v256_add_pd2 (07F6B62611F0h)
	  000007F6B62611F0 48 83 EC 78          sub         rsp,78h

	  000007F6B62611F4 48 89 C8             mov         rax,rcx

	  000007F6B62611F7 4C 89 6C 24 60       mov         qword ptr [rsp+60h],r13
	  000007F6B62611FC 4C 8D 6C 24 3F       lea         r13,[rsp+3Fh]
	  21:
	  000007F6B6261201 C5 FD 10 02          vmovupd     ymm0,ymmword ptr [rdx]
	  1:
	  2:

	  20:
	  000007F6B6261205 49 83 E5 E0          and         r13,0FFFFFFFFFFFFFFE0h
	  21:
	  000007F6B6261209 C4 C1 7D 58 08       vaddpd      ymm1,ymm0,ymmword ptr [r8]
	  000007F6B626120E C4 C1 7D 11 4D 00    vmovupd     ymmword ptr [r13],ymm1
	  000007F6B6261214 C4 C1 78 10 55 10    vmovups     xmm2,xmmword ptr [r13+10h]
	  000007F6B626121A C4 C1 78 10 5D 00    vmovups     xmm3,xmmword ptr [r13]
	  000007F6B6261220 C5 F8 11 51 10       vmovups     xmmword ptr [rcx+10h],xmm2
	  000007F6B6261225 C5 F8 11 19          vmovups     xmmword ptr [rcx],xmm3
	  000007F6B6261229 4C 8B 6C 24 60       mov         r13,qword ptr [rsp+60h]
	  000007F6B626122E C5 F8 77             vzeroupper
	  000007F6B6261231 48 83 C4 78          add         rsp,78h
	  000007F6B6261235 C3                   ret
*/


v4f64 vec4f64_add_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_add_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_add_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_sub_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_sub_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_sub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_mul_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_mul_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_mul_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_div_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_div_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_div_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_addsub_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_addsub_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_addsub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_and_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_and_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_and_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_andnot_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_andnot_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_andnot_pd(*(__m256d*)&a,*(__m256d*)&b)));
}


// Compile error:  -- //1>C:\Users\Bernard\documents\visual studio 2013\Projects\GuidedMissileSim\LibSIMD\simd_avx.cpp(69, 2) : error : Intrinsic parameter must be an immediate value
 v4f64 vec4f64_blend0_vec4f64(v4f64 a, v4f64 b) {
	 __m256d ret = _mm256_blend_pd(*(__m256d*)&a, *(__m256d*)&b, 0);
	 return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_blend_pd(*(__m256d*)&a,*(__m256d*)&b,0)));
} 

 v4f64 vec4f64_blend1_vec4f64(v4f64 a, v4f64 b) {
	 __m256d ret = _mm256_blend_pd(*(__m256d*)&a, *(__m256d*)&b, 1);
	 return (*(v4f64*)&ret);
	 //return (*(v4f64*)&(_mm256_blend_pd(*(__m256d*)&a, *(__m256d*)&b,1)));
 }

v4f64 vec4f64_blendv_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_blendv_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_blendv_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_ceil_v4f64(v4f64 a) {
	__m256d ret = _mm256_ceil_pd(*(__m256d*)&a);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_ceil_pd(*(__m256d*)&a)));
}

v4f64 vec4f64_cmp_eqoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b, 0);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a,*(__m256d*)&b,0)));
}

v4f64 vec4f64_cmp_ltoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b, 17);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,17)));
}

v4f64 vec4f64_cmp_leoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b, 18);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,18)));
}

v4f64 vec4f64_cmp_neqoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b, 12);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,12)));
}

v4f64 vec4f64_cmp_gtoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b, 29);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,29)));
}

v4f64 vec4f64_cmp_geoq_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,30);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a, *(__m256d*)&b,30)));
}

v4f64 vec4f64_floor_vec4f64(v4f64 a) {
	__m256d ret = _mm256_floor_pd(*(__m256d*)&a);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_floor_pd(*(__m256d*)&a)));
}

v4f64 vec4f64_fmadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fmadd_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fmadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmaddsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fmaddsub_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fmaddsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fmsub_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fmsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmsubadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fmsubadd_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fmsubadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fnmadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fnmadd_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fnmadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fnmsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	__m256d ret = _mm256_fnmsub_pd(*(__m256d*)&a, *(__m256d*)&b, *(__m256d*)&c);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_fnmsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_hadd_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_hadd_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_hadd_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_hsub_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_hsub_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_hsub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_max_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_max_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_max_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_min_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_min_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_min_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_movedup_vec4f64(v4f64 a) {
	__m256d ret = _mm256_movedup_pd(*(__m256d*)&a);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_movedup_pd(*(__m256d*)&a)));
}

int vec4f64_movemask_vec4f64(v4f64 a) {
	return (_mm256_movemask_pd(*(__m256d*)&a));
}

v4f64 vec4f64_or_vec4f64(v4f64 a, v4f64 b) {
	__m256d ret = _mm256_or_pd(*(__m256d*)&a, *(__m256d*)&b);
	return (*(v4f64*)&ret);
	//return (*(v4f64*)&(_mm256_or_pd(*(__m256d*)&a, *(__m256d*)&b)));
}




//        void function -- implementations
// ================================================================================ //
//

/*
		Machine code implementation of void function wrappers

		000007F7B7A611A0 C5 FD 10 01          vmovupd     ymm0,ymmword ptr [rcx]
		000007F7B7A611A4 C5 FD 58 0A          vaddpd      ymm1,ymm0,ymmword ptr [rdx]
		000007F7B7A611A8 C4 C1 7D 11 08       vmovupd     ymmword ptr [r8],ymm1

		000007F7B7A611AD C5 F8 77             vzeroupper
		000007F7B7A611B0 C3                   ret

*/

void vec4f64_add_pd(double * __restrict c, 
		          double * __restrict b, 
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_add_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_sub_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_sub_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_mul_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_mul_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_div_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_div_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_addsub_pd(double * __restrict c,
				     double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_addsub_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_and_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_and_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_andnot_pd(double * __restrict c,
				     double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_andnot_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}



void vec4f64_blendv_pd(double * __restrict c,
					 double * __restrict b,
					 double * __restrict a,
					 double * __restrict pred) {
	_mm256_store_pd(&c[0], _mm256_blendv_pd(*(__m256d*)&b[0],*(__m256d*)&a[0],*(__m256d*)&pred[0]));
}

void vec4f64_ceil_pd(double * __restrict c,
				   double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_ceil_pd(*(__m256d*)&a[0]));
}

void vec4f64_cmpeq_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
				 
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0],*(__m256d*)&a[0],0));
}

void vec4f64_cmpneq_pd(double * __restrict c,
					   double * __restrict b,
					   double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0], *(__m256d*)&a[0],12));
}

void vec4f64_cmplt_pd(double * __restrict c,
					  double * __restrict b,
					  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0], *(__m256d*)&a[0], 17));
}

void vec4f64_cmple_pd(double * __restrict c,
					  double * __restrict b,
					  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0], *(__m256d*)&a[0], 18));
}

void vec4f64_cmpgt_pd(double * __restrict c,
					  double * __restrict b,
					  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0], *(__m256d*)&a[0], 29));
}

void vec4f64_cmpge_pd(double * __restrict c,
					  double * __restrict b,
					  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_cmp_pd(*(__m256d*)&b[0], *(__m256d*)&a[0], 30));
}

void vec4f64_floor_pd(double * __restrict c,
				    double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_floor_pd(*(__m256d*)&a[0]));
}

void vec4f64_fmadd_pd(double * __restrict d,
				    double * __restrict c,
					double * __restrict b,
				    double * __restrict a) {
	_mm256_store_pd(&d[0], _mm256_fmadd_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_fmaddsub_pd(double * __restrict d,
					   double * __restrict c,
					   double * __restrict b,
					   double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_fmaddsub_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_fmsub_pd(double * __restrict d,
					 double * __restrict c,
					 double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&d[0], _mm256_fmsub_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_fmsubadd_pd(double * __restrict d,
					   double * __restrict c,
					   double * __restrict b,
					   double * __restrict a) {
	_mm256_store_pd(&d[0], _mm256_fmsubadd_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a));
}

void vec4f64_fnmadd_pd(double * __restrict d,
					   double * __restrict c,
					   double * __restrict b,
				       double * __restrict a) {
	_mm256_store_pd(&d[0], _mm256_fnmadd_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_fnmsub_pd(double * __restrict d,
					   double * __restrict c,
					   double * __restrict b,
					   double * __restrict a) {
	_mm256_store_pd(&d[0], _mm256_fnmsub_pd(*(__m256d*)&c[0],*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_hadd_pd(double * __restrict c,
					 double * __restrict b,
				     double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_hadd_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_hsub_pd(double * __restrict c,
				     double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_hsub_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_max_pd(double * __restrict c,
				    double * __restrict b,
					double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_max_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_min_pd(double * __restrict c,
					double * __restrict b,
					double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_min_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec4f64_movedup_pd(double * __restrict b,
						double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_movedup_pd(*(__m256d*)&a[0]));
}

void vec4f64_movemask_pd(double * __restrict a,
						 int * imm) {
	*imm = _mm256_movemask_pd(*(__m256d*)&a[0]);
}

void vec4f64_or_pd(double * __restrict c,
				   double * __restrict b,
				   double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_or_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}



void vec4f64_cvtpd_ps(float * __restrict b,
					  double * __restrict a) {
	_mm_store_ps(&b[0], _mm256_cvtpd_ps(*(__m256d*)&a[0]));
}

void vec4f64_cvtps_pd(double * __restrict b,
					  float * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cvtps_pd(*(__m128*)&a[0]));
}

void vec4f64_sqrt_pd(double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_sqrt_pd(*(__m256d*)&a[0]));
}

void vec4f64_undefined_pd(double * __restrict a) {
	_mm256_store_pd(&a[0], _mm256_undefined_pd());
}

void vec_zeroall(void) {
	_mm256_zeroall();
}

void vec_zeroupper(void) {
	_mm256_zeroupper();
}

void vec4f64_broadcast_pd(double * __restrict b,
					      double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_broadcast_pd((__m128d*)&a[0]));
}

void vec4f64_broadcast_sd(double * __restrict b,
						  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_broadcast_sd(&a[0]));
} 

void vec4f64_testc_pd(int * c,
					  double * __restrict b,
					  double * __restrict a) {
	*c = _mm256_testc_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]);
}

void vec4f64_testnzc_pd(int * c,
					    double * __restrict b,
						double * __restrict a) {
	*c = _mm256_testnzc_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]);
}

void vec4f64_testz_pd(int * c,
					  double * __restrict b,
					  double * __restrict a) {
	*c = _mm256_testz_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]);
}

void vec4f64_round_nearest_pd(double * __restrict b,
					  double * __restrict a){
					 
	_mm256_store_pd(&b[0], _mm256_round_pd(*(__m256d*)&a[0],_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC));
}

void vec4f64_round_down_pd(double * __restrict b,
						   double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_round_pd(*(__m256d*)&a[0], _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC));
}

void vec4f64_round_up_pd(double * __restrict b,
					     double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_round_pd(*(__m256d*)&a[0], _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC));
}

void vec4f64_round_truncate_pd(double * __restrict b,
							   double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_round_pd(*(__m256d*)&a[0], _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC));
}

void vec4f64_stream_pd(double * __restrict b,
					   double * __restrict a) {
	_mm256_stream_pd(&b[0], *(__m256d*)&a[0]);
}

void vec4f64_unpacklo_pd(double * __restrict c,
					     double * __restrict b,
					     double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_unpacklo_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec4f64_unpackhi_pd(double * __restrict c,
						 double * __restrict b,
						 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_unpackhi_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}


void vec2f64_acos_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_acos_pd(*(__m128d*)&a[0]));
}

void vec4f64_acos_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_acos_pd(*(__m256d*)&a[0]));
}

void vec2f64_acosh_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_acosh_pd(*(__m128d*)&a[0]));
}

void vec4f64_acosh_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_acosh_pd(*(__m256d*)&a[0]));
}

void vec2f64_asin_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_asin_pd(*(__m128d*)&a[0]));
}

void vec4f64_asin_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_asin_pd(*(__m256d*)&a[0]));
}

void vec2f64_asinh_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_asinh_pd(*(__m128d*)&a[0]));
}

void vec4f64_asinh_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_asinh_pd(*(__m256d*)&a[0]));
}

void vec2f64_atan2_pd(double * __restrict c,
					  const double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&c[0], _mm_atan2_pd(*(__m128d*)&b[0],*(__m128d*)&a[0]));
}

void vec4f64_atan2_pd(double * __restrict c,
					  const double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_atan2_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void vec2f64_atan_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_atan_pd(*(__m128d*)&a[0]));
}

void vec4f64_atan_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_atan_pd(*(__m256d*)&a[0]));
}

void vec2f64_atanh_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_atanh_pd(*(__m128d*)&a[0]));
}

void vec4f64_atanh_pd(double * __restrict b,
				      const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_atanh_pd(*(__m256d*)&a[0]));
}

void vec2f64_cbrt_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_cbrt_pd(*(__m128d*)&a[0]));
}

void vec4f64_cbrt_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cbrt_pd(*(__m256d*)&a[0]));
}

void vec2f64_cdfnorm_pd(double * __restrict b,
					    const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_cdfnorm_pd(*(__m128d*)&a[0]));
}

void vec4f64_cdfnorm_pd(double * __restrict b,
					    const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cdfnorm_pd(*(__m256d*)&a[0]));
}

void vec2f64_cdfnorminv_pd(double * __restrict b,
						   const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_cdfnorminv_pd(*(__m128d*)&a[0]));
}

void vec4f64_cdfnorminv_pd(double * __restrict b,
						   const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cdfnorminv_pd(*(__m256d*)&a[0]));
}

void vec2f64_cos_pd(double * __restrict b,
					const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_cos_pd(*(__m128d*)&a[0]));
}

void vec4f64_cos_pd(double * __restrict b,
					const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cos_pd(*(__m256d*)&a[0]));
}

void vec2f64_cosh_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_cosh_pd(*(__m128d*)&a[0]));
}

void vec4f64_cosh_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_cosh_pd(*(__m256d*)&a[0]));
}

void vec2f64_erf_pd(double * __restrict b,
					const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_erf_pd(*(__m128d*)&a[0]));
}

void vec4f64_erf_pd(double * __restrict b,
					const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_erf_pd(*(__m256d*)&a[0]));
}

void vec2f64_erfc_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_erfc_pd(*(__m128d*)&a[0]));
}

void vec4f64_erfc_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_erfc_pd(*(__m256d*)&a[0]));
}

void vec2f64_erfcinv_pd(double * __restrict b,
					   const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_erfcinv_pd(*(__m128d*)&a[0]));
}

void vec4f64_erfcinv_pd(double * __restrict b,
						const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_erfcinv_pd(*(__m256d*)&a[0]));
}

void vec2f64_erfinv_pd(double * __restrict b,
					   const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_erfinv_pd(*(__m128d*)&a[0]));
}

void vec4f64_erfinv_pd(double * __restrict b,
					   const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_erfinv_pd(*(__m256d*)&a[0]));
}

void vec2f64_exp10_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_exp10_pd(*(__m128d*)&a[0]));
}

void vec4f64_exp10_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_exp10_pd(*(__m256d*)&a[0]));
}

void vec2f64_exp2_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_exp2_pd(*(__m128d*)&a[0]));
}

void vec4f64_exp2_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_exp2_pd(*(__m256d*)&a[0]));
}

void vec2f64_exp_pd(double * __restrict b,
				    const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_exp_pd(*(__m128d*)&a[0]));
}

void vec4f64_exp_pd(double * __restrict b,
				    const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_exp_pd(*(__m256d*)&a[0]));
}

void vec2f64_expm1_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_expm1_pd(*(__m128d*)&a[0]));
}

void vec4f64_expm1_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_expm1_pd(*(__m256d*)&a[0]));
}

void vec2f64_hypot_pd(double * __restrict c,
					  const double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&c[0], _mm_hypot_pd(*(__m128d*)&b[0], *(__m128d*)&a[0]));
}

void vec4f64_hypot_pd(double * __restrict c,
					  const double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_hypot_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void vec2f64_invcbrt_pd(double * __restrict b,
						const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_invcbrt_pd(*(__m128d*)&a[0]));
}

void vec4f64_invcbrt_pd(double * __restrict b,
					    const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_invcbrt_pd(*(__m256d*)&a[0]));
}

void vec2f64_invsqrt_pd(double * __restrict b,
						 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_invsqrt_pd(*(__m128d*)&a[0]));
}

void vec4f64_invsqrt_pd(double * __restrict b,
						const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_invsqrt_pd(*(__m256d*)&a[0]));
}

void vec2f64_log10_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_log10_pd(*(__m128d*)&a[0]));
}

void vec4f64_log10_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_log10_pd(*(__m256d*)&a[0]));
}

void vec2f64_log1p_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_log1p_pd(*(__m128d*)&a[0]));
}

void vec4f64_log1p_pd(double * __restrict b,
					  const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_log1p_pd(*(__m256d*)&a[0]));
}

void vec2f64_log2_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_log2_pd(*(__m128d*)&a[0]));
}

void vec4f64_log2_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_log2_pd(*(__m256d*)&a[0]));
}

void vec2f64_log_pd(double * __restrict b,
				    const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_log_pd(*(__m128d*)&a[0]));
}

void vec4f64_log_pd(double * __restrict b,
				    const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_log_pd(*(__m256d*)&a[0]));
}

void vec2f64_logb_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm_store_pd(&b[0], _mm_logb_pd(*(__m128d*)&a[0]));
}

void vec4f64_logb_pd(double * __restrict b,
					 const double * __restrict a) {
	_mm256_store_pd(&b[0], _mm256_logb_pd(*(__m256d*)&a[0]));
}




void vecf32_divps_acc_24bit(float * __restrict c,
						  const float * __restrict b,
					      const float * __restrict a,
						  const long long int len) {
	
	__asm{
		mov rcx, c
		mov rbx, b
		mov rax, a
		mov rsi, len
		xor rdx, rdx
		
		L1:
		
		vmovups ymm0, [rax+rdx*1]
		vmovups ymm1, [rbx+rdx*1]
		vdivps  ymm2, ymm1, ymm0
		vmovups[rcx+rdx*1], ymm2
		add rdx, 32
		cmp rdx, rsi
		jl L1
	}
}

void vecf32_rcpps_acc_11bit(float * __restrict c,
							const float * __restrict b,
						    const float * __restrict a,
							const long long int len) {
	__asm {
		mov rcx, c
		mov rbx, b
		mov rax, a
		mov rsi, len
		xor rdx, rdx

		L1:

		vmovups  ymm0, [rax+rdx*1]
		vmovups  ymm1, [rbx+rdx*1]
		vrcpps ymm1, ymm1
		vmulps ymm2, ymm1, ymm0
		vmovups[rcx+rdx*1], ymm2
		add rdx, 32
		cmp rdx, rsi
		jl L1
	}
}

void vecf32_rcpps_mulps_acc_22bit(float * __restrict c,
								  const float * __restrict b,
							      const float * __restrict a,
								  const int long long len) {
	
	__asm {
	    mov rcx, c
		mov rbx, b
		mov rax, a
		mov rsi, len
		xor rdx, rdx

		L1:

		vmovups ymm0, [rax+rdx*1]
		vmovups ymm1, [rbx+rdx*1]
		vrcpps  ymm3, ymm1
		vaddps  ymm2, ymm3, ymm3
		vmulps  ymm3, ymm3, ymm3
		vmulps  ymm3, ymm3, ymm1
		vsubps  ymm2, ymm2, ymm3
		vmulps  ymm0, ymm0, ymm2
		vmovups[rcx+rdx*1], ymm0
		add rdx, 32
		cmp rdx, rsi
		jl L1
	}
}




