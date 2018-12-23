
#include <immintrin.h>
#include "simd_avx.h"

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
	return (*(v4f64*)&(_mm256_add_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_sub_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_sub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_mul_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_mul_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_div_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_div_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_addsub_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_addsub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_and_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_and_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_andnot_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_andnot_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_blend_vec4f64(v4f64 a, v4f64 b, const int imm) {
	return (*(v4f64*)&(_mm256_blend_pd(*(__m256d*)&a,*(__m256d*)&b,imm)));
}

v4f64 vec4f64_blendv_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_blendv_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_ceil_v4f64(v4f64 a) {
	return (*(v4f64*)&(_mm256_ceil_pd(*(__m256d*)&a)));
}

v4f64 vec4f64_cmp_vec4f64(v4f64 a, v4f64 b, const int imm) {
	return (*(v4f64*)&(_mm256_cmp_pd(*(__m256d*)&a,*(__m256d*)&b,imm)));
}

v4f64 vec4f64_floor_vec4f64(v4f64 a) {
	return (*(v4f64*)&(_mm256_floor_pd(*(__m256d*)&a)));
}

v4f64 vec4f64_fmadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fmadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmaddsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fmaddsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fmsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fmsubadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fmsubadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fnmadd_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fnmadd_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_fnmsub_vec4f64(v4f64 a, v4f64 b, v4f64 c) {
	return (*(v4f64*)&(_mm256_fnmsub_pd(*(__m256d*)&a,*(__m256d*)&b,*(__m256d*)&c)));
}

v4f64 vec4f64_hadd_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_hadd_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_hsub_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_hsub_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_max_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_max_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_min_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_min_pd(*(__m256d*)&a,*(__m256d*)&b)));
}

v4f64 vec4f64_movedup_vec4f64(v4f64 a) {
	return (*(v4f64*)&(_mm256_movedup_pd(*(__m256d*)&a)));
}

int vec4f64_movemask_vec4f64(v4f64 a) {
	return (_mm256_movemask_pd(*(__m256d*)&a));
}

v4f64 vec4f64_or_vec4f64(v4f64 a, v4f64 b) {
	return (*(v4f64*)&(_mm256_or_pd(*(__m256d*)&a, *(__m256d*)&b)));
}

v4f64 vec4f64_permute2f128_vec4f64(v4f64 a, v4f64 b, const int imm) {
	return (*(v4f64*)&(_mm256_permute2f128_pd(*(__m256d*)&a, *(__m256d*)&b, imm)));
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

void v4f64_add_pd(double * __restrict c, 
		          double * __restrict b, 
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_add_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void v4f64_sub_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_sub_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void v4f64_mul_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_mul_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void v4f64_div_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_div_pd(*(__m256d*)&b[0], *(__m256d*)&a[0]));
}

void v4f64_addsub_pd(double * __restrict c,
				     double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_addsub_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void v4f64_and_pd(double * __restrict c,
				  double * __restrict b,
				  double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_and_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void v4f64_andnot_pd(double * __restrict c,
				     double * __restrict b,
					 double * __restrict a) {
	_mm256_store_pd(&c[0], _mm256_andnot_pd(*(__m256d*)&b[0],*(__m256d*)&a[0]));
}

void v4f64_blend_pd(double * __restrict c,
					double * __restrict b,
				    double * __restrict a,
					const int imm) {
	_mm256_store_pd(&c[0], _mm256_blend_pd(*(__m256d*)&b[0],*(__m256d*)&a[0],imm));
}

void v4f64_blendv_pd(double * __restrict c,
					 double * __restrict b,
					 double * __restrict a,
					 double * __restrict pred) {
	_mm256_store_pd(&c[0], _mm256_blendv_pd(*(__m256d*)&b[0],*(__m256d*)&a[0],*(__m256d*)&pred[0]));
}

