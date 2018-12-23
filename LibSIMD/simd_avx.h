
#ifndef __SIMD_AVX_H__
#define __SIMD_AVX_H__

// File version number granularity
const int gSIMD_AVX_MAJOR = 1;

const int gSIMD_AVX_MINOR = 0;

const int gSIMD_AVX_MICRO = 0;

const int gSIMD_AVX_FULLVER = 1000*gSIMD_AVX_MAJOR+100*gSIMD_AVX_MINOR+10*gSIMD_AVX_MICRO;

const char * const gSIMD_AVX_CREATE_DATE = "22-12-2018 11:08 + 00200 (SAT 22 DEC 2018 GMT+2)";

const char * const gSIMD_AVX_BUILD_DATE = "00-00-0000 00:00";

const char * const gSIMD_AVX_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";

const char * const gSIMD_AVX_SYNOPSIS = "Callable from Fortran C wrappers for Intel AVX Intrinsics.";

#if defined (__cplusplus)

extern "C" {
	
	// Interoparable with coresponding Fortran derived type.
	typedef struct __declspec(align(32)) v4f64 {
		double v[4];
	}v4f64;

	// Vector addition
	// Calls: _mm256_add_pd
	v4f64 vec4f64_add_vec4f64(v4f64, v4f64);

	// Vector subtraction
	// Calls: _mm256_sub_pd
	v4f64 vec4f64_sub_vec4f64(v4f64, v4f64);

	// Vector multiplication
	// Calls: _mm256_mul_pd
	v4f64 vec4f64_mul_vec4f64(v4f64, v4f64);

	// Vector division
	// Calls: _mm256_div_pd
	v4f64 vec4f64_div_vec4f64(v4f64, v4f64);

    // Vector alternate addition-subtraction
	// Calls: _mm256_addsub_pd
	v4f64 vec4f64_addsub_vec4f64(v4f64, v4f64);

	// Vector bitwise AND
	// Calls: _mm256_and_pd
	v4f64 vec4f64_and_vec4f64(v4f64, v4f64);

	// Vector bitwise AND-NOT
    // Calls: _mm256_andnot_pd
	v4f64 vec4f64_andnot_vec4f64(v4f64, v4f64);

	// Vector blend
	// Calls: _mm256_blend_pd
	v4f64 vec4f64_blend_vec4f64(v4f64, v4f64, const int);

	// Vector blend
	// Calls: _mm256_blendv_pd
	v4f64 vec4f64_blendv_vec4f64(v4f64, v4f64, v4f64);

	// Vector ceil
	// Calls: _mm256_ceil_pd
	v4f64 vec4f64_ceil_vec4f64(v4f64);

	// Vector compare
	// Calls: _mm256_cmp_pd
	v4f64 vec4f64_cmp_vec4f64(v4f64, v4f64, const int);

    // Vector floor
	// Calls: _mm256_floor_pd
	v4f64 vec4f64_floor_vec4f64(v4f64);

	// Vector FMADD
	// Calls: _mm256_fmadd_pd
	v4f64 vec4f64_fmadd_vec4f64(v4f64, v4f64, v4f64);

	// Vector FMADDSUB
	// Calls: _mm256_fmaddsub_pd
	v4f64 vec4f64_fmaddsub_vec4f64(v4f64, v4f64, v4f64);

	// Vector FMSUB
	// Calls: _mm256_fmsub_pd
	v4f64 vec4f64_fmsub_vec4f64(v4f64, v4f64, v4f64);

	// Vector FMSUBADD
	// Calls: _mm256_fmsubadd_pd
	v4f64 vec4f64_fmsubadd_vec4f64(v4f64, v4f64, v4f64);

	// Vector FNMADD
	// Calls: _mm256_fnamdd_pd
	v4f64 vec4f64_fnmadd_vec4f64(v4f64, v4f64, v4f64);

	// Vector FNMSUB
	// Calls: _mm256_fnmsub_pd
	v4f64 vec4f64_fnmsub_vec4f64(v4f64, v4f64, v4f64);

   // Vector HADD
   // Calls: _mm256_hadd_pd
	v4f64 vec4f64_hadd_vec4f64(v4f64, v4f64);

   // Vector HSUB
   // Calls: _mm256_hsub_pd
	v4f64 vec4f64_hsub_vec4f64(v4f64, v4f64);

   // Vector MAX
   // Calls: _mm256_max_pd
	v4f64 vec4f64_max_vec4f64(v4f64, v4f64);

   // Vector MIN
   // Calls: _mm256_min_pd
	v4f64 vec4f64_min_vec4f64(v4f64,v4f64);

   // Vector MOVEDUP
   // Calls: _mm256_movedup_pd
	v4f64 vec4f64_movedup_vec4f64(v4f64);

   // Vector MOVEMASK
   // Calls: _mm256_movemask_pd
	int vec4f64_movemask_vec4f64(v4f64);

	// Vector bitwise OR
	// Calls: _mm256_or_pd
	v4f64 vec4f64_or_vec4f64(v4f64, v4f64);

	// Vector PERMUTE -- 2 F128
	v4f64 vec4f64_permute2f128_vec4f64(v4f64, v4f64, const int);

    //
	// The same functionality implememted as void functions
	//

	// Vector addition
	// Calls: _mm256_add_pd
	void vec4f64_add_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector subtraction
	// Calls: _mm256_sub_pd
	void vec4f64_sub_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector multiplication
	// Calls: _mm256_mul_pd
	void vec4f64_mul_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector division
	// Calls: _mm256_div_pd
	void vec4f64_div_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector alternate addition-subtraction
	// Calls: _mm256_addsub_pd
	void vec4f64_addsub_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector bitwise AND
	// Calls: _mm256_and_pd
	void vec4f64_and_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector bitwise AND-NOT
	// Calls: _mm256_andnot_pf
	void vec4f64_andnot_pd(double * __restrict, double * __restrict, double * __restrict);

	// Vector blend
	// Calls: _mm256_blend_pd
	void vec4f64_blend_pd(double * __restrict, double * __restrict, double * __restrict, const int);

	// Vector blend
	// Calls: _mm256_blendv_pd
	void vec4f64_blendv_pd(double * __restrict, double * __restrict, double * __restrict, double * __restrict);

	// Vector ceil
	// Calls: _mm256_ceil_pd
	void vec4f64_ceil_pd(double * __restrict, double * __restrict);

	// Vector compare
	// Calls: _mm256_cmp_pd
	void vec4f64_cmp_pd(double * __restrict, double * __restrict, double * __restrict, const int);

	// Vector floor
	// Calls: _mm256_floor_pd
	void vec4f64_floor_pd(double * __restrict, double * __restrict);

	// Vector FMADD
	// Calls: _mm256_fmadd_pd
	void vec4f64_fmadd_pd(double * __restrict, double * __restrict, double * __restrict, double * __restrict);

	// Vector FMADDSUB
	// Calls: _mm256_fmaddsub_pd
	void vec4f64_fmaddsub_pd(double * __restrict, double * __restrict, double * __restrict, double * __restrict);

	// Vector FMSUB
	// Calls: _mm256_fmsub_pd
	void vec4f64_fmsub_pd(double * __restrict, double * __restrict, double * __restrict, double * __restrict);



} // extern "C" end of



#endif






#endif /*__SIMD_AVX_H__*/