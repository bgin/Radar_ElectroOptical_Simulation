

#ifndef __SIMD_AVX512_H__
#define __SIMD_AVX512_H__


const unsigned int gGMS_SIMD_AVX512_MAJOR = 1;
const unsigned int gGMS_SIMD_AVX512_MINOR = 0;
const unsigned int gGMS_SIMD_AVX512_MICRO = 0;
const unsigned int gGMS_SIMD_AVX512_FULLVER =
  1000U*gGMS_SIMD_AVX512_MAJOR+100U*gGMS_SIMD_AVX512_MINOR+10U*gGMS_SIMD_AVX512_MICRO;
const char * const pgGMS_SIMD_AVX512_CREATE_DATE = "10-10-2019 11:39 +00200 (SUN 10 NOV 2019 GMT+2)";
const char * const pgGMS_SIMD_AVX512_BUILD_DATE_TIME  = __DATE__" " __TIME__;
const char * const pgGMS_SIMD_AVX512_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
const char * const pgGMS_SIMD_AVX512_SYNOPSIS = "Callable from Fortran C wrappers for AVX512F Intrinsics.";

// Compile it as a C-code

    // Interoperable with corresponding Fortran derived type
#if defined _WIN64
        typedef struct __declspec(align(64)) v8f64 {
                 double v[8];
	}v8f64;

	typedef struct __declspec(align(64)) v16f32 {
                 double v[16];
	}v16f32;
#elif defined __linux
        typedef struct __attribute__((align(64))) v8f64 {
                 double v[8];
        }v8f64;

        typedef struct __attribute__((align(64))) v16f32 {
                 double v[16];
	}v16f32;
#else
#error  Unsupported OS (Linux and Win64 are supported only!!)
#endif

        v8f64 v8f64_add_pd(v8f64 ,v8f64 );

	v8f64 v8f64_sub_pd(v8f64 ,v8f64 );

	v8f64 v8f64_mul_pd(v8f64 ,v8f64 );

	v8f64 v8f64_div_pd(v8f64 ,v8f64 );

	v8f64 v8f64_mask_ceil_pd(v8f64 ,unsigned char ,v8f64 );

	v8f64 v8f64_ceil_pd(v8f64 );

	v8f64 v8f64_abs_pd(v8f64 );

	v8f64 v8f64_mask_abs_pd(v8f64 ,unsigned char ,v8f64 );

	v8f64 v8f64_mask_add_pd(v8f64 ,unsigned char ,v8f64 ,v8f64 );

	v8f64 v8f64_maskz_add_pd(unsigned char ,v8f64 ,v8f64 );

	v8f64 v8f64_add_round_pd(v8f64 ,v8f64 ,int);

	v8f64 v8f64_mask_add_round_pd(v8f64 ,unsigned char ,v8f64 ,v8f64 ,int );

	v8f64 v8f64_maskz_add_round_pd(unsigned char ,v8f64 ,v8f64 ,int );

	v8f64 v8f64_mask_div_pd(v8f64 ,unsigned char ,v8f64 ,v8f64 );

	v8f64 v8f64_maskz_div_pd(unsigned char, v8f64, v8f64);

	v8f64 v8f64_div_round_pd(v8f64, v8f64, int);

	v8f64 v8f64_mask_div_round_pd(v8f64, unsigned char, v8f64, v8f64,int);

	v8f64 v8f64_maskz_div_round_pd(unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_fmadd_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fmadd_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fmadd_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fmadd_pd(unsigned char, v8f64, v8f64, v8f64);

	v8f64 v8f64_fmadd_round_pd(v8f64, v8f64, v8f64, int);

	v8f64 v8f64_mask_fmadd_round_pd(v8f64, unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_mask3_fmadd_round_pd(v8f64, v8f64, v8f64, unsigned char, int);

	v8f64 v8f64_maskz_fmadd_round_pd(unsigned char, v8f64, v8f64, v8f64, int);

	v8f64 v8f64_fmaddsub_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fmaddsub_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fmaddsub_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fmaddsub_pd(unsigned char, v8f64, v8f64, v8f64);

	v8f64 v8f64_fmaddsub_round_pd(v8f64, v8f64, v8f64, int);

	v8f64 v8f64_mask_fmaddsub_round_pd(v8f64, unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_mask3_fmaddsub_round_pd(v8f64, v8f64, v8f64, unsigned char, int);

	v8f64 v8f64_maskz_fmaddsub_round_pd(unsigned char, v8f64, v8f64, v8f64, int);

	v8f64 v8f64_fmsub_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fmsub_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fmsub_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fmsub_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_fmsubadd_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fmsubadd_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fmsubadd_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fmusbadd_pd(unsigned char, v8f64, v8f64, v8f64);

	v8f64 v8f64_fmsubadd_round_pd(v8f64, v8f64, v8f64, int);

	v8f64 v8f64_mask_fmsubadd_round_pd(v8f64, unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_mask3_fmsubbadd_round_pd(v8f64, v8f64, v8f64, unsigned char, int);

	v8f64 v8f64_maskz_fmsubadd_round_pd(unsigned char, v8f64, v8f64, v8f64, int);

	v8f64 v8f64_fnmadd_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fnmadd_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fnmadd_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fnmadd_pd(unsigned char, v8f64, v8f64, v8f64);

        v8f64 v8f64_fnmadd_round_pd(v8f64, v8f64, v8f64, int);

	v8f64 v8f64_mask_fnmadd_round_pd(v8f64, unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_mask3_fnmadd_round_pd(v8f64, v8f64, v8f64, unsigned char, int);

	v8f64 v8f64_maskz_fnmadd_round_pd(unsigned char, v8f64, v8f64, v8f64, int);

	v8f64 v8f64_fnmsub_pd(v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_fnmsub_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask3_fnmsub_pd(v8f64, v8f64, v8f64, unsigned char);

	v8f64 v8f64_maskz_fnmsub_pd(unsigned char, v8f64, v8f64, v8f64);

	v8f64 v8f64_mask_mul_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_maskz_mul_pd(unsigned char, v8f64, v8f64);

	v8f64 v8f64_mask_mul_round_pd(v8f64, unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_maskz_mul_round_pd(unsigned char, v8f64, v8f64, int);

	v8f64 v8f64_mask_reduce_add_pd(unsigned char, v8f64);

	v8f64 v8f64_reduce_add_pd(v8f64);

	v8f64 v8f64_mask_reduce_mul_pd(unsigned char, v8f64);

	v8f64 v8f64_reduce_mul_pd(v8f64);

	v8f64 v8f64_mask_sub_pd(v8f64, unsigned char, v8f64, v8f64);

	v8f64 v8f64_maskz_sub_pd(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmp_pd_mask(v8f64, v8f64, int);

	unsigned char  v8f64_mask_cmp_pd_mask(unsigned char, v8f64, v8f64, int);

	unsigned char  v8f64_cmp_round_pd_mask(v8f64, v8f64, int, int);

	unsigned char  v8f64_mask_cmp_round_pd_mask(unsigned char, v8f64, v8f64, int, int);

	unsigned char  v8f64_cmpeq_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpeq_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmpunord_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpunord_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmpord_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpord_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmpnlt_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpnlt_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmpnle_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpnle_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmpneq_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmpneq_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmplt_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmplt_pd_mask(unsigned char, v8f64, v8f64);

	unsigned char  v8f64_cmple_pd_mask(v8f64, v8f64);

	unsigned char  v8f64_mask_cmple_pd_mask(unsigned char, v8f64, v8f64);

	v8f64  v8f64_mask_rcp14_pd(v8f64, unsigned char, v8f64);

	v8f64  v8f64_maskz_rcp14_pd(unsigned char, v8f64);

	v8f64  v8f64_rcp14_pd(v8f64);

	v8f64  v8f64_mask_rsqrt14_pd(v8f64, unsigned char, v8f64);

	v8f64  v8f64_maskz_rsqrt14_pd(unsigned char, v8f64);

	v8f64  v8f64_rsqrt14_pd(v8f64);

	v8f64  v8f64_mask_sqrt_pd(v8f64, unsigned char, v8f64);

	v8f64  v8f64_maskz_sqrt_pd(unsigned char, v8f64);

	v8f64  v8f64_mask_sqrt_round_pd(v8f64, unsigned char, v8f64, int);

	v8f64  v8f64_maskz_sqrt_round_pd(unsigned char, v8f64, int);

	v8f64  v8f64_mask_expandloadu_pd(v8f64, unsigned char, void *);

	v8f64  v8f64_maskz_expandload_pd(unsigned char, void *);

	v8f64  v8f64_mask_load_pd(v8f64, unsigned char, void *);

	v8f64  v8f64_maskz_load_pd(unsigned char, void *);

	v8f64  v8f64_mask_loadu_pd(v8f64, unsigned char, void *);

	v8f64  v8f64_maskz_loadu_pd(unsigned char, void * );

	v8f64  v8f64_getexp_pd(v8f64);

	v8f64  v8f64_mask_getexp_pd(v8f64, unsigned char, v8f64);

	v8f64  v8f64_maskz_getexp_pd(unsigned char, v8f64);

	v8f64  v8f64_getexp_round_pd(v8f64, int);

	v8f64  v8f64_mask_getexp_round_pd(v8f64, unsigned char, v8f64, int);

	v8f64  v8f64_maskz_getexp_round_pd(unsigned char, v8f64, int);

	v8f64  v8f64_getmant_pd(v8f64, int, int);

	v8f64  v8f64_mask_getmant_pd(v8f64, unsigned char, v8f64, int, int);

	v8f64  v8f64_maskz_getmant_pd(unsigned char, v8f64, int, int);

	v8f64  v8f64_getmant_round_pd(v8f64, int, int, int);

	v8f64  v8f64_mask_getmant_round_pd(v8f64, unsigned char, v8f64, int, int, int);

	v8f64  v8f64_maskz_getmant_round_pd(unsigned char, v8f64, int, int, int);

	v8f64  v8f64_mask_roundscale_pd(v8f64, unsigned char, v8f64, int);

	


#endif /*__SIMD_AVX512_H__*/
