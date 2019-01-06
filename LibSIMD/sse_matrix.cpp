
#include <immintrin.h>

#include "sse_matrix.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846f
#endif

#if !defined( _mm_ror_ps)
#define _mm_ror_ps(vec,i)   (((i)%4) ? (_mm_shuffle_ps(vec,vec, _MM_SHUFFLE((unsigned char)(i+3)%4,(unsigned char)(i+2)%4,(unsigned char)(i+1)%4,(unsigned char)(i+0)%4))) : (vec))
#endif
#if !defined( _mm_rol_ps)
#define _mm_rol_ps(vec,i)   (((i)%4) ? (_mm_shuffle_ps(vec,vec, _MM_SHUFFLE((unsigned char)(7-i)%4,(unsigned char)(6-i)%4,(unsigned char)(5-i)%4,(unsigned char)(4-i)%4))) : (vec))
#endif

static const __declspec(align(16))  int mask_neg[4] = { 0x80000000, 0x80000000,
												   0x80000000, 0x80000000 };

static const __declspec(align(16))  int mask[4] = {0x00000000,0x80000000,0x80000000,0x00000000};

static const __declspec(align(16))  int mask8F[4] = {0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF};

static const __declspec(align(16)) float PIoY[4]  = {0.0f,M_PI*0.5f,M_PI*0.5f,-M_PI};
static const __declspec(align(16)) float PIoXZ[4] = {-M_PI,M_PI*0.5f,M_PI*0.5f,0.0f};

void warmup_fp128unit(volatile float * __restrict vc,
					  const float * __restrict vb,
					  const float * __restrict va,
					  const int n) {
	int i;
	for (i = 0; i != n; i += 4) {
		_mm_store_ps(&vc[i], _mm_add_ps(*(__m128*)&vb[i], *(__m128*)&va[i]));
	}
	i = 0;
	for (i; i != n; i += 4) {
		_mm_store_ps(&vc[i], _mm_mul_ps(*(__m128*)&vb[i], *(__m128*)&va[i]));
	}
	i = 0;
	for (i; i != n; i += 4) {
		_mm_store_ps(&vc[i], _mm_div_ps(*(__m128*)&vb[i], *(__m128*)&va[i]));
	}
}

void M4x4f32_setzero(struct M4x4f32 * __restrict m) {
	_mm_store_ps(&m->row0[0], _mm_setzero_ps());
	_mm_store_ps(&m->row1[0], _mm_setzero_ps());
	_mm_store_ps(&m->row2[0], _mm_setzero_ps());
	_mm_store_ps(&m->row3[0], _mm_setzero_ps());
}

void M4x4f32_setzero_stream(struct M4x4f32 * __restrict m) {
	_mm_stream_ps(&m->row0[0], _mm_setzero_ps());
	_mm_stream_ps(&m->row1[0], _mm_setzero_ps());
	_mm_stream_ps(&m->row2[0], _mm_setzero_ps());
	_mm_stream_ps(&m->row3[0], _mm_setzero_ps());
}

void M4x4f32_set1(struct M4x4f32 * __restrict m,
				  const float v0) {
	_mm_store_ps(&m->row0[0], _mm_set1_ps(v0));
	_mm_store_ps(&m->row1[0], _mm_set1_ps(v0));
	_mm_store_ps(&m->row2[0], _mm_set1_ps(v0));
	_mm_store_ps(&m->row3[0], _mm_set1_ps(v0));
}

void M4x4f32_set1_stream(struct M4x4f32 * __restrict m,
	const float v0) {
	_mm_stream_ps(&m->row0[0], _mm_set1_ps(v0));
	_mm_stream_ps(&m->row1[0], _mm_set1_ps(v0));
	_mm_stream_ps(&m->row2[0], _mm_set1_ps(v0));
	_mm_stream_ps(&m->row3[0], _mm_set1_ps(v0));
}

void M4x4f32_set_scalars(struct M4x4f32 * __restrict  m,
						 const float v0, const float  v1,
						 const float v2, const float  v3,
						 const float v4, const float  v5,
						 const float v6, const float  v7,
						 const float v8, const float  v9,
						 const float v10, const float v11,
						 const float v12, const float v13,
						 const float v14, const float v15) {
	_mm_store_ps(&m->row0[0], _mm_set_ps(v0,v1,v2,v3));
	_mm_store_ps(&m->row1[0], _mm_set_ps(v4,v5,v6,v7));
	_mm_store_ps(&m->row2[0], _mm_set_ps(v8,v9,v10,v11));
	_mm_store_ps(&m->row3[0], _mm_set_ps(v12,v13,v14,v15));
}

void M4x4f32_set_scalars_stream(struct M4x4f32 * __restrict  m,
								const float v0, const float  v1,
								const float v2, const float  v3,
								const float v4, const float  v5,
								const float v6, const float  v7,
								const float v8, const float  v9,
								const float v10, const float v11,
								const float v12, const float v13,
								const float v14, const float v15) {
	_mm_stream_ps(&m->row0[0], _mm_set_ps(v0, v1, v2, v3));
	_mm_stream_ps(&m->row1[0], _mm_set_ps(v4, v5, v6, v7));
	_mm_stream_ps(&m->row2[0], _mm_set_ps(v8, v9, v10, v11));
	_mm_stream_ps(&m->row3[0], _mm_set_ps(v12, v13, v14, v15));
}

void M4x4f32_set_arrays(struct M4x4f32 * __restrict m,
						const float * __restrict    a0,
						const float * __restrict    a1,
						const float * __restrict    a2,
						const float * __restrict    a3) {
	_mm_store_ps(&m->row0[0], _mm_load_ps(&a0[0]));
	_mm_store_ps(&m->row1[0], _mm_load_ps(&a1[0]));
	_mm_store_ps(&m->row2[0], _mm_load_ps(&a2[0]));
	_mm_store_ps(&m->row3[0], _mm_load_ps(&a3[0]));
}

void M4x4f32_set_arrays_stream(struct M4x4f32 * __restrict m,
							   const float * __restrict    a0,
							   const float * __restrict    a1,
							   const float * __restrict    a2,
							   const float * __restrict    a3) {
	_mm_stream_ps(&m->row0[0], _mm_load_ps(&a0[0]));
	_mm_stream_ps(&m->row1[0], _mm_load_ps(&a1[0]));
	_mm_stream_ps(&m->row2[0], _mm_load_ps(&a2[0]));
	_mm_stream_ps(&m->row3[0], _mm_load_ps(&a3[0]));
}

void M4x4f32_set_array(struct M4x4f32 * __restrict m,
					   const float * __restrict a) {
	_mm_store_ps(&m->row0[0], _mm_load_ps(&a[0]));
	_mm_store_ps(&m->row1[0], _mm_load_ps(&a[4]));
	_mm_store_ps(&m->row2[0], _mm_load_ps(&a[8]));
	_mm_store_ps(&m->row3[0], _mm_load_ps(&a[12]));
}

void M4x4f32_set_array_stream(struct M4x4f32 * __restrict m,
							  const float * __restrict a) {
	_mm_stream_ps(&m->row0[0], _mm_load_ps(&a[0]));
	_mm_stream_ps(&m->row1[0], _mm_load_ps(&a[4]));
	_mm_stream_ps(&m->row2[0], _mm_load_ps(&a[8]));
	_mm_stream_ps(&m->row3[0], _mm_load_ps(&a[12]));
}

void M4x4f32_set_M4x4f32(struct M4x4f32 * __restrict m,
						 const struct M4x4f32 * __restrict m1) {
	_mm_store_ps(&m->row0[0], _mm_load_ps(&m1->row0[0]));
	_mm_store_ps(&m->row1[0], _mm_load_ps(&m1->row1[0]));
	_mm_store_ps(&m->row2[0], _mm_load_ps(&m1->row2[0]));
	_mm_store_ps(&m->row3[0], _mm_load_ps(&m1->row3[0]));
}

void M4x4f32_set_M4x4f32_stream(struct M4x4f32 * __restrict m,
								const struct M4x4f32 * __restrict m1) {
	_mm_stream_ps(&m->row0[0], _mm_load_ps(&m1->row0[0]));
	_mm_stream_ps(&m->row1[0], _mm_load_ps(&m1->row1[0]));
	_mm_stream_ps(&m->row2[0], _mm_load_ps(&m1->row2[0]));
	_mm_stream_ps(&m->row3[0], _mm_load_ps(&m1->row3[0]));
}

void M4x4f32_identity_matrix(struct M4x4f32 * __restrict m) {
	_mm_store_ps(&m->row0[0], _mm_set_ps(1.0f,0.f,0.f,0.f));
	_mm_store_ps(&m->row1[0], _mm_set_ps(0.f,1.0f,0.f,0.f));
	_mm_store_ps(&m->row2[0], _mm_set_ps(0.f,0.f,1.f,0.f));
	_mm_store_ps(&m->row3[0], _mm_set_ps(0.f,0.f,0.f,1.f));
}

void M4x4f32_add_M4x4f32(struct M4x4f32 * __restrict mc,
						 const struct M4x4f32 * __restrict mb,
						 const struct M4x4f32 * __restrict ma) {
	_mm_store_ps(&mc->row0[0], _mm_add_ps(*(__m128*)&mb->row0[0],*(__m128*)&ma->row0[0]));
	_mm_store_ps(&mc->row1[0], _mm_add_ps(*(__m128*)&mb->row1[0],*(__m128*)&ma->row1[0]));
	_mm_store_ps(&mc->row2[0], _mm_add_ps(*(__m128*)&mb->row2[0],*(__m128*)&ma->row2[0]));
	_mm_store_ps(&mc->row3[0], _mm_add_ps(*(__m128*)&mb->row3[0],*(__m128*)&ma->row3[0]));
}

void M4x4f32_add_M4x4f32_inplace(struct M4x4f32 * __restrict mb,
								 const struct M4x4f32 * __restrict ma) {
	_mm_store_ps(&mb->row0[0], _mm_add_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0]));
	_mm_store_ps(&mb->row1[0], _mm_add_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0]));
	_mm_store_ps(&mb->row2[0], _mm_add_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0]));
	_mm_store_ps(&mb->row3[0], _mm_add_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0]));
}

void M4x4f32_sub_M4x4f32(struct M4x4f32 * __restrict mc,
						 const struct M4x4f32 * __restrict mb,
						 const struct M4x4f32 * __restrict ma) {
	_mm_store_ps(&mc->row0[0], _mm_sub_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0]));
	_mm_store_ps(&mc->row1[0], _mm_sub_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0]));
	_mm_store_ps(&mc->row2[0], _mm_sub_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0]));
	_mm_store_ps(&mc->row3[0], _mm_sub_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0]));
}

void M4x4f32_sub_M4x4f32_inplace(struct M4x4f32 * __restrict mb,
								 const struct M4x4f32 * __restrict ma) {
	_mm_store_ps(&mb->row0[0], _mm_sub_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0]));
	_mm_store_ps(&mb->row1[0], _mm_sub_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0]));
	_mm_store_ps(&mb->row2[0], _mm_sub_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0]));
	_mm_store_ps(&mb->row3[0], _mm_sub_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0]));
}

void M4x4f32_mul_M4x4f32(struct M4x4f32 * __restrict mc,
						 const struct M4x4f32 * __restrict mb,
						 const struct M4x4f32 * __restrict ma) {
	__m128 t1 = _mm_setzero_ps(), t2 = _mm_setzero_ps();
	t1 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0],0x00),
		 _mm_load_ps(&ma->row0[0]));
	t2 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0],0x00),
		_mm_load_ps(&ma->row0[0]));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0],*(__m128*)&ma->row0[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&ma->row0[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&ma->row1[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	_mm_store_ps(&mc->row0[0],t1);
	_mm_store_ps(&mc->row1[0],t2);
	t1 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t2 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&ma->row2[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&ma->row3[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	_mm_store_ps(&mc->row2[0], t1);
	_mm_store_ps(&mc->row3[0], t2);
}

void M4x4f32_mul_M4x4f32_inplace(struct M4x4f32 * __restrict mb,
								 const struct M4x4f32 * __restrict ma) {
	__m128 t1 = _mm_setzero_ps(), t2 = _mm_setzero_ps();
	t1 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&mb->row0[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t2 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&mb->row1[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&mb->row0[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&mb->row1[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&mb->row0[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&mb->row1[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row0[0], *(__m128*)&mb->row0[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row1[0], *(__m128*)&mb->row1[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	_mm_store_ps(&mb->row0[0],t1);
	_mm_store_ps(&mb->row1[0],t2);
	t1 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&mb->row2[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t2 = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&mb->row3[0], 0x00),
		_mm_load_ps(&ma->row0[0]));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&mb->row2[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&mb->row3[0], 0x55),
		_mm_load_ps(&ma->row1[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&mb->row2[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&mb->row3[0], 0xAA),
		_mm_load_ps(&ma->row2[0])));
	t1 = _mm_add_ps(t1, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row2[0], *(__m128*)&mb->row2[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	t2 = _mm_add_ps(t2, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&mb->row3[0], *(__m128*)&mb->row3[0], 0xFF),
		_mm_load_ps(&ma->row3[0])));
	_mm_store_ps(&mb->row2[0],t1);
	_mm_store_ps(&mb->row3[0],t2);
}

void M4x4f32_mul_scalar(struct M4x4f32 * __restrict mb,
						const M4x4f32 * __restrict ma,
						const float v0) {
	__m128 t = _mm_set1_ps(v0);
	_mm_store_ps(&mb->row0[0], _mm_mul_ps(*(__m128*)&ma->row0[0],t));
	_mm_store_ps(&mb->row1[0], _mm_mul_ps(*(__m128*)&ma->row1[0],t));
	_mm_store_ps(&mb->row2[0], _mm_mul_ps(*(__m128*)&ma->row2[0],t));
	_mm_store_ps(&mb->row3[0], _mm_mul_ps(*(__m128*)&ma->row3[0],t));
}

void M4x4f32_mul_scalar_inplace(struct M4x4f32 * __restrict ma,
							    const float v0) {
	__m128 t = _mm_set1_ps(v0);
	_mm_store_ps(&ma->row0[0], _mm_mul_ps(*(__m128*)&ma->row0[0], t));
	_mm_store_ps(&ma->row1[0], _mm_mul_ps(*(__m128*)&ma->row1[0], t));
	_mm_store_ps(&ma->row2[0], _mm_mul_ps(*(__m128*)&ma->row2[0], t));
	_mm_store_ps(&ma->row3[0], _mm_mul_ps(*(__m128*)&ma->row3[0], t));
}

void M4x4f32_negate(struct M4x4f32 * __restrict mb,
					const struct M4x4f32 * __restrict ma) {
	
	__m128 vmask = *(__m128*)&mask_neg[0];
	_mm_store_ps(&mb->row0[0], _mm_xor_ps(*(__m128*)&ma->row0[0],vmask));
	_mm_store_ps(&mb->row1[0], _mm_xor_ps(*(__m128*)&ma->row1[0],vmask));
	_mm_store_ps(&mb->row2[0], _mm_xor_ps(*(__m128*)&ma->row2[0],vmask));
	_mm_store_ps(&mb->row3[0], _mm_xor_ps(*(__m128*)&ma->row3[0],vmask));
}

void M4x4f32_transpose(struct M4x4f32 * __restrict m) {
	const __m128 t0 = _mm_unpacklo_ps(*(__m128*)&m->row0[0], *(__m128*)&m->row1[0]);
	const __m128 t1 = _mm_unpacklo_ps(*(__m128*)&m->row2[0], *(__m128*)&m->row3[0]);
	const __m128 t2 = _mm_unpackhi_ps(*(__m128*)&m->row0[0], *(__m128*)&m->row1[0]);
	const __m128 t3 = _mm_unpacklo_ps(*(__m128*)&m->row2[0], *(__m128*)&m->row3[0]);
	_mm_store_ps(&m->row0[0], _mm_movelh_ps(t0,t1));
	_mm_store_ps(&m->row1[0], _mm_movehl_ps(t1,t0));
	_mm_store_ps(&m->row2[0], _mm_movelh_ps(t2,t3));
	_mm_store_ps(&m->row3[0], _mm_movehl_ps(t3,t2));
}

void M4x4f32_rotateX(struct M4x4f32 * __restrict m,
					 const float rad) {
	 __m128 cs = _mm_add_ps(_mm_set1_ps(rad), *(__m128*)&PIoXZ[0]);
	 cs = _mm_sin_ps(cs);
	 m->row0[0] = 1.0f;
	 m->row3[3] = 1.0f;
	 _mm_storeh_pi((__m64*)&m->row1[1],cs);
	 _mm_storel_pi((__m64*)&m->row2[1],cs);
} 

void M4x4f32_rotateY(struct M4x4f32 * __restrict m,
					 const float rad) {
	__m128 cs = _mm_add_ps(_mm_set1_ps(rad), *(__m128*)&PIoY[0]);
	cs = _mm_sin_ps(cs);
	m->row1[1] = 1.0f;
	m->row3[3] = 1.0f;
	_mm_store_ps(&m->row0[0], _mm_unpackhi_ps(cs, _mm_setzero_ps()));
	_mm_store_ps(&m->row2[0], _mm_unpacklo_ps(cs, _mm_setzero_ps()));
}

void M4x4f32_rotateZ(struct M4x4f32 * __restrict m,
				     const float rad) {
	__m128 cs = _mm_add_ps(_mm_set1_ps(rad), *(__m128*)&PIoXZ[0]);
	cs = _mm_sin_ps(cs);
	m->row2[2] = 1.0f;
	m->row3[3] = 1.0f;
	_mm_storeh_pi((__m64*)&m->row0[0],cs);
	_mm_storel_pi((__m64*)&m->row1[0],cs);
}

void M4x4f32_translate_xyz(struct M4x4f32 * __restrict m,
						   const float x, const float y,
						   const float z) {
	M4x4f32_identity_matrix(m);
	mm_store_ps(&m->row3[0], _mm_set_ps(x,y,z,1.0f));
}

void M4x4f32_scale_xyz(struct M4x4f32 * __restrict m,
					   const float x, const float y,
					   const float z) {
	M4x4f32_identity_matrix(m);
	m->row0[0] = x;
	m->row1[1] = y;
	m->row2[2] = z;
}

void M4x4f32_scale_s(struct M4x4f32 * __restrict m,
					 const float s) {
	M4x4f32_identity_matrix(m);
	m->row0[0] = s;
	m->row1[1] = s;
	m->row2[2] = s;
}

void M4x4f32_det(float * det,
				 const struct M4x4f32 * __restrict m) {
	// Based on Zvi Devir (Intel MSL) implementation
	/*
		//
//   Copyright (c) 2001 Intel Corporation.
//
// Permition is granted to use, copy, distribute and prepare derivative works 
// of this library for any purpose and without fee, provided, that the above 
// copyright notice and this statement appear in all copies.  
// Intel makes no representations about the suitability of this software for 
// any purpose, and specifically disclaims all warranties. 
// See LEGAL.TXT for all the legal information.
//
	*/
	__m128 xmm0,xmm1,xmm2;
	__m128 v1,v2,v3,t1,t2,sum,tmp;
	t1 = *(__m128*)&m->row3[0];
	t2 = _mm_ror_ps(*(__m128*)&m->row2[0],1);
	xmm2 = _mm_mul_ps(t2, _mm_ror_ps(t1,0));
	xmm0 = _mm_mul_ps(t2, _mm_ror_ps(t1,2));
	xmm1 = _mm_mul_ps(t2, _mm_ror_ps(t1,3));
	v1 = _mm_sub_ps(_mm_ror_ps(xmm0, 1), _mm_ror_ps(xmm2,2));
	v2 = _mm_sub_ps(_mm_ror_ps(xmm1, 2), _mm_ror_ps(xmm1,0));
	v3 = _mm_sub_ps(_mm_ror_ps(xmm0, 0), _mm_ror_ps(xmm2,1));
	xmm0 = _mm_ror_ps(*(__m128*)&m->row1[0],1);
	sum = _mm_mul_ps(xmm0,v1);
	xmm1 = _mm_ror_ps(xmm0,1);
	sum = _mm_add_ps(sum, _mm_mul_ps(xmm1,v2));
	xmm2 = _mm_ror_ps(xmm1,1);
	sum = _mm_add_ps(sum, _mm_mul_ps(xmm2,v3));
	tmp = _mm_setzero_ps();
	tmp = _mm_mul_ps(sum, *(__m128*)&m->row0[0]);
	tmp = _mm_add_ps(tmp, _mm_movehl_ps(tmp,tmp));
	tmp = _mm_sub_ss(tmp, _mm_shuffle_ps(tmp,tmp,1));
	*det = tmp.m128_f32[0];
}

void M4x4f32_inverse(float * det,
					  const struct M4x4f32 * __restrict m) {
	// Based on Zvi Devir (Intel MSL) implementation
	
	//
	//   Copyright (c) 2001 Intel Corporation.
	//
	// Permition is granted to use, copy, distribute and prepare derivative works
	// of this library for any purpose and without fee, provided, that the above
	// copyright notice and this statement appear in all copies.
	// Intel makes no representations about the suitability of this software for
	// any purpose, and specifically disclaims all warranties.
	// See LEGAL.TXT for all the legal information.
	//
	__m128 A = _mm_movelh_ps(*(__m128*)&m->row0[0], *(__m128*)&m->row1[0]);
	__m128 B = _mm_movehl_ps(*(__m128*)&m->row1[0], *(__m128*)&m->row0[0]);
	__m128 C = _mm_movelh_ps(*(__m128*)&m->row2[0], *(__m128*)&m->row3[0]);
	__m128 D = _mm_movehl_ps(*(__m128*)&m->row3[0], *(__m128*)&m->row2[0]);
	__m128 iA,iB,iC,iD,DC,AB;
	__m128 dA,dB,dC,dD,vdet,d1,d2,rd,d;
	AB = _mm_mul_ps(_mm_shuffle_ps(A,A,0x0F),B);
	AB = _mm_sub_ps(AB, _mm_mul_ps(_mm_shuffle_ps(A, A, 0xA5), _mm_shuffle_ps(B,B,0x4E)));
	DC = _mm_mul_ps(_mm_shuffle_ps(D,D,0x0F), C);
	DC = _mm_sub_ps(DC, _mm_mul_ps(_mm_shuffle_ps(D, D, 0xA5), _mm_shuffle_ps(C,C,0x4E)));
	dA = _mm_mul_ps(_mm_shuffle_ps(A,A,0x5F),A);
	dA = _mm_sub_ss(dA, _mm_movehl_ps(dA,dA));
	dB = _mm_mul_ps(_mm_shuffle_ps(B,B,0x5F),B);
	dB = _mm_sub_ss(dB, _mm_movehl_ps(dB,dB));
	dC = _mm_mul_ps(_mm_shuffle_ps(C,C,0x5F),C);
	dC = _mm_sub_ss(dC, _mm_movehl_ps(dC,dC));
	dD = _mm_mul_ps(_mm_shuffle_ps(D,D,0x5F),D);
	dD = _mm_sub_ss(dD, _mm_movehl_ps(dD,dD));
	d = _mm_mul_ps(_mm_shuffle_ps(DC,DC,0xD8),AB);
	iD = _mm_mul_ps(_mm_shuffle_ps(C, C, 0xA0), _mm_movelh_ps(AB,AB));
	iD = _mm_add_ps(iD, _mm_mul_ps(_mm_shuffle_ps(C, C, 0xF5), _mm_movehl_ps(AB,AB)));
	iA = _mm_mul_ps(_mm_shuffle_ps(B, B, 0xA0), _mm_movelh_ps(DC,DC));
	iA = _mm_add_ps(iA, _mm_mul_ps(_mm_shuffle_ps(B, B, 0xF5), _mm_movehl_ps(DC,DC)));
	d = _mm_add_ps(d, _mm_movehl_ps(d,d));
	d = _mm_add_ss(d, _mm_shuffle_ps(d,d,1));
	d1 = _mm_mul_ps(dA,dD);
	d2 = _mm_mul_ps(dB,dC);
	iD = _mm_mul_ps(D, _mm_sub_ps(_mm_shuffle_ps(dA,dA,0),iD));
	iA = _mm_mul_ps(A, _mm_sub_ps(_mm_shuffle_ps(dD,dD,0),iA));
	vdet = _mm_sub_ps(_mm_add_ps(d1,d2),d);
	rd = _mm_rcp_ps(vdet);

#if defined (ZERO_SINGULAR)
	rd = _mm_and_ps(_mm_cmpneq_ss(vdet, _mm_setzero_ps()),rd);
#endif

	iB = _mm_mul_ps(D, _mm_shuffle_ps(AB,AB,0x33));
	iB = _mm_sub_ps(iB, _mm_mul_ps(_mm_shuffle_ps(D, D, 0xB1), _mm_shuffle_ps(AB,AB,0x66)));
	iC = _mm_mul_ps(A, _mm_shuffle_ps(DC,DC,0x33));
	iC = _mm_sub_ps(iC, _mm_mul_ps(_mm_shuffle_ps(A, A, 0xB1), _mm_shuffle_ps(DC,DC,0x66)));
	rd = _mm_shuffle_ps(rd,rd,0);
	rd = _mm_xor_ps(rd, *(__m128*)&mask[0]);
	iB = _mm_mul_ps(C, _mm_sub_ps(_mm_shuffle_ps(dB,dB,0),iB));
	iC = _mm_mul_ps(B, _mm_sub_ps(_mm_shuffle_ps(dC,dC,0),iC));
	iA = _mm_mul_ps(iA,rd);
	iB = _mm_mul_ps(iB,rd);
	iC = _mm_mul_ps(iC,rd);
	iD = _mm_mul_ps(iD,rd);
	_mm_store_ps(&m->row0[0], _mm_shuffle_ps(iA,iB,0x77));
	_mm_store_ps(&m->row1[0], _mm_shuffle_ps(iA,iB,0x22));
	_mm_store_ps(&m->row2[0], _mm_shuffle_ps(iC,iD,0x77));
	_mm_store_ps(&m->row3[0], _mm_shuffle_ps(iC,iD,0x22));
	*det = vdet.m128_f32[0];
}

void V1x4f32_setzero(struct V1x4f32 * __restrict vec) {
	_mm_store_ps(&vec->v[0], _mm_setzero_ps());
}

void V1x4f32_setzero_stream(struct V1x4f32 * __restrict vec) {
	_mm_stream_ps(&vec->v[0], _mm_setzero_ps());
}

void V1x4f32_set1(struct V1x4f32 * __restrict vec,
				  const float s) {
	_mm_store_ps(&vec->v[0], _mm_set1_ps(s));
}

void V1x4f32_set1_stream(struct V1x4f32 * __restrict vec,
						 const float s) {
	_mm_stream_ps(&vec->v[0], _mm_set1_ps(s));
}

void V1x4f32_set_scalars(struct V1x4f32 * __restrict vec,
						 const float s0, const float s1,
						 const float s2, const float s3) {
	_mm_store_ps(&vec->v[0], _mm_set_ps(s0,s1,s2,s3));
}

void V1x4f32_set_scalars_stream(struct V1x4f32 * __restrict vec,
								const float s0, const float s1,
								const float s2, const float s3) {
	_mm_stream_ps(&vec->v[0], _mm_set_ps(s0,s1,s2,s3));
}

void V1x4f32_set_array(struct V1x4f32 * __restrict vec,
					   const float * __restrict row) {
	_mm_store_ps(&vec->v[0], *(__m128*)&row[0]);
}

void V1x4f32_set_array_stream(struct V1x4f32 * __restrict vec,
							  const float * __restrict row) {
	_mm_stream_ps(&vec->v[0], *(__m128*)&row[0]);
}

void V1x4f32_set_V1x4f32(struct V1x4f32 * __restrict vec0,
						 const struct V1x4f32 * __restrict vec1) {
	_mm_store_ps(&vec0->v[0], *(__m128*)&vec1->v[0]);
}

void V1x4f32_set_V1x4f32_stream(struct V1x4f32 * __restrict vec0,
								const struct V1x4f32 * __restrict vec1) {
	_mm_stream_ps(&vec0->v[0], *(__m128*)&vec1->v[0]);
}

void V1x4f32_set_M4x4f32(struct V1x4f32 * __restrict vec,
						 const struct M4x4f32 * __restrict m,
						 const int imm) {
	switch (imm) {
			case 0: _mm_store_ps(&vec->v[0], *(__m128*)&m->row0[0]); break;
			case 1: _mm_store_ps(&vec->v[0], *(__m128*)&m->row1[0]); break;
			case 2: _mm_store_ps(&vec->v[0], *(__m128*)&m->row2[0]); break;
			case 3: _mm_store_ps(&vec->v[0], *(__m128*)&m->row3[0]); break;
			default: return;
	}
}

void V1x4f32_set_M4x4f32_stream(struct V1x4f32 * __restrict vec,
								const struct M4x4f32 * __restrict m,
								const int imm) {
	switch (imm) {
			case 0: _mm_stream_ps(&vec->v[0], *(__m128*)&m->row0[0]); break;
			case 1: _mm_stream_ps(&vec->v[0], *(__m128*)&m->row1[0]); break;
			case 2: _mm_stream_ps(&vec->v[0], *(__m128*)&m->row2[0]); break;
			case 3: _mm_stream_ps(&vec->v[0], *(__m128*)&m->row3[0]); break;
			default: return;
	}
}

void V1x4f32_mul_M4x4f32(struct V1x4f32 * __restrict vb,
						 struct V1x4f32 * __restrict va,
						 struct M4x4f32 * __restrict ma) {
	__m128 tmp = _mm_setzero_ps();
	tmp = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&va->v[0], 
									*(__m128*)&va->v[0], 0x00), *(__m128*)&ma->row0[0]);
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&va->v[0],
									                *(__m128*)&va->v[0], 0x55), *(__m128*)&ma->row1[0]));
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&va->v[0],
												    *(__m128*)&va->v[0], 0x55), *(__m128*)&ma->row2[0]));
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&va->v[0],
												    *(__m128*)&va->v[0], 0xFF), *(__m128*)&ma->row3[0]));
	_mm_store_ps(&vb->v[0], tmp);
}

void V1x4f32_mul_M4x4f32_inplace(struct V1x4f32 * __restrict vb,
							     const M4x4f32 * __restrict ma) {
	__m128 tmp = _mm_setzero_ps();
	tmp = _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&vb->v[0],
		*(__m128*)&vb->v[0], 0x00), *(__m128*)&ma->row0[0]);
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&vb->v[0],
		*(__m128*)&vb->v[0], 0x55), *(__m128*)&ma->row1[0]));
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&vb->v[0],
		*(__m128*)&vb->v[0], 0x55), *(__m128*)&ma->row2[0]));
	tmp = _mm_add_ps(tmp, _mm_mul_ps(_mm_shuffle_ps(*(__m128*)&vb->v[0],
		*(__m128*)&vb->v[0], 0xFF), *(__m128*)&ma->row3[0]));
	_mm_store_ps(&vb->v[0], tmp);
}

void V1x4f32_product(struct V1x4f32 * __restrict vc,
					 const struct V1x4f32 * __restrict vb,
					 const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vc->v[0], _mm_mul_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]));
}

void V1x4f32_product_inplace(struct V1x4f32 * __restrict vb,
							 const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vb->v[0], _mm_mul_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]));
}

void V1x4f32_mul_scalar(struct V1x4f32 * __restrict vc,
						const struct V1x4f32 * __restrict vb,
						const float s) {
	_mm_store_ps(&vc->v[0], _mm_mul_ps(*(__m128*)&vb->v[0], _mm_set1_ps(s)));
}

void V1x4f32_add_V1x4f32(struct V1x4f32 * __restrict vc,
						const struct V1x4f32 * __restrict vb,
						const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vc->v[0], _mm_add_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]));
}

void V1x4f32_add_V1x4f32_inplace(struct V1x4f32 * __restrict vb,
								 const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vb->v[0], _mm_add_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]));
}

void V1x4f32_sub_V1x4f32(struct V1x4f32 * __restrict vc,
						 const struct V1x4f32 * __restrict vb,
						 const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vc->v[0], _mm_sub_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]));
}

void V1x4f32_sub_V1x4f32_inplace(struct V1x4f32 * __restrict vb,
								 const struct V1x4f32 * __restrict va) {
	_mm_store_ps(&vb->v[0], _mm_sub_ps(*(__m128*)&vb->v[0],*(__m128*)&va->v[0]));
}

void V1x4f32_dot(float * dot,
				 const struct V1x4f32 * __restrict vb,
				 const struct V1x4f32 * __restrict va) {
	__m128 tmp = _mm_setzero_ps();
	tmp = _mm_mul_ps(*(__m128*)&vb->v[0], *(__m128*)&va->v[0]);
	tmp = _mm_add_ps(_mm_movehl_ps(tmp,tmp),tmp);
	*dot = _mm_add_ss(_mm_shuffle_ps(tmp, tmp, 1), tmp).m128_f32[0];
}

void V1x4f32_cross(struct V1x4f32 * __restrict vc,
				   const struct V1x4f32 * __restrict vb,
				   const struct V1x4f32 * __restrict va) {
	__m128 xmm0,xmm1,xmm2,xmm3;
	xmm0 = _mm_shuffle_ps(*(__m128*)&vb->v[0], *(__m128*)&vb->v[0], _MM_SHUFFLE(3,1,0,2));
	xmm1 = _mm_shuffle_ps(*(__m128*)&va->v[0], *(__m128*)&va->v[0], _MM_SHUFFLE(3,0,2,1));
	xmm3 = _mm_and_ps(_mm_mul_ps(xmm0, xmm1),  *(__m128*)&mask8F[0]);
	xmm0 = _mm_shuffle_ps(*(__m128*)&va->v[0], *(__m128*)&va->v[0],_MM_SHUFFLE(3,0,2,1));
	xmm1 = _mm_shuffle_ps(*(__m128*)&vb->v[0], *(__m128*)&vb->v[0], _MM_SHUFFLE(3,1,0,2));
	xmm2 = _mm_mul_ps(xmm0,xmm1);
	_mm_store_ps(&vc->v[0], _mm_sub_ps(xmm2,xmm3));
}





