
#include <immintrin.h>
#include "sse_matrix.h"

static const __declspec(align(16)) mask_neg[4] = { 0x80000000, 0x80000000,
												   0x80000000, 0x80000000 };

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

