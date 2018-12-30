

#ifndef __SSE_MATRIX_H__
#define __SSE_MATRIX_H__


#if !defined (SSE_MATRIX_MAJOR)
#define SSE_MATRIX_MAJOR 1
#endif

#if !defined (SSE_MATRIX_MINOR)
#define SSE_MATRIX_MINOR 0
#endif

#if !defined (SSE_MATRIX_MICRO)
#define SSE_MATRIX_MICRO 0
#endif

#if !defined (SSE_MATRIX_FULLVER) 
#define SSE_MATRIX_FULLVER 1000*(SSE_MATRIX_MAJOR)+100*(SSE_MATRIX_MINOR)+10*(SSE_MATRIX_MICRO)
#endif

#if !defined (SSE_MATRIX_CREATE_DATE)
#define SSE_MATRIX_CREATE_DATE  "30-12-2018 11:40 + 00200 (SUN 30 DEC 2018 GMT+2)"
#endif

#if !defined (SSE_MATRIX_BUILD_DATE)
#define SSE_MATRIX_BUILD_DATE "00-00-0000 00:00"
#endif

#if !defined (SSE_MATRIX_AUTHOR)
#define SSE_MATRIX_AUTHOR "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
#endif

#if !defined (SSE_MATRIX_SYNOPSIS)
#define SSE_MATRIX_SYNOPSIS "SSE accelerated Matrix-Matrix, Matrix-Vector operations -- Fortran callable."
#endif

// Interoparable with coresponding Fortran derived type.
   
typedef struct __declspec(align(16))  M4x4f32 {
	    float row0[4];
	    float row1[4];
	    float row2[4];
	    float row3[4];
 }M4x4f32;

typedef struct __declspec(align(16))  V1x4f32 {
		float v[4];
 }V1x4f32;

typedef struct __declspec(align(16))  V1x3f32 {
	    float v[4]; // Last element should be set either to 0 or to 1
 }V1x3f32;


 // void functions callable from Fortran.

void M4x4f32_setzero(struct M4x4f32 * __restrict);

void M4x4f32_setzero_stream(struct M4x4f32 * __restrict);

void M4x4f32_set1(struct M4x4f32 * __restrict,
				  const float);

void M4x4f32_set1_stream(struct M4x4f32 * __restrict,
					     const float);

void M4x4f32_set_scalars(struct M4x4f32 * __restrict, 
							  const float, const float,
							  const float, const float,
							  const float, const float,
							  const float, const float,
							  const float, const float,
							  const float, const float,
							  const float, const float,
							  const float, const float );

void M4x4f32_set_scalars_stream(struct M4x4f32 * __restrict,
								const float, const float,
								const float, const float,
								const float, const float,
								const float, const float,
								const float, const float,
								const float, const float,
								const float, const float,
								const float, const float);

void M4x4f32_set_arrays(struct M4x4f32 * __restrict,
						     const float * __restrict,
							 const float * __restrict,
							 const float * __restrict,
							 const float * __restrict);

void M4x4f32_set_arrays_stream(struct M4x4f32 * __restrict,
							   const float * __restrict,
							   const float * __restrict,
							   const float * __restrict,
							   const float * __restrict);

void M4x4f32_set_array(struct M4x4f32 * __restrict,
							const float * __restrict);

void M4x4f32_set_array_stream(struct M4x4f32 * __restrict,
							  const float * __restrict);

void M4x4f32_set_M4x4f32(struct M4x4f32 * __restrict,
							  const struct M4x4f32 * __restrict);

void M4x4f32_set_M4x4f32_stream(struct M4x4f32 * __restrict,
							    const struct M4x4f32 * __restrict);

void M4x4f32_identity_matrix(struct M4x4f32 * __restrict);


void M4x4f32_add_M4x4f32(struct M4x4f32 * __restrict,
				         const struct M4x4f32 *  __restrict,
						 const struct M4x4f32 * __restrict);

void M4x4f32_add_M4x4f32_inplace(struct M4x4f32 * __restrict,
							     const struct M4x4f32 * __restrict);

void M4x4f32_sub_M4x4f32(struct M4x4f32 * __restrict,
						 const struct M4x4f32 * __restrict,
						 const struct M4x4f32 * __restrict);

void M4x4f32_sub_M4x4f32_inplace(struct M4x4f32 * __restrict,
							     const struct M4x4f32 * __restrict);
	
	// Matrix 4x4 - mul - Matrix 4x4
void M4x4f32_mul_M4x4f32(struct M4x4f32 * __restrict , 
						 const struct M4x4f32 * __restrict , 
						 const struct M4x4f32 * __restrict );

void M4x4f32_mul_M4x4f32_inplace(struct M4x4f32 * __restrict,
								 const struct M4x4f32 * __restrict);

void M4x4f32_mul_scalar(struct M4x4f32 * __restrict,
					    const struct M4x4f32 * __restrict,
						const float);

void M4x4f32_mul_scalar_inplace(struct M4x4f32 * __restrict,
							    const float);

void M4x4f32_negate(struct M4x4f32 * __restrict,
				    const struct M4x4f32 * __restrict);

void M4x4f32_transpose(struct M4x4f32 * __restrict);

void M4x4f32_rotateX(struct M4x4f32 * __restrict,
					 const float);

void M4x4f32_rotateY(struct M4x4f32 * __restrict,
					 const float);

void M4x4f32_rotateZ(struct M4x4f32 * __restrict,
					 const float);

void M4x4f32_translate_xyz(struct M4x4f32 * __restrict,
					   const float, const float,
					   const float);

void M4x4f32_scale_xyz(struct M4x4f32 * __restrict,
				   const float, const float,
				   const float);

void M4x4f32_scale_s(struct M4x4f32 * __restrict,
					 const float);

void M4x4f32_minimum(float * , const struct M4x4f32 * __restrict);

void M4x4f32_maximum(float * , const struct M4x4f32 * __restrict);

void M4x4f32_det(float * , const struct M4x4f32 * __restrict);

void M4x4f32_inverse(float * , const struct M4x4f32 * __restrict);



#endif /*__SSE_MATRIX_H__*/