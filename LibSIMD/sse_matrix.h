

#ifndef __SSE_MATRIX_H__
#define __SSE_MATRIX_H__ 301220181140




// Interoparable with coresponding Fortran derived type.
   
typedef struct __attribute__((aligned(16)))  M4x4f32 {
	    float row0[4];
	    float row1[4];
	    float row2[4];
	    float row3[4];
 }M4x4f32;

typedef struct __attribute__((aligned(16))) V1x4f32 {
		float v[4];
 }V1x4f32;

typedef struct __attribute__((aligned(16)))  V1x3f32 {
	    float v[4]; // Last element should be set either to 0 or to 1
 }V1x3f32;


 // void functions callable from Fortran.

/*void warmup_fp128unit(volatile float * __restrict ,
					  const float * __restrict ,
					  const float * __restrict ,
					  const int n );*/

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

void V1x4f32_setzero(struct V1x4f32 * __restrict);

void V1x4f32_setzero_stream(struct V1x4f32 * __restrict);

void V1x4f32_set1(struct V1x4f32 * __restrict,
				  const float);

void V1x4f32_set1_stream(struct V1x4f32 * __restrict,
					     const float);

void V1x4f32_set_scalars(struct V1x4f32 * __restrict,
					     const float, const float,
						 const float, const float);

void V1x4f32_set_scalars_stream(struct V1x4f32 * __restrict,
							    const float, const float,
								const float, const float);

void V1x4f32_set_array(struct V1x4f32 * __restrict,
					   const float * __restrict);

void V1x4f32_set_array_stream(struct V1x4f32 * __restrict,
							  const float * __restrict);

void V1x4f32_set_V1x4f32(struct V1x4f32 * __restrict,
					     const struct V1x4f32 * __restrict);

void V1x4f32_set_V1x4f32_stream(struct V1x4f32 * __restrict,
							    const struct V1x4f32 * __restrict);

void V1x4f32_set_M4x4f32(struct V1x4f32 * __restrict,
						 const struct M4x4f32 * __restrict,
						 const int);

void V1x4f32_set_M4x4f32_stream(struct V1x4f32 * __restrict,
								const struct M4x4f32 * __restrict,
								const int);

void V1x4f32_mul_M4x4f32(struct V1x4f32 * __restrict,
						 const struct V1x4f32 * __restrict ,
						 const struct M4x4f32 * __restrict);

void V1x4f32_mul_M4x4f32_inplace(struct V1x4f32 * __restrict,
								 const struct M4x4f32 * __restrict);

void V1x4f32_product(struct V1x4f32 * __restrict,
					 const struct V1x4f32 * __restrict,
					 const struct V1x4f32 * __restrict);

void V1x4f32_product_inplace(struct V1x4f32 * __restrict,
							 const struct V1x4f32 * __restrict);

void V1x4f32_mul_scalar(struct V1x4f32 * __restrict ,
					    const struct V1x4f32 * __restrict,
						const float);

void V1x4f32_add_V1x4f32(struct V1x4f32 * __restrict,
						 const struct V1x4f32 * __restrict,
						 const struct V1x4f32 * __restrict);

void V1x4f32_add_V1x4f32_inplace(struct V1x4f32 * __restrict,
								 const struct V1x4f32 * __restrict);

void V1x4f32_sub_V1x4f32(struct V1x4f32 * __restrict,
						 const struct V1x4f32 * __restrict,
						 const struct V1x4f32 * __restrict);

void V1x4f32_sub_V1x4f32_inplace(struct V1x4f32 * __restrict,
							     const struct V1x4f32 * __restrict);

void V1x4f32_dot(float * ,
				 const struct V1x4f32 * __restrict,
				 const struct V1x4f32 * __restrict);

void V1x4f32_cross(struct V1x4f32 * __restrict,
				   const struct V1x4f32 * __restrict,
				   const struct V1x4f32 * __restrict);

//void V1x4f32_normalize(struct V1x4f32 * __restrict);




#endif /*__SSE_MATRIX_H__*/
