

#ifndef __GMS_MAT8X8_DET_AVX_HPP__
#define __GMS_MAT8X8_DET_AVX_HPP__



void
mat8x8_det_a_ymm8r4(const float * __restrict,
                    float * __restrict)
		                         __attribute__((noinline))
					 __attribute__((hot))
					 __attribute__((aligned(32)));


void
mat8x8_det_u_ymm8r4(const float * __restrict,
                    float * __restrict)
		                         __attribute__((noinline))
					 __attribute__((hot))
					 __attribute__((aligned(32)));












#endif /* __GMS_MAT8X8_DET_AVX_HPP__*/
