

#ifndef __GMS_SWAP_AVX_UNROLLED16X_H__
#define __GMS_SWAP_AVX_UNROLLED16X_H__


/*
     This version is based on BLIS framework original implementation.
     Few changes were introduced in order to more easily adapt it 
     to 'Guided Missile Simulation' project.
     //=========================================================================
     // Modified and optimized version of BLIS (AMD) swap routines.
     // Programmer: Bernard Gingold, contact: beniekg@gmail.com
     // 26-08-2021 10:39 AM +00200
     // Original copyright below
     //========================================================================
     
   The original authors license
   BLIS
   An object-based framework for developing high-performance BLAS-like
   libraries.
   Copyright (C) 2016 - 2019, Advanced Micro Devices, Inc.
   Copyright (C) 2018 - 2020, The University of Texas at Austin. All rights reserved.
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:
    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    - Neither the name(s) of the copyright holder(s) nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
     
*/

 // This code must be compiled as C code.

#include <stdint.h>


void
sswap_u_ymm8r4_unroll16x(const int32_t,
                         float * __result,
			 const int32_t,
			 float * __result,
			 const int32_t) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((aligned(32)));

void
sswap_a_ymm8r4_unroll16x(const int32_t,
                         float * __result,
			 const int32_t,
			 float * __result,
			 const int32_t) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((aligned(32)));


void
sswap_a_ymm8r4_unroll16x_omp(const int32_t,
                             float * __result,
			     const int32_t,
			     float * __result,
			     const int32_t) __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((aligned(32)));


void
dswap_u_ymm4r8_unroll16x(const int32_t,
                         double * __result,
			 const int32_t,
			 double * __result,
			 const int32_t)     __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((aligned(32)));


void
dswap_a_ymm4r8_unroll16x(const int32_t,
                         double * __result,
			 const int32_t,
			 double * __result,
			 const int32_t)     __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((aligned(32)));



void
dswap_a_ymm4r8_unroll16x_omp(const int32_t,
                             double * __result,
			     const int32_t,
			     double * __result,
			     const int32_t) __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((aligned(32)));
					    


					    


















#endif /*__GMS_SWAP_AVX_UNROLLED16X_H__*/
