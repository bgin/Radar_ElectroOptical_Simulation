

#ifndef __GMS_ZGEMV_T_H__
#define __GMS_ZGEMV_T_H__


//
// Version callable from the Fortran interface
//
//=========================================================================
// Modified and optimized version of OpenBLAS zgemv_t and kernels zgemv_kernel_4x4,
// zgemv_kernel_4x2, zgemv_kernel_4x1
// Programmer: Bernard Gingold, contact: beniekg@gmail.com
// 20-09-2020 3:22 PM +00200
// Original copyright below
//========================================================================
/***************************************************************************
Copyright (c) 2014, The OpenBLAS Project
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in
the documentation and/or other materials provided with the
distribution.
3. Neither the name of the OpenBLAS project nor the names of
its contributors may be used to endorse or promote products
derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE OPENBLAS PROJECT OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/
#include <stdint.h>


int32_t zgemv_t(const int32_t,
		const int32_t,
		const double,
		const double,
		double * __restrict,
		int32_t,
		double * __restrict,
		int32_t,
		double * __restrict,
		int32_t,
		double * __restrict) __attribute__((aligned(16))) __attribute__((hot)) __attribute__((noinline));


void zgemv_t_kernel_4x4(const int32_t,
			double ** __restrict,
			double  * __restrict,
			double  * __restrict,
			double  * __restrict) __attribute__((aligned(32))) __attribute__((hot)) __attribute((noinline));


void zgemv_t_kernel_4x2(const int32_t,
			double ** __restrict,
			double  * __restrict,
			double  * __restrict,
			double  * __restrict) __attribute__((aligned(32))) __attribute__((hot)) __attribute__((noinline));


void zgemv_t_kernel_4x1(const int32_t,
			double ** __restrict,
			double  * __restrict,
			double  * __restrict,
			double  * __restrict) __attribute__((aligned(32))) 








#endif /*__GMS_ZGEMV_T_H__*/
