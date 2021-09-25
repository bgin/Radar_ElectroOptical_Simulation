
#ifndef __GMS_32F_ACOS_32F_AVX512_H__
#define __GMS_32F_ACOS_32F_AVX512_H__ 25092021120


/*
    Based on VOLK project.
    Modified by Bernard Gingold on:
    Date: 25-09-2021 1:20PM +00200
    contact: beniekg@gmail.com
    Few modification were added to original
    implementation (ICC pragmas, alignment directives and code layout rescheduled,
    unrolling completely 2-iteration for-loops)
    Version converted to AVX512F.
    
*/

/*
 * Copyright 2018 Free Software Foundation, Inc.
 *
 * This file is part of GNU Radio
 *
 * GNU Radio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU Radio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Radio; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street,
 * Boston, MA 02110-1301, USA.
 */

#include <immintrin.h>
#include <stdint.h>


void
acos_u_zmm16r4_zmm16r4_looped(float * __restrict,
			      float * __restrict,
			      const int32_t) __attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((aligned(32)));

void
acos_a_zmm16r4_zmm16r4_looped(float * __restrict,
			      float * __restrict,
			      const int32_t) __attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((aligned(32)));


__m512
acos_zmm16r4_zmm16r4(const __m512) __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((aligned(32)))
                                   __attribute__((vectorcall))
                                   __attribute__((regcall)); // This __attribute__((regcall)) is an Intel Compiler only!!








#endif  /*__GMS_32F_ACOS_32F_AVX512_H__*/
