

#ifndef __GMS_32F_EXP_32F_H__
#define __GMS_32F_EXP_32F_H__



/*
    Based on VOLK project.
    Modified by Bernard Gingold on:
    Date: 09-10-2021 13:34AM +00200
    contact: beniekg@gmail.com
    Few modification were added to original
    implementation (ICC pragmas, alignment directives and code layout rescheduled,
    unrolling completely 2-iteration for-loops)
    
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

 /* SIMD (SSE4) implementation of exp
   Inspired by Intel Approximate Math library, and based on the
   corresponding algorithms of the cephes math library
*/

/* Copyright (C) 2007  Julien Pommier

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  (this is the zlib license)
*/

#include <immintrin.h>
#include <stdint.h>


void
exp_u_ymm8r4_ymm8r4_looped(float * __restrict,
                           const float * __restrict,
			   const int32_t) __attribute__((noinline))
			                  __attribute__((hot))
				          __attribute__((aligned(32)));

void
exp_a_ymm8r4_ymm8r4_looped(float * __restrict,
                           const float * __restrict,
			   const int32_t) __attribute__((noinline))
			                  __attribute__((hot))
				          __attribute__((aligned(32)));


__m256
exp_ymm8r4_ymm8r4(const __m256)  __attribute__((noinline))
			         __attribute__((hot))
				 __attribute__((aligned(32)))
                                 __attribute__((vectorcall))
                                 __attribute__((regcall)); // This __attribute__((regcall)) is an Intel Compiler only!!








#endif /*__GMS_32F_EXP_32F_H__*/
