

#ifndef __GMS_RCS_COMMON_ZMM16R4_H__
#define __GMS_RCS_COMMON_ZMM16R4_H__

/*MIT License
Copyright (c) 2020 Bernard Gingold
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/



    const unsigned int GMS_RCS_COMMON_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_COMMON_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_COMMON_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_COMMON_ZMM16R4_FULLVER =
      1000U*GMS_RCS_COMMON_ZMM16R4_MAJOR+
      100U*GMS_RCS_COMMON_ZMM16R4_MINOR+
      10U*GMS_RCS_COMMON_ZMM16R4_MICRO;
    const char * const GMS_RCS_COMMON_ZMM16R4_CREATION_DATE = "04-01-2023 10:12 AM +00200 (WED 04 01 2023 GMT+2)";
    const char * const GMS_RCS_COMMON_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_COMMON_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_COMMON_ZMM16R4_DESCRIPTION   = "AVX512 optimized Radar Cross Section common functions."


#include <cstdint>
#include <immintrin.h>


              /*
                        Complex wavenumber, vector of 16 varying values of
                        complex permeability and permitivity.
                    */


                    void k_zmm16r4(const __m512 mur,
                                   const __m512 mui
                                   const __m512 epsr,
                                   const __m512 epsi,
                                   const __m512 om,
                                   __m512 * __restrict kr,
                                   __m512 * __restrict ki)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));



























#endif /*__GMS_RCS_COMMON_ZMM16R4_H__*/
