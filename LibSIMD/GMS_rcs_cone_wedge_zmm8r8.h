

#ifndef __GMS_RCS_CONE_WEDGE_ZMM8R8_H__
#define __GMS_RCS_CONE_WEDGE_ZMM8R8_H__ 130320231230


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



    const unsigned int GMS_RCS_CONE_WEDGE_ZMM8R8_MAJOR = 1U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM8R8_MINOR = 0U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM8R8_MICRO = 0U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM8R8_FULLVER =
      1000U*GMS_RCS_CONE_WEDGE_ZMM8R8_MAJOR+
      100U*GMS_RCS_CONE_WEDGE_ZMM8R8_MINOR+
      10U*GMS_RCS_CONE_WEDGE_ZMM8R8_MICRO;
    const char * const GMS_RCS_CONE_WEDGE_ZMM8R8_CREATION_DATE = "13-03-2023 12:30 PM +00200 (MON 13 MAR 2023 GMT+2)";
    const char * const GMS_RCS_CONE_WEDGE_ZMM8R8_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_CONE_WEDGE_ZMM8R8_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_CONE_WEDGE_ZMM8R8_DESCRIPTION   = "AVX512 optimized Cones and Wedges Radar Cross Section (analytic) functionality.";



#include <cstdint>
#include <stdbool.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"




                   /*
                       Small-angle cone (alpha ~ 0).
                       Backscattered RCS.
                       Formula 6.2-12
                   */


                 
                   __m512d rcs_f6212_zmm8r8(const __m512d gam0,
                                            const __m512d alp) 
                                                         FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f6212_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) pgam0,
                                              const double * __restrict __attribute__((aligned(64))) palp) 
                                                         FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f6212_zmm8r8_u(const double * __restrict  pgam0,
                                              const double * __restrict  palp)
                                                         FUNC_ATTRIBUTES;


                     /*
                           Small-angle cone (alpha ~ pi/2).
                           Backscattered RCS.
                           Formula 6.2-13
                       */

                 
                   __m512d rcs_f6213_zmm8r8(const __m512d gam0,
                                            const __m512d alp) 
                                                          FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f6213_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) pgam0,
                                              const double * __restrict __attribute__((aligned(64))) palp) 
                                                          FUNC_ATTRIBUTES;
 

                  
                   __m512d rcs_f6213_zmm8r8_u(const double * __restrict  pgam0,
                                              const double * __restrict  palp) 
                                                          FUNC_ATTRIBUTES;
 

                   /*
                         Backscattering case.
                         E-field scattered for (phi component).
                         Formula 6.2-16
    
                     */


                 
                   void ESph_f6216_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) 
                                                        FUNC_ATTRIBUTES;

               
                   void ESph_f6216_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                FUNC_ATTRIBUTES;

                 
                   void ESph_f6216_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pr,
                                           const double * __restrict  palp,
                                           const double * __restrict  ptht,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi)
                                                                   FUNC_ATTRIBUTES;

                   /*
                         Bistatic RCS case.
                         E-field scattered for (theta component).
                         Formula 6.2-14
                    */


                
                   void ESth_f6214_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht1, //inc
                                           const __m512d tht2  //scat
                                           const __m512d phi1, //inc
                                           const __m512d phi2, //scat
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) 
                                                           FUNC_ATTRIBUTES;

                 
                   void ESth_f6214_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64) ESi) 
                                                             FUNC_ATTRIBUTES;

                
                   void ESth_f6214_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pr,
                                           const double * __restrict  palp,
                                           const double * __restrict  ptht1,
                                           const double * __restrict  ptht2,
                                           const double * __restrict  pphi1,
                                           const double * __restrict  pphi2,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) 
                                                           FUNC_ATTRIBUTES;
                    /*
                           Bistatic RCS case.
                           E-field scattered for (phi component).
                           Formula 6.2-15
                      */


                 
                   void ESph_f6215_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht1, //inc
                                           const __m512d tht2  //scat
                                           const __m512d phi1, //inc
                                           const __m512d phi2, //scat
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) 
                                                           FUNC_ATTRIBUTES;

                 
                   void ESph_f6215_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                  FUNC_ATTRIBUTES;
                 
                   void ESph_f6215_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pr,
                                           const double * __restrict  palp,
                                           const double * __restrict  ptht1,
                                           const double * __restrict  ptht2,
                                           const double * __restrict  pphi1,
                                           const double * __restrict  pphi2,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) 
                                                                  FUNC_ATTRIBUTES;
                  /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (theta component).
                           Formula 6.2-17
                    */



                   void ESth_f6217_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           const __m512d phi,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) 
                                                                 FUNC_ATTRIBUTES;


                 
                   void ESth_f6217_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                  FUNC_ATTRIBUTES;

                
                   void ESth_f6217_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pr,
                                           const double * __restrict  palp,
                                           const double * __restrict  ptht,
                                           const double * __restrict  pphi,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) 
                                                                  FUNC_ATTRIBUTES;   

                    /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (phi component).
                           Formula 6.2-18
                      */


                
                   void ESph_f6218_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           const __m512d phi,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) 
                                                                  FUNC_ATTRIBUTES;


                 
                   void ESph_f6218_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                    FUNC_ATTRIBUTES;


                
                   void ESph_f6218_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pr,
                                           const double * __restrict  palp,
                                           const double * __restrict  ptht,
                                           const double * __restrict  pphi,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) 
                                                                    FUNC_ATTRIBUTES;


                    /*
                           Physical-Optics axial-incidence bistatic RCS
                           function of theta for (0 << theta < PI-2*alpha)
                           Formula 6.2-20
                      */


                 
                   __m512d rcs_f6220_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d tht) 
                                                                  FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f6220_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const double * __restrict __ATTR_ALIGN__(64) palp,
                                            const double * __restrict __ATTR_ALIGN__(64) ptht)
                                                                   FUNC_ATTRIBUTES;   

                  
                   __m512d rcs_f6220_zmm8r8_u(const double * __restrict  pgam0,
                                            const double * __restrict  palp,
                                            const double * __restrict  ptht)
                                                                    FUNC_ATTRIBUTES;   

                     /*
                           Axial incidence backscattering (theta = 0)
                           RCS (Spencer equation).
                           Formula 6.2-22
                       */

                
                   __m512d rcs_f6222_zmm8r8(const __m512d gam0,
                                        const __m512d alp)
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f6222_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const double * __restrict __ATTR_ALIGN__(64) palp)
                                                                     FUNC_ATTRIBUTES;   

                
                   __m512d rcs_f6222_zmm8r8_u(const double * __restrict  pgam0,
                                            const double * __restrict  palp) 
                                                                      FUNC_ATTRIBUTES;   

                    /*
                           Narrow-angle cone.
                           Scattered E-field.
                           Formula 6.2-24
                       */


                 
                   void ES_f6224_zmm8r8(const __m512d k0,
                                         const __m512d z,
                                         const __m512d alp,
                                         const __m512d x,
                                         const __m512d y,
                                         const __m512d z,
                                         __m512d * __restrict xre,
                                         __m512d * __restrict xim,
                                         __m512d * __restrict yre,
                                         __m512d * __restrict yim,
                                         __m512d * __restrict zre,
                                         __m512d * __restrict zim) 
                                                                 FUNC_ATTRIBUTES;   

                
                   void ES_f6224_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) px,
                                           const double * __restrict __ATTR_ALIGN__(64) py,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           double * __restrict __ATTR_ALIGN__(64) xre,
                                           double * __restrict __ATTR_ALIGN__(64) xim,
                                           double * __restrict __ATTR_ALIGN__(64) yre,
                                           double * __restrict __ATTR_ALIGN__(64) yim,
                                           double * __restrict __ATTR_ALIGN__(64) zre,
                                           double * __restrict __ATTR_ALIGN__(64) zim) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   void ES_f6224_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict pz,
                                           const double * __restrict palp,
                                           const double * __restrict  px,
                                           const double * __restrict  py,
                                           const double * __restrict  pz,
                                           double * __restrict  xre,
                                           double * __restrict  xim,
                                           double * __restrict  yre,
                                           double * __restrict  yim,
                                           double * __restrict  zre,
                                           double * __restrict  zim)
                                                                   FUNC_ATTRIBUTES;   

                     /*
                           Wide-angle cone.
                           Scattered E-field.
                           Formula 6.2-25
                       */


                
                   void ES_f6225_zmm8r8(const __m512d k0,
                                         const __m512d z,
                                         const __m512d alp,
                                         const __m512d x,
                                         const __m512d y,
                                         const __m512d z,
                                         __m512d * __restrict xre,
                                         __m512d * __restrict xim,
                                         __m512d * __restrict yre,
                                         __m512d * __restrict yim,
                                         __m512d * __restrict zre,
                                         __m512d * __restrict zim)
                                                                FUNC_ATTRIBUTES;   

                 
                   void ES_f6225_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           const double * __restrict __ATTR_ALIGN__(64) palp,
                                           const double * __restrict __ATTR_ALIGN__(64) px,
                                           const double * __restrict __ATTR_ALIGN__(64) py,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           double * __restrict __ATTR_ALIGN__(64) xre,
                                           double * __restrict __ATTR_ALIGN__(64) xim,
                                           double * __restrict __ATTR_ALIGN__(64) yre,
                                           double * __restrict __ATTR_ALIGN__(64) yim,
                                           double * __restrict __ATTR_ALIGN__(64) zre,
                                           double * __restrict __ATTR_ALIGN__(64) zim) 
                                                                FUNC_ATTRIBUTES;   

                
                   void ES_f6225_zmm8r8_u(const double * __restrict  pk0,
                                           const double * __restrict  pz,
                                           const double * __restrict  palp,
                                           const double * __restrict  px,
                                           const double * __restrict  py,
                                           const double * __restrict  pz,
                                           double * __restrict  xre,
                                           double * __restrict  xim,
                                           double * __restrict  yre,
                                           double * __restrict  yim,
                                           double * __restrict  zre,
                                           double * __restrict  zim) 
                                                               FUNC_ATTRIBUTES;      


                   /*
                           Wide-angle cone.
                           Radar Cross Section (angle = 0)
                           Formula 6.2-26
                     */

                
                   __m512d rcs_f6226_zmm8r8(const __m512d gam2,
                                            const __m512d alp) 
                                                                FUNC_ATTRIBUTES;   


                 
                   __m512d rcs_f6226_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam2,
                                            const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                                  FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6226_zmm8r8_u(const double * __restrict  pgam2,
                                            const double * __restrict  palp)
                                                                   FUNC_ATTRIBUTES;   


                    /*
                          The concave-tip axial-incidence, Physical-Optics RCS.
                          Formula 6.2-29

                      */


                
                   __m512d rcs_f6229_zmm8r8(const __m512d k0,
                                            const __m512d alp,
                                            const __m512d R) 
                                                                     FUNC_ATTRIBUTES;    

                  
                   __m512d rcs_f6229_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pR)
                                                                       FUNC_ATTRIBUTES;   


                 
                   __m512d rcs_f6229_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  palp,
                                              const double * __restrict  pR)
                                                                         FUNC_ATTRIBUTES;   


                    /*
                           RCS of pointed cone and flat plate of radius b.
                           Formula 6.2-30
                      */


                 
                   __m512d rcs_f6230_zmm8r8(const __m512d b,
                                            const __m512d k0) 
                                                                FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6230_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0)
                                                                 FUNC_ATTRIBUTES;     


                 
                   __m512d rcs_f6230_zmm8r8_u(const double * __restrict  pb,
                                              const double * __restrict  pk0)
                                                                   FUNC_ATTRIBUTES;   


                  /*
                       Physical-Optics approximation of rounded-tip cone,
                       for axial incidence.
                       Formula 6.2-31
                   */


                
                   __m512d rcs_f6231_zmm8r8(const __m512d alp,
                                            const __m512d k0,
                                            const __m512d b) 
                                                                  FUNC_ATTRIBUTES;   


                 
                   __m512d rcs_f6231_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) palp,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0,
                                            const double * __restrict __ATTR_ALIGN__(64) pb) 
                                                                   FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6231_zmm8r8_u(const double * __restrict  palp,
                                            const double * __restrict  pk0,
                                            const double * __restrict  pb)
                                                                    FUNC_ATTRIBUTES;   


                   /*
                          Not nose-one incidence, for theta << alpha -- 
                          PO approximation for rounded-tip cones.
                          Formula 6.2-23
                      */


                
                   __m512d rcs_f6223_zmm8r8(const __m512d tht,
                                            const __m512d k0,
                                            const __m512d b,
                                            const __m512d a,
                                            const __m512d alp) 
                                                                       FUNC_ATTRIBUTES;   

               
                   __m512d rcs_f6223_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ptht,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                                        FUNC_ATTRIBUTES;   

              
                   __m512d rcs_f6223_zmm8r8_u(const double * __restrict  ptht,
                                            const double * __restrict  pk0,
                                            const double * __restrict  pb,
                                            const double * __restrict  pa,
                                            const double * __restrict  palp) 
                                                                      FUNC_ATTRIBUTES;   

                  /*
                         Finite cones, approximated solutions.
                         Rayleigh region.
                         RCS.
                         Formula 6.3-4
                     */



                  
              
                   __m512d rcs_f634_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d h) 
                                                            FUNC_ATTRIBUTES;   


              
                   __m512d rcs_f634_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) ph) 
                                                                FUNC_ATTRIBUTES;     


                
                   __m512d rcs_f634_zmm8r8_u(const double * __restrict pk0,
                                             const double * __restrict  pa,
                                             const double * __restrict ph)
                                                              FUNC_ATTRIBUTES;   


                   /*
                          Rayleigh RCS of cone-hemispheroid.
                          Formula 6.3-5
                     */


                 
                   __m512d rcs_f635_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d b,
                                           const __m512d h)
                                                              FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f635_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pb,
                                             const double * __restrict __ATTR_ALIGN__(64) ph) 
                                                                FUNC_ATTRIBUTES;   


                   __m512d rcs_f635_zmm8r8_u(const double * __restrict  pk0,
                                             const double * __restrict  pa,
                                             const double * __restrict  pb,
                                             const double * __restrict  ph) 
                                                                  FUNC_ATTRIBUTES;   

                  /*
                          Rayleigh RCS of cone-hemispheroid (b == a)
                          Formula 6.3-6
                      */

                
                   __m512d rcs_f636_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d h) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f636_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) ph) 
                                                                   FUNC_ATTRIBUTES;   


               
                   __m512d rcs_f636_zmm8r8_u(const double * __restrict  pk0,
                                             const double * __restrict  pa,
                                             const double * __restrict  ph)
                                                                   FUNC_ATTRIBUTES;   


                   /*
                         Flat-back cone, backscatter RCS.
                         Formula 6.3-9
                   */


               
                   __m512d rcs_f639_zmm8r8(const __m512d gam0,
                                           const __m512d alp,
                                           const __m512d k0,
                                           const __m512d h) 
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f639_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) palp,
                                             const double * __restrict __ATTR_ALIGN__(64) ph)
                                                                     FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f639_zmm8r8_u(const double * __restrict  pgam0, 
                                             const double * __restrict  pk0,
                                             const double * __restrict  palp,
                                             const double * __restrict  ph) 
                                                                      FUNC_ATTRIBUTES;   


                   /*
                        Cone tip scattering RCS.
                        Formula 6.3-10
                    */


              
                   __m512d rcs_f6310_zmm8r8(const __m512d gam0,
                                            const __m512d alp) 
                                                                      FUNC_ATTRIBUTES;   

                
                   __m512d rcs_f6310_zmm8r8_a(const double * __restrict  __ATTR_ALIGN__(64) pgam0, 
                                              const double * __restrict  __ATTR_ALIGN__(64) palp) 
                                                                       FUNC_ATTRIBUTES;   


                  
                   __m512d rcs_f6310_zmm8r8_u(const double * __restrict  pgam0, 
                                              const double * __restrict  palp) 
                                                                        FUNC_ATTRIBUTES;   

                   /*
                         Case of flat-back cone joined by the sphere.
                         Axial-incidence backscatter RCS.
                         Formula 6.3-11
                    */

              
                   __m512d rcs_f6311_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d k0,
                                            const __m512d h,
                                            const __m512d z1) 
                                                              FUNC_ATTRIBUTES;   


                 
                   __m512d rcs_f6311_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) palp,
                                             const double * __restrict __ATTR_ALIGN__(64) ph,
                                             const double * __restrict __ATTR_ALIGN__(64) pz1) 
                                                              FUNC_ATTRIBUTES;   

               
                   __m512d rcs_f6311_zmm8r8_u(const double * __restrict  pgam0, 
                                             const double * __restrict  pk0,
                                             const double * __restrict  palp,
                                             const double * __restrict  ph,
                                             const double * __restrict  pz1) 
                                                                   FUNC_ATTRIBUTES;   

                    /*
                          Flat-back cone backscattering RCS.
                          Formula 6.3-14
                     */

              
                   __m512d rcs_f6314_zmm8r8(const __m512d a,
                                            const __m512d alp)
                                                                     FUNC_ATTRIBUTES;     


                   __m512d rcs_f6314_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                                      FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6314_zmm8r8_u( const double * __restrict  pa,
                                             const double * __restrict  palp) 
                                                                        FUNC_ATTRIBUTES;   

                   /*
                          Scattering from a thick cylinder of length
                          being a slant height of the cone of radius
                          (4/9*a*sec(alpha)), i.e. RCS(PI/2-alpha).
                          Formula 6.3-18
                      */


               
                   __m512d rcs_f6318_zmm8r8(const __m512d gam0,
                                            const __m512d k0h,
                                            const __m512d alp) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f6318_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                                   FUNC_ATTRIBUTES;   
                                            



                 
                   __m512d rcs_f6318_zmm8r8_u(const double * __restrict  pgam0, 
                                             const double * __restrict  pk0h,
                                             const double * __restrict  palp) 
                                                                   FUNC_ATTRIBUTES;   


                 /*
                     Fresnel integral component.
                     Helper kernel for formula 6.3-15
                     Formula 6.3-16
                  */


                 
                   void Frho_f6316_zmm8r8(const __m512d xxa,
                                           const __m512d rho,
                                           __m512d * __restrict ssa,
                                           __m512d * __restrict cca) 
                                                                    FUNC_ATTRIBUTES;   


               
                   void Frho_f6316_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pxxa,
                                             const double * __restrict __ATTR_ALIGN__(64) prho,
                                             double * __restrict __ATTR_ALIGN__(64) ssa,
                                             double * __restrict __ATTR_ALIGN__(64) cca)
                                                                     FUNC_ATTRIBUTES;   

                
                   void Frho_f6316_zmm8r8_u(const double * __restrict  pxxa,
                                             const double * __restrict  prho,
                                             double * __restrict __ATTR_ALIGN__(64) ssa,
                                             double * __restrict __ATTR_ALIGN__(64) cca) 
                                                                     FUNC_ATTRIBUTES;   


                   /*
                          Incidence near broadside to the cone - backscatter RCS.
                          Formula 6.3-15
                      */


                
                   __m512d rcs_f6315_zmm8r8(const __m512d rho,
                                            const __m512d gam0,
                                            const __m512d k0h,
                                            const __m512d tht,
                                            const __m512d alp) 
                                                         FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f6315_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) prho,
                                              const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht,
                                              const double * __restrict __ATTR_ALIGN__(64) palp)
                                                            FUNC_ATTRIBUTES;   

                
                   __m512d rcs_f6315_zmm8r8_u(const double * __restrict  prho,
                                              const double * __restrict  pgam0,
                                              const double * __restrict  pk0h,
                                              const double * __restrict  ptht,
                                              const double * __restrict  palp) 
                                                             FUNC_ATTRIBUTES;   

                      /*
                          Specular return (RCS) of equivalent cylinder
                          to cone sphere.
                          Formula 6.3-19
                      */


                
                   __m512d rcs_f6319_zmm8r8(const __m512d k0h,
                                            const __m512d alp,
                                            const __m512d a) 
                                                            FUNC_ATTRIBUTES;   

                   __m512d rcs_f6319_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) palp)
                                                             FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6319_zmm8r8_u(const double * __restrict  pk0h,
                                              const double * __restrict  pa,
                                              const double * __restrict  palp) 
                                                              FUNC_ATTRIBUTES;    


                  /*
                          Width of specular lobe of cylinder formula 6.3-19
                          Formula 6.3-20 
                     */

                    
                  
                   __m512d dpsi_f6320_zmm8r8(const __m512d gam0,
                                             const __m512d h,
                                             const __m512d alp) 
                                                              FUNC_ATTRIBUTES;   

                
                   __m512d dpsi_f6320_zmm8r8_a(const double * __ATTR_ALIGN__(64) pgam0,
                                               const double * __ATTR_ALIGN__(64) ph,
                                               const double * __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


                
                   __m512d dpsi_f6320_zmm8r8_u(const double *  pgam0,
                                               const double *  ph,
                                               const double *  palp)
                                                               FUNC_ATTRIBUTES;   


                 


                  /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-42
                     */


                 
                   __m512d K1_f6342_zmm8r8(const __m512d k0a,
                                           const __m512d tht,
                                           const __m512d alp) 
                                                              FUNC_ATTRIBUTES;   


                
                   __m512d K1_f6342_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht,
                                             const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


               
                   __m512d K1_f6342_zmm8r8_u(const double * __restrict  pk0a,
                                             const double * __restrict  ptht,
                                             const double * __restrict  palp)
                                                                FUNC_ATTRIBUTES;   


                   /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-43
                     */


               
                   __m512d K2_f6343_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d Ls,
                                           const __m512d tht,
                                           const __m512d alp)
                                                               FUNC_ATTRIBUTES;   

                
                   __m512d K2_f6343_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pLs,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht,
                                             const double * __restrict __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


               
                   __m512d K2_f6343_zmm8r8_u(const double * __restrict  pk0,
                                             const double * __restrict  pa,
                                             const double * __restrict  pLs,
                                             const double * __restrict  ptht,
                                             const double * __restrict  palp)
                                                              FUNC_ATTRIBUTES;   


                    /*
                          Cylindrical current approximation of
                          flat based truncated cone RCS for
                          perpendicular polarization.
                          Formula 6.3-41 
                    */


                 
                   __m512d rcs_f6341_zmm8r8(const __m512d gam0,
                                            const __m512d tht,
                                            const __m512d alp,
                                            const __m512d Ls,
                                            const __m512d k0,
                                            const __m512d a,
                                            const __m512d a1) 
                                                              FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f6341_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pLs,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pa1)
                                                               FUNC_ATTRIBUTES;   

              
                   __m512d rcs_f6341_zmm8r8_u(const double * __restrict  pgam0,
                                              const double * __restrict  ptht,
                                              const double * __restrict  palp,
                                              const double * __restrict  pLs,
                                              const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pa1) 
                                                              FUNC_ATTRIBUTES;   

                   /*
                          Cylindrical current approximation of
                          flat based pointed cone RCS for
                          perpendicular polarization.
                          Formula 6.3-44
                     */


                  
                   __m512d rcs_f6344_zmm8r8(const __m512d gam0,
                                            const __m512d a,
                                            const __m512d tht,
                                            const __m512d alp,
                                            const __m512d k0) 
                                                      FUNC_ATTRIBUTES;   


                
                   __m512d rcs_f6344_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) ptht,
                                            const double * __restrict __ATTR_ALIGN__(64) palp,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0)
                                                        FUNC_ATTRIBUTES;   

                   /*
                        Geometrical Diffraction.
                        Flat-backed cone, backscattered RCS.
                        Simplified by neglecting a second order terms of 6.3-49
                        Formula 6.3-50
                    */


                 
                   __m512d rcs_f6350_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d alp)
                                                       FUNC_ATTRIBUTES;   

                   __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512d rcs_f6350_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) palp)
                                                        FUNC_ATTRIBUTES;   

               
                   __m512d rcs_f6350_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  palp)
                                                         FUNC_ATTRIBUTES;   


                  /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series convergence.
                         Formula 6.3-56
                    */


                  /*
                         Complex 'j' phase associated with each 'j' amplitude.
                         Formula 6.3-58, 6.3-56 (complex exponent term only).
                    */


                   void expj_f6358_zmm16(const __m512d k0,
                                         const __m512d beta,
                                         const __m512d a,
                                         const __m512d h,
                                         const __m512d tht,
                                       __m512d * __restrict cer,
                                       __m512d * __restrict cei) 
                                                   FUNC_ATTRIBUTES;   

                  
                   void expj_f6358_zmm16_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pbeta,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) ph,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht,
                                           double * __restrict __ATTR_ALIGN__(64) cer,
                                           double * __restrict __ATTR_ALIGN__(64) cei) 
                                                   FUNC_ATTRIBUTES;   


                  
                   void expj_f6358_zmm16_u(const double * __restrict  pk0,
                                           const double * __restrict  pbeta,
                                           const double * __restrict  pa,
                                           const double * __restrict  ph,
                                           const double * __restrict  ptht,
                                           double * __restrict  cer,
                                           double * __restrict  cei)
                                                    FUNC_ATTRIBUTES;   


                   /*
                         Axially asymetric edge diffraction amplitudes.
                         Part of kernel 6.3-56
                         Formula 6.3-57
                     */


                 
                   __m512d rcs_f6357_zmm8r8(const __m512d alp,
                                            const __m512d tht,
                                            const __m512d beta,
                                            const __m512d beta1,
                                            const __m512d a,
                                            const __m512d k0,
                                            const bool ver ) 
                                                      FUNC_ATTRIBUTES;   


                 
                   __m512d rcs_f6357_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht,
                                              const double * __restrict __ATTR_ALIGN__(64) pbeta,
                                              const double * __restrict __ATTR_ALIGN__(64) pbeta1,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const bool ver) 
                                                       FUNC_ATTRIBUTES;   


                      __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512d rcs_f6357_zmm8r8_u(const double * __restrict  palp,
                                              const double * __restrict  ptht,
                                              const double * __restrict  pbeta,
                                              const double * __restrict  pbeta1,
                                              const double * __restrict  pa,
                                              const double * __restrict  pk0,
                                              const bool ver) 
                                                     FUNC_ATTRIBUTES;   

                       /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- single term only
                    */


                
                   __m512d rcs_f6356_term1_zmm8r8(const __m512d alp,
                                              const __m512d h,
                                              const __m512d beta,
                                              const __m512d beta1,
                                              const __m512d a,
                                              const __m512d k0,
                                              const __m512d tht,
                                              const bool ver) 
                                                    FUNC_ATTRIBUTES;   

                
                   __m512d rcs_f6356_term1_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) palp,
                                                  const double * __restrict __ATTR_ALIGN__(64) ph,
                                                  const double * __restrict __ATTR_ALIGN__(64) pbeta,
                                                  const double * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                  const double * __restrict __ATTR_ALIGN__(64) pa,
                                                  const double * __restrict __ATTR_ALIGN__(64) pk0,
                                                  const double * __restrict __ATTR_ALIGN__(64) ptht,
                                                  const bool ver)
                                                    FUNC_ATTRIBUTES;   


                   __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512d rcs_f6356_term1_zmm8r8_u(const double * __restrict  palp,
                                                  const double * __restrict  ph,
                                                  const double * __restrict  pbeta,
                                                  const double * __restrict  pbeta1,
                                                  const double * __restrict  pa,
                                                  const double * __restrict  pk0,
                                                  const double * __restrict  ptht,
                                                  const bool ver) 
                                                     FUNC_ATTRIBUTES;   


                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms.
                    */


               
                   __m512d rcs_f6356_nterm_zmm8r8(  const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                    const __m512d h,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                    const __m512d a,
                                                    const __m512d k0,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                    const int32_t n,
                                                    const bool ver) 
                                                          FUNC_ATTRIBUTES;   


                    /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 16x.
                    */

                
                   double rcs_f6356_nterm_u16x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                     FUNC_ATTRIBUTES;   


                   /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 8x.
                    */


                 
                   double rcs_f6356_nterm_u8x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                        FUNC_ATTRIBUTES;   

              

                      /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 4x.
                    */


                 
                   double rcs_f6356_nterm_u4x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                         FUNC_ATTRIBUTES;   

                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 2x.
                    */


                 
                   double rcs_f6356_nterm_u2x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                             FUNC_ATTRIBUTES;   
                       

                    

                   /*
                          Large loop k0a >> 1 , for theta = 0 degree.
                          Formula 6.4-10
                      */


                
                   __m512d rcs_f6410_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b) 
                                                             FUNC_ATTRIBUTES;   


                   __m512d rcs_f6410_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                            const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb)
                                                             FUNC_ATTRIBUTES;     


                
                   __m512d rcs_f6410_zmm8r8_u(const double * __restrict  pk0,
                                            const double * __restrict  pa,
                                            const double * __restrict  pb) 
                                                              FUNC_ATTRIBUTES;   

                     /*
                         THe far-zone amplitude for TE-case.
                         For small loop k0a<<1
                         Formula 6.4-11
                      */

                       
                 
                   void Uv_f6411_zmm8r8(const __m512d a,
                                         const __m512d k0,
                                         const __m512d b,
                                         const __m512d tht,
                                         __m512d *  __restrict Uvr,
                                         __m512d *  __restrict Uvi) 
                                                              FUNC_ATTRIBUTES;   


                
                   void Uv_f6411_u16x_zmm8r8(const __m512d a,
                                              const __m512d k0,
                                              const __m512d b,
                                              const __m512d * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) 
                                                               FUNC_ATTRIBUTES;   

                
                   void Uv_f6411_u8x_zmm8r8( const __m512d a,
                                              const __m512d k0,
                                              const __m512d b,
                                              const __m512d * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) 
                                                                FUNC_ATTRIBUTES;   


                  
                   void Uv_f6411_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pb,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht,
                                           double * __restrict __ATTR_ALIGN__(64) Uvr,
                                           double * __restrict __ATTR_ALIGN__(64) Uvi) 
                                                                 FUNC_ATTRIBUTES;   


                
                   void Uv_f6411_zmm8r8_u(const double * __restrict  pa,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pb,
                                           const double * __restrict  ptht,
                                           double * __restrict  Uvr,
                                           double * __restrict  Uvi) 
                                                                   FUNC_ATTRIBUTES;   

                  
                   __m512d rcs_f6412_zmm16(const __m512d a,
                                          const __m512d b,
                                          const __m512d k0,
                                          const __m512d tht) 
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512d rcs_f6412_zmm16_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0,
                                            const double * __restrict __ATTR_ALIGN__(64) ptht)
                                                                     FUNC_ATTRIBUTES;    


                 
                   __m512d rcs_f6412_zmm16_u(const double * __restrict  pa,
                                            const double * __restrict  pb,
                                            const double * __restrict  pk0,
                                            const double * __restrict  ptht) 
                                                                      FUNC_ATTRIBUTES;   

                 
                                          

                    


                    





                  



















#endif  /*__GMS_RCS_CONE_WEDGE_ZMM8R8_H__*/
