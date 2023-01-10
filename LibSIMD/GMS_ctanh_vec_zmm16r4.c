






#include <immintrin.h>
#include "GMS_config.h"
#include "GMS_csinh_vec_zmm16r4.h"
#include "GMS_ccosh_vec_zmm16r4.h"
#include "GMS_cdiv_vec_zmm16r4.h"


               
                 
                   void ctanh_zmm16r4_unroll_10x_u( const float * __restrict xre,
                                                   const float * __restrict xim,
                                                   float * __restrict wrkcr,
                                                   float * __restrict wrkci,
                                                   float * __restrict wrksr,
                                                   float * __restrict wrksi,
                                                   float * __restrict ctre,
                                                   float * __restrict ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_10x_u(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_10x_u(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_10x_u(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   


                   
                   void ctanh_zmm16r4_unroll_10x_a( const float * __restrict xre,
                                                   const float * __restrict  xim,
                                                   float * __restrict  wrkcr,
                                                   float * __restrict  wrkci,
                                                   float * __restrict  wrksr,
                                                   float * __restrict  wrksi,
                                                   float * __restrict  ctre,
                                                   float * __restrict  ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_10x_a(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_10x_a(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_10x_a(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   


                  
                   void ctanh_zmm16r4_unroll_8x_u( const float * __restrict xre,
                                                   const float * __restrict xim,
                                                   float * __restrict wrkcr,
                                                   float * __restrict wrkci,
                                                   float * __restrict wrksr,
                                                   float * __restrict wrksi,
                                                   float * __restrict ctre,
                                                   float * __restrict ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_8x_u(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_8x_u(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_8x_u(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   


                   
                   void ctanh_zmm16r4_unroll_8x_a( const float * __restrict  xre,
                                                   const float * __restrict  xim,
                                                   float * __restrict  wrkcr,
                                                   float * __restrict  wrkci,
                                                   float * __restrict  wrksr,
                                                   float * __restrict  wrksi,
                                                   float * __restrict  ctre,
                                                   float * __restrict  ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_8x_a(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_8x_a(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_8x_a(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   


                 
                   void ctanh_zmm16r4_unroll_6x_u( const float * __restrict xre,
                                                   const float * __restrict xim,
                                                   float * __restrict wrkcr,
                                                   float * __restrict wrkci,
                                                   float * __restrict wrksr,
                                                   float * __restrict wrksi,
                                                   float * __restrict ctre,
                                                   float * __restrict ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_6x_u(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_6x_u(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_6x_u(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   


                  
                   void ctanh_zmm16r4_unroll_6x_a( const float * __restrict  xre,
                                                   const float * __restrict  xim,
                                                   float * __restrict  wrkcr,
                                                   float * __restrict  wrkci,
                                                   float * __restrict  wrksr,
                                                   float * __restrict  wrksi,
                                                   float * __restrict  ctre,
                                                   float * __restrict  ctim,
                                                   const int32_t n) {

                        csinhv_zmm16r4_unroll_6x_a(xre,xim,wrksr,wrksi,n);
                        ccoshv_zmm16r4_unroll_6x_a(xre,xim,wrkcr,wrkci,n);
                        cdiv_zmm16r4_unroll_6x_a(wrksr,wrksi,wrkcr,wrkci,ctre,ctri,n);
                }   



   
