
#include "GMS_complex_zmm8r8.h"


                    void cadd_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim)  {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&yre[0]);
                        _mm512_storeu_pd(&zre[0], _mm512_add_pd(zmm0,zmm1));
                        zmm2  = _mm512_loadu_pd(&xim[0]);
                        zmm3  = _mm512_loadu_pd(&yim[0]);
                        _mm512_storeu_pd(&zim[0], _mm512_add_pd(zmm2,zmm3)); 
                   }


                    void cadd_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&yre[0]);
                        _mm512_store_pd(&zre[0], _mm512_add_pd(zmm0,zmm1));
                        zmm2  = _mm512_load_pd(&xim[0]);
                        zmm3  = _mm512_load_pd(&yim[0]);
                        _mm512_store_pd(&zim[0], _mm512_add_pd(zmm2,zmm3));  
                   }


                  void cadd_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim) {

                        register __m512d zmm0,zmm1;
                        zmm0  = _mm512_add_pd(xre,yre);
                        *zre  = zmm0;
                        zmm1  = _mm512_add_pd(xim,yim);
                        *zim  = zmm1; 
                  }


                 
                   void cadd_zmm8r8_uip(const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) {
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&xim[0]);
                        zmm2  = _mm512_loadu_pd(&zre[0]);
                        zmm3  = _mm512_loadu_pd(&zim[0])
                        _mm512_storeu_pd(&zre[0], _mm512_add_pd(zmm2,zmm0));
                        _mm512_storeu_pd(&zim[0], _mm512_add_pd(zmm3,zmm1));
              }


                  
                   void cadd_zmm8r8_aip(const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim) {
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&xim[0]);
                        zmm2  = _mm512_load_pd(&zre[0]);
                        zmm3  = _mm512_load_pd(&zim[0])
                        _mm512_store_pd(&zre[0], _mm512_add_pd(zmm2,zmm0));
                        _mm512_store_pd(&zim[0], _mm512_add_pd(zmm3,zmm1));
              }


              /////////////////////////////////////////////////////////////////////////////////////////


                
                   void csub_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&yre[0]);
                        _mm512_storeu_pd(&zre[0], _mm512_sub_pd(zmm0,zmm1));
                        zmm2  = _mm512_loadu_pd(&xim[0]);
                        zmm3  = _mm512_loadu_pd(&yim[0]);
                        _mm512_storeu_pd(&zim[0], _mm512_sub_pd(zmm2,zmm3));
                 }


                  
                   void csub_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&yre[0]);
                        _mm512_store_pd(&zre[0], _mm512_sub_pd(zmm0,zmm1));
                        zmm2  = _mm512_load_pd(&xim[0]);
                        zmm3  = _mm512_load_pd(&yim[0]);
                        _mm512_store_pd(&zim[0], _mm512_sub_pd(zmm2,zmm3));
                 }


                 
                   void csub_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim) {
                     
                        register __m512d zmm0,zmm1;
                        zmm0  = _mm512_sub_pd(xre,yre);
                        *zre  = zmm0;
                        zmm1  = _mm512_sub_pd(xim,yim);
                        *zim  = zmm1;
                }


                 
                   void csub_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim) {

                        *zre = _mm512_sub_pd(xre,s);
                        *zim = _mm512_sub_pd(xim,s);
               }


                  
                   void csub_zmm8r8_uip(const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) {
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&xim[0]);
                        zmm2  = _mm512_loadu_pd(&zre[0]);
                        zmm3  = _mm512_loadu_pd(&zim[0])
                        _mm512_storeu_pd(&zre[0], _mm512_sub_pd(zmm2,zmm0));
                        _mm512_storeu_pd(&zim[0], _mm512_sub_pd(zmm3,zmm1));
              }


                  
                   void csub_zmm8r8_aip(const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim) {
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&xim[0]);
                        zmm2  = _mm512_load_pd(&zre[0]);
                        zmm3  = _mm512_load_pd(&zim[0])
                        _mm512_store_pd(&zre[0], _mm512_sub_pd(zmm2,zmm0));
                        _mm512_store_pd(&zim[0], _mm512_sub_pd(zmm3,zmm1));
              }


           ////////////////////////////////////////////////////////////////////////////////////


                
                   void cmul_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim) {

                           register __m512d zmm0,zmm1,zmm2,zmm3,zmm4,zmm5;
                           zmm0  = _mm512_loadu_pd(&xre[0]);
                           zmm1  = _mm512_loadu_pd(&yre[0]);
                           zmm2  = _mm512_loadu_pd(&xim[0]);
                           zmm3  = _mm512_loadu_pd(&yim[0]);
                           zmm4  = _mm512_sub_pd(_mm512_mul_pd(zmm0,zmm1),
                                                                        _mm512_mul_pd(zmm2,zmm3));
                           _mm512_storeu_pd(&zre[0], zmm4);
                           zmm5  = _mm512_mul_pd(_mm512_mul_pd(zmm2,zmm1),
                                                                        _mm512_mul_pd(zmm0,zmm3));
                           _mm512_storeu_pd(&zim[0], zmm5);
               }


                
                   void cmul_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim) {

                           register __m512d zmm0,zmm1,zmm2,zmm3,zmm4,zmm5;
                           zmm0  = _mm512_load_pd(&xre[0]);
                           zmm1  = _mm512_load_pd(&yre[0]);
                           zmm2  = _mm512_load_pd(&xim[0]);
                           zmm3  = _mm512_load_pd(&yim[0]);
                           zmm4  = _mm512_sub_pd(_mm512_mul_pd(zmm0,zmm1),
                                                                        _mm512_mul_pd(zmm2,zmm3));
                           _mm512_store_pd(&zre[0], zmm4);
                           zmm5  = _mm512_mul_pd(_mm512_mul_pd(zmm2,zmm1),
                                                                        _mm512_mul_pd(zmm0,zmm3));
                           _mm512_store_pd(&zim[0], zmm5);
               }


                 
                   void cmul_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim) {

                         register __m512d zmm0,zmm1;
                         zmm0 = _mm512_sub_pd(_mm512_mul_pd(xre,yre),
                                              _mm512_mul_pd(xim,yim));
                         *zre  = zmm0;
                         zmm1 = _mm512_mul_pd(_mm512_mul_pd(xim,yre),
                                              _mm512_mul_pd(xre,yim));
                         *zim  = zmm1;
                }


                  
                   void cmul_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict   zre,
                                     __m512d * __restrict   zim) {

                        *zre = _mm512_mul_pd(xre,s);
                        *zim = _mm512_mul_pd(xim,s);
               }


                  
                   void cmul_zmm8r8_uip(const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) {

                           register __m512d zmm0,zmm1,zmm2,zmm3,zmm4,zmm5;
                           zmm0  = _mm512_loadu_pd(&xre[0]);
                           zmm1  = _mm512_loadu_pd(&zre[0]);
                           zmm2  = _mm512_loadu_pd(&xim[0]);
                           zmm3  = _mm512_loadu_pd(&zim[0]);
                           zmm4  = _mm512_sub_pd(_mm512_mul_pd(zmm0,zmm1),
                                                 _mm512_mul_pd(zmm2,zmm3));
                           _mm512_storeu_pd(&zre[0], zmm4);
                           zmm5  = _mm512_mul_pd(_mm512_mul_pd(zmm2,zmm1),
                                                 _mm512_mul_pd(zmm0,zmm3));
                           _mm512_storeu_pd(&zim[0], zmm5);
               }


                  
                   void cmul_zmm8r8_aip(const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim) {

                           register __m512d zmm0,zmm1,zmm2,zmm3,zmm4,zmm5;
                           zmm0  = _mm512_load_pd(&xre[0]);
                           zmm1  = _mm512_load_pd(&zre[0]);
                           zmm2  = _mm512_load_pd(&xim[0]);
                           zmm3  = _mm512_load_pd(&zim[0]);
                           zmm4  = _mm512_sub_pd(_mm512_mul_pd(zmm0,zmm1),
                                                 _mm512_mul_pd(zmm2,zmm3));
                           _mm512_store_pd(&zre[0], zmm4);
                           zmm5  = _mm512_mul_pd(_mm512_mul_pd(zmm2,zmm1),
                                                 _mm512_mul_pd(zmm0,zmm3));
                           _mm512_store_pd(&zim[0], zmm5);
               }


              /////////////////////////////////////////////////////////////////////////////////


              
                   void cdiv_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3; 
                        register __m512d zmm4,zmm5,zmm6;
                        zmm0  = _mm512_loadu_pd(&xre[0]); //a
                        zmm1  = _mm512_loadu_pd(&yim[0]); //d
                        zmm2  = _mm512_loadu_pd(&xim[0]); //b
                        zmm3  = _mm512_loadu_pd(&yre[0]); //c
                        zmm4  = _mm512_fmadd_pd(zmm0,zmm3,
                                                _mm512_mul_pd(zmm2,zmm1));
                        zmm5  = _mm512_fmsub_pd(zmm2,zmm3,
                                                _mm512_mul_pd(zmm0,zmm1));
                        zmm6  = _mm512_fmadd_pd(zmm3,zmm3),
                                                _mm512_mul_pd(zmm1,zmm1));
                        _mm512_storeu_pd(&zre[0], _mm512_div_pd(zmm4,zmm6));
                        _mm512_storeu_pd(&zim[0], _mm512_div_pd(zmm5,zmm6));
                }


                  
                   void cdiv_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3; 
                        register __m512d zmm4,zmm5,zmm6;
                        zmm0  = _mm512_load_pd(&xre[0]); //a
                        zmm1  = _mm512_load_pd(&yim[0]); //d
                        zmm2  = _mm512_load_pd(&xim[0]); //b
                        zmm3  = _mm512_load_pd(&yre[0]); //c
                        zmm4  = _mm512_fmadd_pd(zmm0,zmm3,
                                                _mm512_mul_pd(zmm2,zmm1));
                        zmm5  = _mm512_fmsub_pd(zmm2,zmm3,
                                                _mm512_mul_pd(zmm0,zmm1));
                        zmm6  = _mm512_fmadd_pd(zmm3,zmm3,
                                                _mm512_mul_pd(zmm1,zmm1));
                        _mm512_store_pd(&zre[0], _mm512_div_pd(zmm4,zmm6));
                        _mm512_store_pd(&zim[0], _mm512_div_pd(zmm5,zmm6));
                }
              
                                         
                  
                   void cdiv_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim) {

                      register __m512d zmm0,zmm1,zmm2;
                      zmm0 = _mm512_fmadd_pd(xre,yre,
                                           _mm512_mul_pd(xim,yim));
                      zmm1 = _mm512_fmsub_pd(xim,yre,
                                           _mm512_mul_pd(xre,yim));
                      zmm2 = _mm512_fmadd_pd(zmm3,zmm3,
                                           _mm512_mul_pd(zmm1,zmm1));
                      *zre  = _mm512_div_pd(zmm0,zmm2);
                      *zim  = _mm512_div_pd(zmm1,zmm2);
                }


                 
                   void cdiv_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim) {

                        *zre = _mm512_div_pd(xre,s);
                        *zim = _mm512_div_pd(xim,s);
               }


                  
                   void cdiv_zmm8r8_uip(const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3; 
                        register __m512d zmm4,zmm5,zmm6;
                        zmm0  = _mm512_loadu_pd(&xre[0]); //a
                        zmm1  = _mm512_loadu_pd(&zim[0]); //d
                        zmm2  = _mm512_loadu_pd(&xim[0]); //b
                        zmm3  = _mm512_loadu_pd(&zre[0]); //c
                        zmm4  = _mm512_fmadd_pd(zmm0,zmm3,
                                                _mm512_mul_pd(zmm2,zmm1));
                        zmm5  = _mm512_fmsub_pd(zmm2,zmm3,
                                                _mm512_mul_pd(zmm0,zmm1));
                        zmm6  = _mm512_fmadd_pd(zmm3,zmm3,
                                                _mm512_mul_pd(zmm1,zmm1));
                        _mm512_storeu_pd(&zre[0], _mm512_div_pd(zmm4,zmm6));
                        _mm512_storeu_pd(&zim[0], _mm512_div_pd(zmm5,zmm6));
              }


                  
                   void cdiv_zmm8r8_aip(const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3; 
                        register __m512d zmm4,zmm5,zmm6;
                        zmm0  = _mm512_load_pd(&xre[0]); //a
                        zmm1  = _mm512_load_pd(&zim[0]); //d
                        zmm2  = _mm512_load_pd(&xim[0]); //b
                        zmm3  = _mm512_load_pd(&zre[0]); //c
                        zmm4  = _mm512_fmadd_pd(zmm0,zmm3,
                                                _mm512_mul_pd(zmm2,zmm1));
                        zmm5  = _mm512_fmsub_pd(zmm2,zmm3,
                                                _mm512_mul_pd(zmm0,zmm1));
                        zmm6  = _mm512_fmadd_pd(zmm3,zmm3,
                                                _mm512_mul_pd(zmm1,zmm1));
                        _mm512_store_pd(&zre[0], _mm512_div_pd(zmm4,zmm6));
                        _mm512_store_pd(&zim[0], _mm512_div_pd(zmm5,zmm6));
              }


             //////////////////////////////////////////////////////////////////////////////////


              
                   void cdiv_smith_zmm8r8_u(const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double *       __restrict zre,
                                             double *       __restrict zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d r,den;
                        __mmask8 m = 0x0;
                        zmm0 = _mm512_loadu_pd(&yre[0]); // c
                        zmm1 = _mm512_loadu_pd(&yim[0]); // d
                        zmm2 = _mm512_loadu_pd(&xre[0]); // a
                        zmm3 = _mm512_loadu_pd(&xim[0]); // b
                        m    = _mm512_cmp_pd_mask(_mm512_abs_pd(zmm0),
                                                  _mm512_abs_pd(zmm1),
                                                  _CMP_GE_OQ);
                        r    = _mm512_mask_blend_pd(m,_mm512_div_pd(zmm0,zmm1),
                                                      _mm512_div_pd(zmm1,zmm0)); // r
                        den  = _mm512_mask_blend_pd(m,_mm512_fmadd_pd(r,zmm0,zmm1),
                                                      _mm512_fmadd_pd(r,zmm1,zmm0));
                        _mm512_storeu_pd(&zre[0], _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmadd_pd(zmm2,r,zmm3),den),
                                                _mm512_div_pd(_mm512_fmadd_pd(zmm3,r,zmm2),den)));
                        _mm512_storeu_pd(&zim[0], _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmsub_pd(zmm3,r,zmm2),den),
                                                _mm512_div_pd(_mm512_sub_pd(zmm3,_mm512_mul_pd(zmm2,r)),den)));
               }


                 
                   void cdiv_smith_zmm8r8_a(const double * __restrict __attribute__((aligned(64)))  xre,
                                             const double * __restrict __attribute__((aligned(64)))  xim,
                                             const double * __restrict __attribute__((aligned(64)))  yre,
                                             const double * __restrict __attribute__((aligned(64)))  yim,
                                             double *       __restrict __attribute__((aligned(64)))  zre,
                                             double *       __restrict __attribute__((aligned(64)))  zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d r,den;
                        __mmask8 m = 0x0;
                        zmm0 = _mm512_load_pd(&yre[0]); // c
                        zmm1 = _mm512_load_pd(&yim[0]); // d
                        zmm2 = _mm512_load_pd(&xre[0]); // a
                        zmm3 = _mm512_load_pd(&xim[0]); // b
                        m    = _mm512_cmp_pd_mask(_mm512_abs_pd(zmm0),
                                                  _mm512_abs_pd(zmm1),
                                                  _CMP_GE_OQ);
                        r    = _mm512_mask_blend_pd(m,_mm512_div_pd(zmm0,zmm1),
                                                      _mm512_div_pd(zmm1,zmm0)); // r
                        den  = _mm512_mask_blend_pd(m,_mm512_fmadd_pd(r,zmm0,zmm1),
                                                      _mm512_fmadd_pd(r,zmm1,zmm0));
                        _mm512_storeu_pd(&zre[0], _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmadd_pd(zmm2,r,zmm3),den),
                                                _mm512_div_pd(_mm512_fmadd_pd(zmm3,r,zmm2),den)));
                        _mm512_storeu_pd(&zim[0], _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmsub_pd(zmm3,r,zmm2),den),
                                                _mm512_div_pd(_mm512_sub_pd(zmm3,_mm512_mul_pd(zmm2,r)),den)));
               }


                  
                   void cdiv_smith_zmm8r8(const __m512d xre,
                                           const __m512d xim,
                                           const __m512d yre,
                                           const __m512d yim,
                                           __m512d * __restrict zre,
                                           __m512d * __restrict zim) {

                        register __m512d r,den;
                        __mmask8 m = 0x0;
                        m    = _mm512_cmp_pd_mask(_mm512_abs_pd(yre),
                                                  _mm512_abs_pd(yim),
                                                  _CMP_GE_OQ);
                        r    = _mm512_mask_blend_pd(m,_mm512_div_pd(yre,yim),
                                                      _mm512_div_pd(yim,yre)); // r
                        den  = _mm512_mask_blend_pd(m,_mm512_fmadd_pd(r,yre,yim),
                                                      _mm512_fmadd_pd(r,yim,yre));
                        *zre  =  _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmadd_pd(xre,r,xim),den),
                                                _mm512_div_pd(_mm512_fmadd_pd(xim,r,xre),den));
                        *zim  =  _mm512_mask_blend_pd(m,
                                                _mm512_div_pd(_mm512_fmsub_pd(xim,r,xre),den),
                                                _mm512_div_pd(_mm512_sub_pd(xim,_mm512_mul_pd(xre,r)),den)));
               }

#include "GMS_sleefsimddp.h"


               /////////////////////////////////////////////////////////////////////////////////////


                 
                   void cabs_zmm8r8_u(const double * __restrict re,
                                       const double * __restrict im,
                                       double * __restrict  cabs) {

                        register __m512d zmm0,zmm1,zmm2,zmm3,zmm4;
                        zmm0  = _mm512_loadu_pd(&re[0]);
                        zmm1  = _mm512_mul_pd(zmm0,zmm0);
                        zmm2  = _mm512_loadu_pd(&im[0]);
                        zmm3  = _mm512_mul_pd(zmm2,zmm2);
                        zmm4  = xsqrt(_mm512_add_pd(zmm1,zmm3));
                        _mm512_storeu_pd(&cabs[0],zmm4);
                 }


                 
                   void cabs_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) re,
                                       const double * __restrict __attribute__((aligned(64))) im,
                                       double * __restrict  __attribute__((aligned(64))) cabs) {

                        register __m512d zmm0,zmm1,zmm2,zmm3,zmm4;
                        zmm0  = _mm512_load_pd(&re[0]);
                        zmm1  = _mm512_mul_pd(zmm0,zmm0);
                        zmm2  = _mm512_load_pd(&im[0]);
                        zmm3  = _mm512_mul_pd(zmm2,zmm2);
                        zmm4  = xsqrt(_mm512_add_pd(zmm1,zmm3));
                        _mm512_store_pd(&cabs[0],zmm4);
                 }


                  
                   __m512d cabs_zmm8r8(const __m512d re,
                                       const __m512d im) {

                        register __m512d zmm0,zmm1,cabs;
                        zmm0 = _mm512_mul_pd(re,re);
                        zmm1 = _mm512_mul_pd(im,im);
                        cabs = xsqrt(_mm512_add_pd(zmm0,zmm1));
                        return (cabs);
                 }


              ////////////////////////////////////////////////////////////////////////////////


                 
                   void carg_zmm8r8_u(const double * __restrict re,
                                       const double * __restrict im,
                                       double * __restrict  carg) {

                        register __m512d zmm0,zmm1;
                        zmm0 = _mm512_loadu_pd(&re[0]);
                        zmm1 = _mm512_loadu_pd(&im[0]);
                        _mm512_storeu_pd(&carg[0], xatan2(zmm0,zmm1));
                }


                  
                   void carg_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) re,
                                       const double * __restrict __attribute__((aligned(64))) im,
                                       double * __restrict  __attribute__((aligned(64))) carg) {

                        register __m512d zmm0,zmm1;
                        zmm0 = _mm512_load_pd(&re[0]);
                        zmm1 = _mm512_load_pd(&im[0]);
                        _mm512_store_pd(&carg[0], xatan2(zmm0,zmm1));
                }


                 
                   __m512d carg_zmm8r8(const __m512d re,
                                       const __m512d im) {

                       register __m512d carg;
                       carg = xatan2(re,im);
                       return (carg);
                }


             //////////////////////////////////////////////////////////////////////////////


                
                   void cconj_zmm8r8_u(const double * __restrict im,
                                        double * __restrict  conj) {

                        register __m512d zmm0;
                        const register __m512d none = _mm512_set1_pd(-1.0);
                        zmm0 = _mm512_loadu_pd(&im[0]);
                        _mm512_storeu_pd(&conj[0], _mm512_mul_pd(none,zmm0));
               }  


                 
                   void cconj_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) im,
                                        double * __restrict  __attribute__((aligned(64))) conj) {

                        register __m512d zmm0;
                        const register __m512d none = _mm512_set1_pd(-1.0);
                        zmm0 = _mm512_load_pd(&im[0]);
                        _mm512_store_pd(&conj[0], _mm512_mul_pd(none,zmm0));
               }  


                  
                   __m512d cconj_zmm8r8(const __m512d im) {
                          
                         const register __m512d none = _mm512_set1_pd(-1.0);
                         register __m512d conj;
                         conj = _mm512_mul_pd(none,im);
                         return (conj); 
               }  


              //////////////////////////////////////////////////////////////////


                   void ccos_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xcos(zmm0),xcosh(zmm1));
                      _mm512_storeu_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xsin(zmm0),xsinh(zmm1));
                      _mm512_storeu_pd(&csim[0],zmm3);
               }


                 
                   void ccos_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_load_pd(&xre[0]);
                      zmm1  = _mm512_load_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xcos(zmm0),xcosh(zmm1));
                      _mm512_store_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xsin(zmm0),xsinh(zmm1));
                      _mm512_store_pd(&csim[0],zmm3);
               }


                
                   void ccos_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim) {

                      register __m512d zmm0,zmm1;
                      zmm0  = _mm512_mul_pd(xcos(xre),xcosh(xim));
                      *csre = zmm0;
                      zmm1  = _mm512_mul_pd(xsin(xre),xsinh(xim));
                      *csim = zmm1; 
               }


               ////////////////////////////////////////////////////////////////////////////////


                void csin_zmm8r8_u(   const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim)  {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xsin(zmm0),xcosh(zmm1));
                      _mm512_storeu_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xcos(zmm0),xsinh(zmm1));
                      _mm512_storeu_pd(&csim[0],zmm3);
               }


                void csin_zmm8r8_a(   const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim)  {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_load_pd(&xre[0]);
                      zmm1  = _mm512_load_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xsin(zmm0),xcosh(zmm1));
                      _mm512_store_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xcos(zmm0),xsinh(zmm1));
                      _mm512_store_pd(&csim[0],zmm3);
               }


               void csin_zmm8r8(    const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim) {

                      register __m512d zmm0,zmm1;
                      zmm0  = _mm512_mul_pd(xsin(xre),xcosh(xim));
                      *csre = zmm0;
                      zmm1  = _mm512_mul_pd(xcos(xre),xsinh(xim));
                      *csim = zmm1; 
               }

            ///////////////////////////////////////////////////////////////////////


               void csinh_zmm8r8_u(   const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xsinh(zmm0),xcos(zmm1));
                      _mm512_storeu_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xcosh(zmm0),xsin(zmm1));
                      _mm512_storeu_pd(&csim[0],zmm3);
                }


               void csinh_zmm8r8_a(   const double * __restrict  __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim) {


                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_load_pd(&xre[0]);
                      zmm1  = _mm512_load_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xsinh(zmm0),xcos(zmm1));
                      _mm512_store_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xcosh(zmm0),xsin(zmm1));
                      _mm512_store_pd(&csim[0],zmm3);
                }


               void csinh_zmm8r8(   const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim)  {

                      register __m512d zmm0,zmm1;
                      zmm0  = _mm512_mul_pd(xsinh(xre),xcosh(xim));
                      *csre = zmm0;
                      zmm1  = _mm512_mul_pd(xcosh(xre),xsinh(xim));
                      *csim = zmm1; 
                }

            ///////////////////////////////////////////////////////////////////////
                
                   void ccosh_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xcosh(zmm0),xcos(zmm1));
                      _mm512_storeu_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xsinh(zmm0),xsin(zmm1));
                      _mm512_storeu_pd(&csim[0],zmm3);
               }


                  
                   void ccosh_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0  = _mm512_load_pd(&xre[0]);
                      zmm1  = _mm512_load_pd(&xim[0]);
                      zmm2  = _mm512_mul_pd(xcosh(zmm0),xcos(zmm1));
                      _mm512_store_pd(&csre[0],zmm2);
                      zmm3  = _mm512_mul_pd(xsinh(zmm0),xsin(zmm1));
                      _mm512_store_pd(&csim[0],zmm3);
               }


                   
                   void ccosh_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim) {

                      register __m512d zmm0,zmm1;
                      zmm0  = _mm512_mul_pd(xcosh(xre),xcos(xim));
                      *csre = zmm0;
                      zmm1  = _mm512_mul_pd(xsinh(xre),xsin(xim));
                      *csim = zmm1; 
               }


             ////////////////////////////////////////////////////////////////////////////////////


                 void cpow_zmm8r8_u(      const double * __restrict xre,
                                           const double * __restrict xim,
                                           const double n,
                                           double * __restrict powr,
                                           double * __restrict powi) {

                      register __m512d zmm0,zmm1,r,tht,vn,ptrm,targ,rep,imp;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      rep   = _mm512_mul_pd(zmm0,zmm0);
                      vn    = _mm512_set1_pd(n);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      imp   = _mm512_mul_pd(zmm1,zmm1);
                      r     = _mm512_sqrt_pd(_mm512_add_pd(rep,imp));
                      tht   = xatan(_mm512_div_pd(zmm1,zmm0));
                      ptrm  = _mm512_pow_pd(r,vn);
                      targ  = _mm512_mul_pd(vn,tht);
                      _mm512_storeu_pd(&powr[0],_mm512_mul_pd(ptrm,xcosf(targ)));
                      _mm512_storeu_pd(&powi[0],_mm512_mul_pd(ptrm,xsinf(targ)));
                }


                   void cpow_zmm8r8_a(    const double * __restrict __attribute__((aligned(64)))  xre,
                                           const double * __restrict __attribute__((aligned(64)))  xim,
                                           const double n,
                                           double * __restrict __attribute__((aligned(64)))  powr,
                                           double * __restrict __attribute__((aligned(64)))  powi) {

                      register __m512d zmm0,zmm1,r,tht,vn,ptrm,targ,rep,imp;
                      zmm0  = _mm512_load_pd(&xre[0]);
                      rep   = _mm512_mul_pd(zmm0,zmm0);
                      vn    = _mm512_set1_pd(n);
                      zmm1  = _mm512_load_pd(&xim[0]);
                      imp   = _mm512_mul_pd(zmm1,zmm1);
                      r     = _mm512_sqrt_pd(_mm512_add_pd(rep,imp));
                      tht   = xatan(_mm512_div_pd(zmm1,zmm0));
                      ptrm  = _mm512_pow_pd(r,vn);
                      targ  = _mm512_mul_pd(vn,tht);
                      _mm512_store_pd(&powr[0],_mm512_mul_pd(ptrm,xcosf(targ)));
                      _mm512_store_pd(&powi[0],_mm512_mul_pd(ptrm,xsinf(targ)));
                }


                 void cpow_zmm8r8(const __m512d xre,
                                   const __m512d xim,
                                   const double n,
                                   __m512d * __restrict powr,
                                   __m512d * __restrict powi) {

                      register __m512d r,tht,vn,ptrm,targ,rep,imp;
                      rep   = _mm512_mul_pd(xre,xre);
                      vn    = _mm512_set1_pd(n);
                      imp   = _mm512_mul_pd(xim,xim);
                      r     = _mm512_sqrt_pd(_mm512_add_pd(rep,imp));
                      tht   = xatan(_mm512_div_pd(xim,xre));
                      ptrm  = _mm512_pow_pd(r,vn);
                      targ  = _mm512_mul_pd(vn,tht);
                      *powr = _mm512_mul_pd(ptrm,xcosf(targ));
                      *powi = _mm512_mul_pd(ptrm,xsinf(targ));
               }


             ////////////////////////////////////////////////////////////////////////////////////
 

                  void clog_zmm8r8_u(  const double * __restrict xre,
                                        const double * __restrict xim,
                                        double * __restrict logr,
                                        double * __restrict logi) {

                      register __m512d zmm0,zmm1,cabs,carg,rep;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      cabs  = cabs_zmm8r8(zmm0,zmm1);
                      carg  = carg_zmm8r8(zmm0,zmm1);
                      rep   = xlog(cabs);
                      _mm512_storeu_pd(&logr[0],rep);
                      _mm512_storeu_pd(&logi[0],carg); 
               }


                   void clog_zmm8r8_u(  const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double * __restrict __attribute__((aligned(64))) logr,
                                         double * __restrict __attribute__((aligned(64))) logi) {

                      register __m512d zmm0,zmm1,cabs,carg,rep;
                      zmm0  = _mm512_loadu_pd(&xre[0]);
                      zmm1  = _mm512_loadu_pd(&xim[0]);
                      cabs  = cabs_zmm8r8(zmm0,zmm1);
                      carg  = carg_zmm8r8(zmm0,zmm1);
                      rep   = xlog(cabs);
                      _mm512_storeu_pd(&logr[0],rep);
                      _mm512_storeu_pd(&logi[0],carg); 
               }


                   void clog_zmm8r8(    const __m512d xre,
                                         const __m512d xim,
                                         __m512d * __restrict logr,
                                         __m512d * __restrict logi) {

                      register __m512d cabs,carg,rep;
                      cabs  = cabs_zmm8r8(zmm0,zmm1);
                      carg  = carg_zmm8r8(zmm0,zmm1);
                      rep   = xlog(cabs);
                      *logr = rep;
                      *logi = carg; 
               }


             ////////////////////////////////////////////////////////////////////////////////////


                  void ctan_zmm8r8_u( const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict tanr,
                                       double * __restrict tani) {

                        register __m512d zmm0,zmm1,sinr,sini,cosr,cosi,resr,resi;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&xim[0]);
                        csin_zmm8r8(zmm0,zmm1,&sinr,&sini);
                        ccos_zmm8r8(zmm0,zmm1,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        _mm512_storeu_pd(&tanr[0], resr);
                        _mm512_storeu_pd(&tani[0], resi);
                }


                  void ctan_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict __attribute__((aligned(64))) tanr,
                                       double * __restrict __attribute__((aligned(64))) tani) {

                        register __m512d zmm0,zmm1,sinr,sini,cosr,cosi,resr,resi;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&xim[0]);
                        csin_zmm8r8(zmm0,zmm1,&sinr,&sini);
                        ccos_zmm8r8(zmm0,zmm1,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        _mm512_store_pd(&tanr[0], resr);
                        _mm512_store_pd(&tani[0], resi);
                }


                 void ctan_zmm8r8(   const __m512d xre,
                                      const __m512d xim,
                                      __m512d * __restrict tanr,
                                      __m512d * __restrict tani)  {

                        register __m512d sinr,sini,cosr,cosi,resr,resi;
                        csin_zmm8r8(xre,xim,&sinr,&sini);
                        ccos_zmm8r8(xre,xim,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        *tanr = resr;
                        *tani = resi; 
               }

            ////////////////////////////////////////////////////////////////////////////////////


                 void ctanh_zmm8r8_u( const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict tanr,
                                       double * __restrict tani) {

                        register __m512d zmm0,zmm1,sinr,sini,cosr,cosi,resr,resi;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&xim[0]);
                        csinh_zmm8r8(zmm0,zmm1,&sinr,&sini);
                        ccosh_zmm8r8(zmm0,zmm1,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        _mm512_storeu_pd(&tanr[0], resr);
                        _mm512_storeu_pd(&tani[0], resi);
                }


                  void ctanh_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict __attribute__((aligned(64))) tanr,
                                       double * __restrict __attribute__((aligned(64))) tani) {

                        register __m512d zmm0,zmm1,sinr,sini,cosr,cosi,resr,resi;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&xim[0]);
                        csinh_zmm8r8(zmm0,zmm1,&sinr,&sini);
                        ccosh_zmm8r8(zmm0,zmm1,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        _mm512_store_pd(&tanr[0], resr);
                        _mm512_store_pd(&tani[0], resi);
                }


                 void ctanh_zmm8r8(   const __m512d xre,
                                      const __m512d xim,
                                      __m512d * __restrict tanr,
                                      __m512d * __restrict tani)  {

                        register __m512d sinr,sini,cosr,cosi,resr,resi;
                        csinh_zmm8r8(xre,xim,&sinr,&sini);
                        ccosh_zmm8r8(xre,xim,&cosr,&cosi);
                        cdiv_zmm8r8(sinr,sini,cosr,cosi,&resr,&resi);
                        *tanr = resr;
                        *tani = resi; 
               }



           ////////////////////////////////////////////////////////////////////////////////////


              
                   void ceq_zmm8r8_u(const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask8 * __restrict eqr,
                                      __mmask8 * __restrict eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_loadu_pd(&xre[0]);
                      zmm1 = _mm512_loadu_pd(&yre[0]);
                      _mm512_storeu_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_EQ_OQ));
                      zmm2 = _mm512_loadu_pd(&xim[0]);
                      zmm3 = _mm512_loadu_pd(&yim[0]);
                      _mm512_storeu_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_EQ_OQ));
              }


                 
                   void ceq_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqr,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_load_pd(&xre[0]);
                      zmm1 = _mm512_load_pd(&yre[0]);
                      _mm512_store_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_EQ_OQ));
                      zmm2 = _mm512_load_pd(&xim[0]);
                      zmm3 = _mm512_load_pd(&yim[0]);
                      _mm512_store_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_EQ_OQ));
              }


                
                   void ceq_zmm8r8(const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask8 * __restrict eqr,
                                    __mmask8 * __restrict eqi) {

                         *eqr = _mm512_cmp_pd_mask(xre,yre,_CMP_EQ_OQ);
                         *eqi = _mm512_cmp_pd_mask(xim,yim,_CMP_EQ_OQ);
              }


              ////////////////////////////////////////////////////////////////////
                  
                   void cgt_zmm8r8_u(const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask8 * __restrict eqr,
                                      __mmask8 * __restrict eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_loadu_pd(&xre[0]);
                      zmm1 = _mm512_loadu_pd(&yre[0]);
                      _mm512_storeu_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_GT_OQ));
                      zmm2 = _mm512_loadu_pd(&xim[0]);
                      zmm3 = _mm512_loadu_pd(&yim[0]);
                      _mm512_storeu_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_GT_OQ));
              }


                 
                   void cgt_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqr,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_load_pd(&xre[0]);
                      zmm1 = _mm512_load_pd(&yre[0]);
                      _mm512_store_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_GT_OQ));
                      zmm2 = _mm512_load_pd(&xim[0]);
                      zmm3 = _mm512_load_pd(&yim[0]);
                      _mm512_store_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_GT_OQ));
              }


                 
                   void cgt_zmm8r8(const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask8 * __restrict eqr,
                                    __mmask8 * __restrict eqi) {

                         *eqr = _mm512_cmp_pd_mask(xre,yre,_CMP_GT_OQ);
                         *eqi = _mm512_cmp_pd_mask(xim,yim,_CMP_GT_OQ);
              }


              //////////////////////////////////////////////////////////////////////////


                   void clt_zmm8r8_u(const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask8 * __restrict eqr,
                                      __mmask8 * __restrict eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_loadu_pd(&xre[0]);
                      zmm1 = _mm512_loadu_pd(&yre[0]);
                      _mm512_storeu_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_LT_OQ));
                      zmm2 = _mm512_loadu_pd(&xim[0]);
                      zmm3 = _mm512_loadu_pd(&yim[0]);
                      _mm512_storeu_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_LT_OQ));
              }


               
                   void clt_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqr,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_load_pd(&xre[0]);
                      zmm1 = _mm512_load_pd(&yre[0]);
                      _mm512_store_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_LT_OQ));
                      zmm2 = _mm512_load_pd(&xim[0]);
                      zmm3 = _mm512_load_pd(&yim[0]);
                      _mm512_store_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_LT_OQ));
              }


                 
                   void clt_zmm8r8(const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask8 * __restrict eqr,
                                    __mmask8 * __restrict eqi) {

                         *eqr = _mm512_cmp_pd_mask(xre,yre,_CMP_LT_OQ);
                         *eqi = _mm512_cmp_pd_mask(xim,yim,_CMP_LT_OQ);
              }


               /////////////////////////////////////////////////////////////////////


                 
                   void cneq_zmm8r8_u(const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask8 * __restrict eqr,
                                      __mmask8 * __restrict eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_loadu_pd(&xre[0]);
                      zmm1 = _mm512_loadu_pd(&yre[0]);
                      _mm512_storeu_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_NEQ_OQ));
                      zmm2 = _mm512_loadu_pd(&xim[0]);
                      zmm3 = _mm512_loadu_pd(&yim[0]);
                      _mm512_storeu_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_NEQ_OQ));
              }


                 
                   void cneq_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqr,
                                      __mmask8 * __restrict __attribute__((aligned(64))) eqi ) {

                      register __m512d zmm0,zmm1,zmm2,zmm3;
                      zmm0 = _mm512_load_pd(&xre[0]);
                      zmm1 = _mm512_load_pd(&yre[0]);
                      _mm512_store_pd(&eqr[0],
                                       _mm512_cmp_pd_mask(zmm0,zmm1,_CMP_NEQ_OQ));
                      zmm2 = _mm512_load_pd(&xim[0]);
                      zmm3 = _mm512_load_pd(&yim[0]);
                      _mm512_store_pd(&eqi[0],
                                       _mm512_cmp_pd_mask(zmm2,zmm3,_CMP_NEQ_OQ));
              }


                  
                   void cneq_zmm8r8(const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask8 * __restrict eqr,
                                    __mmask8 * __restrict eqi) {

                         *eqr = _mm512_cmp_pd_mask(xre,yre,_CMP_NEQ_OQ);
                         *eqi = _mm512_cmp_pd_mask(xim,yim,_CMP_NEQ_OQ);
              }


               ////////////////////////////////////////////////////////////////////////////


               
                   void cexp_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict cexpr,
                                       double * __restrict cexpi ) {

                        register const __m512d I = _mm512_set1_pd(1.0);
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&xim[0]);
                        zmm2  = xexp(zmm0);
                        zmm3  = _mm512_mul_pd(zmm2,xcos(zmm1));
                        _mm512_storeu_pd(&cexpr[0],zmm3);
                        zmm4  = _mm512_mul_pd(zmm2,_mm512_mul_pd(xsin(zmm1),I));
                        _mm512_storeu_pd(&cexpi[0],zmm4);
              }


                 
                   void cexp_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict __attribute__((aligned(64))) cexpr,
                                       double * __restrict __attribute__((aligned(64))) cexpi ) {

                        register const __m512d I = _mm512_set1_pd(1.0);
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&xim[0]);
                        zmm2  = xexp(zmm0);
                        zmm3  = _mm512_mul_pd(zmm2,xcos(zmm1));
                        _mm512_store_pd(&cexpr[0],zmm3);
                        zmm4  = _mm512_mul_pd(zmm2,_mm512_mul_pd(xsin(zmm1),I));
                        _mm512_store_pd(&cexpi[0],zmm4);
              }


                  
                   void cexp_zmm8r8(const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict cexpr,
                                     __m512d * __restrict cexpi) {

                        register const __m512d I = _mm512_set1_pd(1.0);
                        register __m512d zmm0;
                        zmm0   = xexp(xre);
                        *cexpr = _mm512_mul_pd(zmm0,xcos(xim));
                        *cexpi = _mm512_mul_pd(zmm0,_mm512_mul_pd(xsin(xim),I));
              }


               ////////////////////////////////////////////////////////////////////


              
                   void cpolar_zmm8r8_u(const double * __restrict rho,
                                         const double * __restrict tht,
                                         double * __restrict  re,
                                         double * __restrict  im) {

                         register __m512d zmm0,zmm1,zmm2,zmm3;
                         zmm0 = _mm512_loadu_pd(&rho[0]);
                         zmm1 = _mm512_loadu_pd(&tht[0]);
                         zmm2 = _mm512_mul_pd(zmm0,xcos(zmm1)); //tht
                         _mm512_storeu_pd(&re[0],zmm2);
                         zmm3 = _mm512_mul_pd(zmm0,xsin(zmm1)); //tht
                         _mm512_storeu_pd(&im[0],zmm3);
              }


                 
                   void cpolar_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) rho,
                                         const double * __restrict __attribute__((aligned(64))) tht,
                                         double * __restrict  __attribute__((aligned(64))) re,
                                         double * __restrict  __attribute__((aligned(64))) im) {

                         register __m512d zmm0,zmm1,zmm2,zmm3;
                         zmm0 = _mm512_load_pd(&rho[0]);
                         zmm1 = _mm512_load_pd(&tht[0]);
                         zmm2 = _mm512_mul_pd(zmm0,xcos(zmm1)); //tht
                         _mm512_store_pd(&re[0],zmm2);
                         zmm3 = _mm512_mul_pd(zmm0,xsin(zmm1)); //tht
                         _mm512_store_pd(&im[0],zmm3);
              }


                 
                   void cpolar_zmm8r8(const __m512d rho,
                                       const __m512d tht,
                                       __m512d * __restrict re,
                                       __m512d * __restrict im) {

                        register __m512d zmm0,zmm1;
                        zmm0 = _mm512_mul_pd(rho,xcos(tht));
                        *re  = zmm0;
                        zmm1 = _mm512_mul_pd(rho,xsin(tht));
                        *im  = zmm1;
              }


             ///////////////////////////////////////////////////////////////////////////


              
                   void csqrt_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict wrkc,
                                       double * __restrict csqr,
                                       double * __restrict csqi) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        const register __m512d half = _mm512_set1_pd(0.5);
                        cabs_zmm8r8_u(xre,xim,wrkc);
                        zmm0  = _mm512_loadu_pd(&xre[0]);
                        zmm1  = _mm512_loadu_pd(&wrkc[0]);
                        zmm2  = _mm512_mul_pd(half,_mm512_add_pd(zmm1,zmm0));
                        _mm512_storeu_pd(&csqr[0],_mm512_sqrt_pd(zmm2));
                        zmm3  = _mm512_mul_pd(half,_mm512_sub_pd(zmm1,zmm0));
                        _mm512_storeu_pd(&csqi[0],_mm512_sqrt_pd(zmm3));
              }


                 
                   void csqrt_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict __attribute__((aligned(64))) wrkc,
                                       double * __restrict __attribute__((aligned(64))) csqr,
                                       double * __restrict __attribute__((aligned(64))) csqi) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        const register __m512d half = _mm512_set1_pd(0.5);
                        cabs_zmm8r8_a(xre,xim,wrkc);
                        zmm0  = _mm512_load_pd(&xre[0]);
                        zmm1  = _mm512_load_pd(&wrkc[0]);
                        zmm2  = _mm512_mul_pd(half,_mm512_add_pd(zmm1,zmm0));
                        _mm512_store_pd(&csqr[0],_mm512_sqrt_pd(zmm2));
                        zmm3  = _mm512_mul_pd(half,_mm512_sub_pd(zmm1,zmm0));
                        _mm512_store_pd(&csqi[0],_mm512_sqrt_pd(zmm3));
              }


                 
                   void csqrt_zmm8r8(const __m512d xre,
                                      const __m512d xim,
                                      __m512d * __restrict wrkc,
                                      __m512d * __restrict csqr,
                                      __m512d * __restrict csqi) {

                       register __m512d zmm0,zmm1;
                       const register __m512d half = _mm512_set1_pd(0.5); 
                       cabs_zmm8r8(xre,xim,wrkc);
                       zmm0  = _mm512_mul_pd(half,_mm512_add_pd(*wrkc,xre));
                       *csqr = zmm0;
                       zmm1  = _mm512_mul_pd(half,_mm512_sub_pd(*wrkc,xre));
                       *csqi = zmm1; 
              }


               ////////////////////////////////////////////////////////////////////////////


              
                
                   void cnorm_prod_zmm8r8_u(const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict zre,
                                             double * __restrict zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        zmm0 = _mm512_mul_pd(rep,rep);
                        zmm1 = _mm512_mul_pd(imp,imp);
                        zmm2 = _mm512_sqrt_pd(_mm512_add_pd(zmm0,zmm1));
                        _mm512_storeu_pd(&zre[0], _mm512_div_pd(rep,zmm2));
                        _mm512_storeu_pd(&zim[0], _mm512_div_pd(imp,zmm2));
              }


                  
                   void cnorm_prod_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                             const double * __restrict __attribute__((aligned(64))) xim,
                                             const double * __restrict __attribute__((aligned(64))) yre,
                                             const double * __restrict __attribute__((aligned(64))) yim,
                                             double * __restrict __attribute__((aligned(64))) zre,
                                             double * __restrict __attribute__((aligned(64))) zim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        zmm0 = _mm512_mul_pd(rep,rep);
                        zmm1 = _mm512_mul_pd(imp,imp);
                        zmm2 = _mm512_sqrt_pd(_mm512_add_pd(zmm0,zmm1));
                        _mm512_store_pd(&zre[0], _mm512_div_pd(rep,zmm2));
                        _mm512_store_pd(&zim[0], _mm512_div_pd(imp,zmm2));
              }


                
                   void cnorm_prod_zmm8r8(  const __m512d  xre,
                                             const __m512d  xim,
                                             const __m512d  yre,
                                             const __m512d  yim,
                                             __m512d * __restrict zre,
                                             __m512d * __restrict zim) {

                        register __m512d rep,imp,zmm0,zmm1,zmm2;
                        rep  = _mm512_fmsub_pd(xre,yre,
                                               _mm512_mul_pd(xim,yim));
                        imp  = _mm512_fmadd_pd(xim,yre,
                                               _mm512_mul_pd(xre,yim));
                        zmm0 = _mm512_mul_pd(rep,rep);
                        zmm1 = _mm512_mul_pd(imp,imp);
                        zmm2 = _mm512_sqrt_pd(_mm512_add_pd(zmm0,zmm1));
                        *zre = _mm512_div_pd(rep,zmm2);
                        *zim = _mm512_div_pd(imp,zmm2);
             }


               ///////////////////////////////////////////////////////////////////////////


            
                  
                   void cmean_prod_zmm8r8_u(const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict mre,
                                             double * __restrict mim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        sre = 0.0;
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        sre  = _mm512_reduce_add_pd(rep);
                        *mre = sre*inv16;
                        sim  = 0.0;
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = _mm512_reduce_add_pd(imp);
                        *mim = sim*inv16;
              }


                
                   void cmean_prod_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                             const double * __restrict __attribute__((aligned(64))) xim,
                                             const double * __restrict __attribute__((aligned(64))) yre,
                                             const double * __restrict __attribute__((aligned(64))) yim,
                                             double * __restrict mre,
                                             double * __restrict mim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        sre = 0.0;
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        sre  = _mm512_reduce_add_pd(rep);
                        *mre = sre*inv16;
                        sim  = 0.0;
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = _mm512_reduce_add_pd(imp);
                        *mim = sim*inv16;
              } 


                 
                   void cmean_prod_zmm8r8(const __m512d xre,
                                           const __m512d xim,
                                           const __m512d yre,
                                           const __m512d yim,
                                           double * __restrict mre,
                                           double * __restrict min) {

                        register __m512d rep,imp;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        sre = 0.0;
                        rep  = _mm512_fmsub_pd(xre,yre,
                                               _mm512_mul_pd(xim,yim));
                        sre  = _mm512_reduce_add_pd(rep);
                        *mre = sre*inv16;
                        sim  = 0.0;
                        imp  = _mm512_fmadd_pd(xim,yre,
                                               _mm512_mul_pd(xre,yim));
                        sim  = _mm512_reduce_add_pd(imp);
                        *mim = sim*inv16;
             }


             ////////////////////////////////////////////////////////////////////////////


               
                   void cmean_quot_zmm8r8_u(const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict mre,
                                             double * __restrict mim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp,den,rquot,iquot;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        sre  = 0.0;
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = 0.0;
                        den  = _mm512_fmadd_pd(zmm1,zmm1,
                                               _mm512_mul_pd(zmm3,zmm3));
                        rquot = _mm512_div_pd(rep,den);
                        sre   = _mm512_reduce_add_pd(rquot);
                        *mre  = sre*inv16;
                        iquot = _mm512_div_pd(imp,den);
                        sim   = _mm512_reduce_add_pd(iquot);
                        *mim  = sre*inv16;
              }  


                 
                   void cmean_quot_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                             const double * __restrict __attribute__((aligned(64))) xim,
                                             const double * __restrict __attribute__((aligned(64))) yre,
                                             const double * __restrict __attribute__((aligned(64))) yim,
                                             double * __restrict mre,
                                             double * __restrict mim) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp,den,rquot,iquot;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        sre  = 0.0;
                        rep  = _mm512_fmsub_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        imp  = _mm512_fmadd_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = 0.0;
                        den  = _mm512_fmadd_pd(zmm1,zmm1,
                                               _mm512_mul_pd(zmm3,zmm3));
                        rquot = _mm512_div_pd(rep,den);
                        sre   = _mm512_reduce_add_pd(rquot);
                        *mre  = sre*inv16;
                        iquot = _mm512_div_pd(imp,den);
                        sim   = _mm512_reduce_add_pd(iquot);
                        *mim  = sre*inv16;
              }  


                  
                   void cmean_quot_zmm8r8(  const __m512d xre,
                                             const __m512d xim,
                                             const __m512d yre,
                                             const __m512d yim,
                                             double * __restrict mre,
                                             double * __restrict mim) {

                        register __m512d rep,imp,den,rquot,iquot;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        sre  = 0.0;
                        rep  = _mm512_fmsub_pd(xre,yre,
                                               _mm512_mul_pd(xim,yim));
                        imp  = _mm512_fmadd_pd(xim,yre,
                                               _mm512_mul_pd(xre,yim));
                        sim  = 0.0;
                        den  = _mm512_fmadd_pd(yre,yre,
                                               _mm512_mul_pd(yim,yim));
                        rquot = _mm512_div_pd(rep,den);
                        sre   = _mm512_reduce_add_pd(rquot);
                        *mre  = sre*inv16;
                        iquot = _mm512_div_pd(imp,den);
                        sim   = _mm512_reduce_add_pd(iquot);
                        *mim  = sre*inv16;
              }  


               //////////////////////////////////////////////////////////////


                
                   void cnorm_cprod_zmm8r8_u(const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim ) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp,magc1,magc2,vcmag;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        rep  = _mm512_fmadd_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        magc1= _mm512_mul_pd(rep,rep);
                        imp  = _mm512_fmsub_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        magc2= _mm512_mul_pd(imp,imp);
                        vcmag= _mm512_sqrt_pd(_mm512_add_pd(magc1,magc2));
                        _mm512_storeu_pd(&mre[0], _mm512_div_pd(rep,vcmag));
                        _mm512_storeu_pd(&mim[0], _mm512_div_pd(imp,vcmag));
             }


                 
                   void cnorm_cprod_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              const double * __restrict __attribute__((aligned(64))) yre,
                                              const double * __restrict __attribute__((aligned(64))) yim,
                                              double * __restrict __attribute__((aligned(64))) mre,
                                              double * __restrict __attribute__((aligned(64))) mim ) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d rep,imp,magc1,magc2,vcmag;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        rep  = _mm512_fmad_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        magc1= _mm512_mul_pd(rep,rep);
                        imp  = _mm512_fmsub_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        magc2= _mm512_mul_pd(imp,imp);
                        vcmag= _mm512_sqrt_pd(_mm512_add_pd(magc1,magc2));
                        _mm512_store_pd(&mre[0], _mm512_div_pd(rep,vcmag));
                        _mm512_store_pd(&mim[0], _mm512_div_pd(imp,vcmag));
             }


                
                   void cnorm_cprod_zmm8r8(const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            __m512d * __restrict mre,
                                            __m512d * __restrict mim) {

                        register __m512d rep,imp,magc1,magc2,vcmag;
                        rep  = _mm512_fmad_pd(xre,yre,
                                               _mm512_mul_pd(xim,yim));
                        magc1= _mm512_mul_pd(rep,rep);
                        imp  = _mm512_fmsub_pd(xim,yre,
                                               _mm512_mul_pd(xre,yim));
                        magc2= _mm512_mul_pd(imp,imp);
                        vcmag= _mm512_sqrt_pd(_mm512_add_pd(magc1,magc2));
                        *mre = _mm512_div_pd(rep,vcmag);
                        *mim = _mm512_div_pd(imp,vcmag)
             }


              ///////////////////////////////////////////////////////////////////


               
                   void cmean_cprod_zmm8r8_u(const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim) {
                      
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d re,im;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        re   = _mm512_fmadd_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        sre  = _mm512_reduce_add_pd(re);
                        *mre = sre*inv16;
                        im   = _mm512_fmsub_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = _mm512_reduce_add_pd(im);
                        *mim = sim*inv16;
             }


                  
                   void cmean_cprod_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              const double * __restrict __attribute__((aligned(64))) yre,
                                              const double * __restrict __attribute__((aligned(64))) yim,
                                              double * __restrict mre,
                                              double * __restrict mim) {
                      
                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d re,im;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        re   = _mm512_fmadd_pd(zmm0,zmm1,
                                               _mm512_mul_pd(zmm2,zmm3));
                        sre  = _mm512_reduce_add_pd(re);
                        *mre = sre*inv16;
                        im   = _mm512_fmsub_pd(zmm2,zmm1,
                                               _mm512_mul_pd(zmm0,zmm3));
                        sim  = _mm512_reduce_add_pd(im);
                        *mim = sim*inv16;
             }


                  
                   void cmean_cprod_zmm8r8(const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            double * __restrict mre,
                                            double * __restrict min) {

                        register __m512d re,im;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        re   = _mm512_fmadd_pd(xre,yre,
                                               _mm512_mul_pd(xim,yim));
                        sre  = _mm512_reduce_add_pd(re);
                        *mre = sre*inv16;
                        im   = _mm512_fmsub_pd(xim,yre,
                                               _mm512_mul_pd(xre,yim));
                        sim  = _mm512_reduce_add_pd(im);
                        *mim = sim*inv16;
             }

                  
               /////////////////////////////////////////////////////////////////////


               
                   void arith_cmean_zmm8r8_u(const double * __restrict xre,
                                              const double * __restrict xim,
                                              double * __restrict mre,
                                              double * __restrict min) {

                        register __m512d re,im;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        re   = _mm512_loadu_pd(&xre[0]);
                        sre  = _mm512_reduce_add_pd(re);
                        *mre = sre*inv16;
                        im   = _mm512_loadu_pd(&xim[0]);
                        sim  = _mm512_reduce_add_pd(im);
                        *mim = sim*inv16; 
             }


                 
                   void arith_cmean_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              double * __restrict mre,
                                              double * __restrict min) {

                        register __m512d re,im;
                        const double inv16 = 0.0625;
                        double sre,sim;
                        re   = _mm512_load_pd(&xre[0]);
                        sre  = _mm512_reduce_add_pd(re);
                        *mre = sre*inv16;
                        im   = _mm512_load_pd(&xim[0]);
                        sim  = _mm512_reduce_add_pd(im);
                        *mim = sim*inv16; 
             }


                 
                   void arith_cmean_zmm8r8(  const __m512d xre,
                                              const __m512d xim,
                                              double * __restrict mre,
                                              double * __restrict min) {

                        const double inv16 = 0.0625;
                        double sre,sim;
                        sre  = _mm512_reduce_add_pd(xre);
                        *mre = sre*inv16;
                        sim  = _mm512_reduce_add_pd(xim);
                        *mim = sim*inv16; 
             }


              ///////////////////////////////////////////////////////////////////////////


              
                   void cnormalize_zmm8r8_u( const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim ) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d re,im,cvmag;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(zmm0,zmm1,
                                                              _mm512_mul_pd(zmm2,zmm3)));
                        _mm512_storeu_pd(&mre[0], _mm512_div_pd(zmm0,cvmag));
                        _mm512_storeu_pd(&mim[0], _mm512_div_pd(zmm2,cvmag));
             }


                  
                   void cnormalize_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              const double * __restrict __attribute__((aligned(64))) yre,
                                              const double * __restrict __attribute__((aligned(64))) yim,
                                              double * __restrict __attribute__((aligned(64))) mre,
                                              double * __restrict __attribute__((aligned(64))) mim ) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d re,im,cvmag;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(zmm0,zmm1,
                                                              _mm512_mul_pd(zmm2,zmm3)));
                        _mm512_store_pd(&mre[0], _mm512_div_pd(zmm0,cvmag));
                        _mm512_store_pd(&mim[0], _mm512_div_pd(zmm2,cvmag));
             }


                
                   void cnormalize_zmm8r8( const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            __m512d * __restrict mre,
                                            __m512d * __restrict mim ) {

                        register __m512d re,im,cvmag;
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(xre,yre,
                                                    _mm512_mul_pd(xim,yim)));
                        *mre = _mm512_div_pd(xre,cvmag));
                        *mim =  _mm512_div_pd(xim,cvmag));
             }


                
                   void cmagnitude_zmm8r8_u( const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d cvmag;
                        zmm0 = _mm512_loadu_pd(&xre[0]);
                        zmm1 = _mm512_loadu_pd(&yre[0]);
                        zmm2 = _mm512_loadu_pd(&xim[0]);
                        zmm3 = _mm512_loadu_pd(&yim[0]);
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(zmm0,zmm1,
                                                          _mm512_mul_pd(zmm2,zmm3)));
                        _mm512_storeu_pd(&mre[0], cvmag);
             }


                 
                   void cmagnitude_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              const double * __restrict __attribute__((aligned(64))) yre,
                                              const double * __restrict __attribute__((aligned(64))) yim,
                                              double * __restrict __attribute__((aligned(64))) mre) {

                        register __m512d zmm0,zmm1,zmm2,zmm3;
                        register __m512d cvmag;
                        zmm0 = _mm512_load_pd(&xre[0]);
                        zmm1 = _mm512_load_pd(&yre[0]);
                        zmm2 = _mm512_load_pd(&xim[0]);
                        zmm3 = _mm512_load_pd(&yim[0]);
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(zmm0,zmm1,
                                                          _mm512_mul_pd(zmm2,zmm3)));
                        _mm512_store_pd(&mre[0], cvmag);
             }


                   
                   void cmagnitude_zmm8r8(   const __m512d xre,
                                              const __m512d xim,
                                              const __m512d yre,
                                              const __m512d yim,
                                              __m512d * __restrict  mre) {

                        register __m512d cvmag;
                        cvmag= _mm512_sqrt_pd(_mm512_fmadd_pd(xre,yre,
                                                          _mm512_mul_pd(xim,yim)));
                        *mre = cvmag;
             }



     





  





  
 




   





















