
#include "GMS_stochastic_rk_avx2_ps.h"



                                 /*
                                           Calling svrng_generate4_double function!!
                                           normal  = svrng_new_normal_distribution(0.0,1.0);
					   const double * __restrict ptr = (const double*)&svrng_generate4_double(engine,normal);
					   vrand1 = _mm256_loadu_pd(&ptr[0]);

    The Runge-Kutta scheme is first-order, and suitable for time-invariant
    systems in which F and G do not depend explicitly on time.

    d/dx X(t,xsi) = F ( X(t,xsi) ) + G ( X(t,xsi) ) * w(t,xsi)

                                           Parameters:

    Input, __m256d X, the values at the current time.

    Input, __m256d T, the current time (8 values).

    Input, __m256d H, the time step (8 values).

    Input, __m256d Q, the spectral density of the input white noise (8 values).

    Input, __m256d *FI ( __m256d X ), the name of the deterministic
    right hand side function vector SIMD.

    Input, __m256d  *GI ( __m256d X ), the name of the stochastic
    right hand side function vector SIMD.

   

    Output, __m256d STEP, the 4 values at time T+H.
                                      */
	                          
				    __m256 rk1_ti_step_ymm8r4(const __m256 x,
				                               const __m256 t,
							       const __m256 h,
							       const __m256 q,
							       const __m256 vran, // result of call to svrng_generate4_double(engine,normal)
							       __m256 (*fi) (const __m256),
							       __m256 (*gi) (const __m256)){
							       
							       
                                           register const  __m256 a21 = _mm256_set1_ps(1.0);
					   register const  __m256 q1  = _mm256_set1_ps(1.0);
					   register        __m256 w1;
					   register        __m256 step;
					   register        __m256 k1;
					   register        __m256 tmp0;
					   register        __m256 tmp1;
					   register        __m256 tmp2;
					   register        __m256 tmp3;
					   tmp2 = gi(x);
		                           tmp1  = fi(x);                   
                                  	   tmp0   = _mm256_mul_ps(q1,_mm256_div_ps(q,h));
					   w1     = _mm256_mul_ps(vran,tmp0);
					   tmp3   = _mm256_mul_ps(h,_mm256_mul_ps(tmp2,w1));
					   k1     = _mm256_fmadd_ps(h,tmp1,tmp3);
					   step   = _mm256_fmadd_ps(a21,k1,x);
					   return (step);
				 }

		   /*

    The Runge-Kutta scheme is second-order, and suitable for time-invariant
    systems.

    d/dx X(t,xsi) = F ( X(t,xsi) ) + G ( X(t,xsi) ) * w(t,xsi)
                                           Parameters:

    Input, __m256 X, the values at the current time.

    Input, __m256 T, the current time

    Input, __m256 H, the time step

    Input, __m256 Q, the spectral density of the input white noise

    Input, __m256 *FI ( __m256 X ), the name of the deterministic
    right hand side function vector SIMD.

    Input, __m256  *GI ( __m256 X ), the name of the stochastic
    right hand side function vector SIMD.

   

    Output, __m256 STEP, 8 values at time T+H.
                        */

			           
				    __m256 rk2_ti_step_ymm8r4(const __m256 x,
				                               const __m256 t,
							       const __m256 h,
							       const __m256 q,
							       const __m256 vran1, // result of call to svrng_generate8_float(engine,normal)
							       const __m256 vran2, // result of call to svrng_generate8_float(engine,normal)
							       __m256 (*fi) (const __m256),
							       __m256 (*gi) (const __m256)){
							      
                                        register const __m256 _0  = _mm256_setzero_ps();
                                        register const __m256 a21 = _mm256_set1_ps(1.0);
					register const __m256 a31 = _mm256_set1_ps(0.5);
					register const __m256 a32 = a31;
					register const __m256 q1  = _mm256_set1_ps(2.0);
					register const __m256 q2  = q1;
					const __m256          qh  = _mm256_div_ps(q,h);
					register __m256 tfi       = _0;
					register __m256 tgi       = _0;
					register __m256 w1        = _0;
					register __m256 w2        = _0;
					register __m256 x2        = _0;
					register __m256 k1        = _0;
					register __m256 k2        = _0;
					register __m256 t0        = _0;
					register __m256 t1        = _0;
					register __m256 t2        = _0;
					register __m256 step      = _0;
				        
					tfi = fi(x);
					t0  = _mm256_sqrt_ps(_mm256_mul_ps(q1,qh));
					tgi  = gi(x);
					w1   = _mm256_mul_ps(vran1,t0);
					t1   = _mm256_mul_ps(h,_mm256_mul_pd(tgi,w1));
					k1   = _mm256_fmadd_ps(h,tfi,t1);
					x2   = _mm256_fmadd_ps(a21,k1,x);
					tfi  = fi(x2);
					t0   = _mm256_sqrt_ps(_mm256_mul_ps(q2,qh));
					w2   = _mm256_mul_ps(vran2,t0);
					tgi  = gi(x2);
					t2   = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w2));
					k2   = _mm256_fmadd_ps(h,tfi,t2);
					step = _mm256_fmadd_ps(a32,k2,_mm256_fmadd_ps(a31,k1,x));
					return (step);
				}

/*
    The Runge-Kutta scheme is third-order, and suitable for time-invariant
    systems in which F and G do not depend explicitly on time.

    d/dx X(t,xsi) = F ( X(t,xsi) ) + G ( X(t,xsi) ) * w(t,xsi)
                                            Parameters:

    Input, __m256 X, the values at the current time.

    Input, __m256 T, the current time 

    Input, __m256 H, the time step

    Input, __m256 Q, the spectral density of the input white noise 

    Input, __m256 *FI ( __m256 X ), the name of the deterministic
    right hand side function vector SIMD.

    Input, __m256  *GI ( __m256 X ), the name of the stochastic
    right hand side function vector SIMD.

   

    Output, __m256 STEP, the 8 values at time T+H.
*/

				  
				   __m256 rk3_ti_step_ymm8r4(const __m256 x,
				                              const __m256 t,
							      const __m256 h,
							      const __m256 q,
							      const __m256 vran1,
							      const __m256 vran2,
							      const __m256 vran3,
							      __m256 (*fi) (const __m256),
							      __m256 (*gi) (const __m256)) {
							     
 
                                         const __m256 _0      = _mm256_setzero_ps();
					 const __m256 a21     = _mm256_set1_ps(1.52880952525675F);
					 const __m256 a31     = _0;
					 const __m256 a32     = _mm256_set1_ps(0.51578733443615F);
					 const __m256 a41     = _mm256_set1_ps(0.53289582961739F);
					 const __m256 a42     = _mm256_set1_ps(0.25574324768195F);
					 const __m256 a43     = _mm256_set1_ps(0.21136092270067F);
					 const __m256 q1      = _mm256_set1_ps(1.87653936176981F);
					 const __m256 q2      = _mm256_set1_ps(3.91017166264989F);
					 const __m256 q3      = _mm256_set1_ps(4.73124353935667F);
					 const __m256 qh      = _mm256_div_ps(q,h);
					 register __m256 k1   = _0;
					 register __m256 k2   = _0;
					 register __m256 k3   = _0;
					 register __m256 t0   = _0;
					 register __m256 t1   = _0;
					 register __m256 t2   = _0;
					 register __m256 w1   = _0;
					 register __m256 w2   = _0;
					 register __m256 w3   = _0;
					 register __m256 x2   = _0;
					 register __m256 x3   = _0;
					 register __m256 step = _0;
					 __m256          tfi  = _0;
					 __m256          tgi  = _0;
					 __m256          tmp  = _0;
					 __m256          tmp2 = _0;
					
		                      	 tfi = fi(x);
					 t0  = _mm256_sqrt_ps(_mm256_mul_ps(q1,qh));
					 t1   = _mm256_sqrt_ps(_mm256_mul_ps(q2,qh));
					 t2   = _mm256_sqrt_ps(_mm256_mul_ps(q3,qh));
					 tgi  = gi(x);
					 w1   = _mm256_mul_ps(vran1,t0);
					 tmp  = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w1));
					 k1   = _mm256_fmadd_ps(h,tfi,tmp);
					 tmp2 = _mm256_fmadd_ps(a41,k1,x);
					 x2   = _mm256_fmadd_ps(a21,k1,x);
					 tfi  = fi(x2);
					 w2   = _mm256_mul_ps(vran2,t1);
					 tgi  = gi(x2);
					 tmp  = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w2));
					 k2   = _mm256_fmadd_ps(h,tfi,tmp);
					 x3   = _mm256_fmadd_ps(a32,k2,_mm256_fmadd_ps(a31,k1,x));
					 tfi  = fi(x3);
					 w3   = _mm256_mul_ps(vran3,t2);
					 tgi  = gi(x3);
					 tmp  = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w3));
					 k3   = _mm256_fmadd_ps(h,tfi,tmp);
					 step = _mm256_fmadd_ps(a43,k3,_mm256_fmadd_ps(a42,k2,tmp2));
					 return (step);
				   }

/*
       The Runge-Kutta scheme is third-order, and suitable for time-invariant
    systems in which F and G do not depend explicitly on time.

    d/dx X(t,xsi) = F ( X(t,xsi) ) + G ( X(t,xsi) ) * w(t,xsi)
                                            Parameters:

    Input, __m256 X, the values at the current time.

    Input, __m256 T, the current time 

    Input, __m256 H, the time step

    Input, __m256 Q, the spectral density of the input white noise 

    Input, __m256 *FI ( __m256 X ), the name of the deterministic
    right hand side function vector SIMD.

    Input, __m256  *GI ( __m256 X ), the name of the stochastic
    right hand side function vector SIMD.

   

    Output, __m256d STEP, the 8 values at time T+H.                       
*/

				 
				   __m256 rk4_ti_step_ymm8r4(const __m256 x,
				                              const __m256 t,
							      const __m256 h,
							      const __m256 q,
							      const __m256 vran1,
							      const __m256 vran2,
							      const __m256 vran3,
							      const __m256 vran4,
							      __m256 (*fi) (const __m256),
							      __m256 (*gi) (const __m256)) {

                                           const __m256 _0    = _mm256_setzero_ps();
					   const __m256 a21   = _mm256_set1_ps(2.71644396264860F);
					   const __m256 a31   = _mm256_set1_ps(-6.95653259006152F);
					   const __m256 a32   = _mm256_set1_ps(0.78313689457981F);
					   const __m256 a41   = _0;
					   const __m256 a42   = _mm256_set1_ps(0.48257353309214F);
					   const __m256 a43   = _mm256_set1_ps(0.26171080165848F);
					   const __m256 a51   = _mm256_set1_ps(0.47012396888046F);
					   const __m256 a52   = _mm256_set1_ps(0.36597075368373F);
					   const __m256 a53   = _mm256_set1_ps(0.08906615686702F);
					   const __m256 a54   = _mm256_set1_ps(0.07483912056879F);
					   const __m256 q1    = _mm256_set1_ps(2.12709852335625F);
					   const __m256 q2    = _mm256_set1_ps(2.73245878238737F);
					   const __m256 q3    = _mm256_set1_ps(11.22760917474960F);
					   const __m256 q4    = _mm256_set1_ps(13.36199560336697F);
					   const __m256 qh    = _mm256_div_ps(q,h);
					   register __m256 k1 = _0;
					   register __m256 k2 = _0;
					   register __m256 k3 = _0;
					   register __m256 k4 = _0;
					   register __m256 t1 = _0;
					   register __m256 t2 = _0;
					   register __m256 t3 = _0;
					   register __m256 t4 = _0;
					   register __m256 w1 = _0;
					   register __m256 w2 = _0;
					   register __m256 w3 = _0;
					   register __m256 w4 = _0;
					   register __m256 x2 = _0;
					   register __m256 x3 = _0;
					   register __m256 x4 = _0;
					   register __m256 step = _0;
					   __m256          tfi  = _0;
					   __m256          tgi  = _0;
					   __m256          tmp1 = _0;
					   __m256          tmp2 = _0;
					   tfi  = fi(x);
					   w1   = _mm256_mul_ps(vran1,_mm256_sqrt_ps(_mm256_mul_ps(q1,qh)));
					   tgi  = gi(x);
					   tmp1 = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w1));
					   k1   = _mm256_fmadd_ps(h,tfi,tmp1);
					   x2   = _mm256_fmadd_ps(a21,k1,x);
					   tfi  = fi(x2);
					   w2   = _mm256_mul_ps(vran2,_mm256_sqrt_ps(_mm256_mul_ps(q2,qh)));
					   tgi  = gi(x2);
					   tmp1 = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w2));
					   k2   = _mm256_fmadd_ps(h,tfi,tmp1);
					   x3   = _mm256_fmadd_ps(a32,k2,_mm256_fmadd_ps(a31,k1,x1));
					   tfi  = fi(x3);
					   w3   = _mm256_mul_ps(vran3,_mm256_sqrt_ps(_mm256_mul_ps(q3,qh)));
					   tgi  = gi(x3);
					   tmp1 = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w3));
					   k3   = _mm256_fmadd_ps(h,tfi,tmp1);
					   x4   = _mm256_fmadd_ps(a42,k2,_mm256_fmadd_ps(a41,k1,x1));
					   tfi  = fi(x4);
					   w4   = _mm256_mul_ps(vran4,_mm256_sqrt_ps(_mm256_mul_ps(q4,qh)));
					   tgi  = gi(x4);
					   tmp1 = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w4));
					   k4   = _mm256_fmadd_ps(h,tfi,tmp1);
					   step = _mm256_fmadd_ps(a51,k1,x1);
					   step = _mm256_fmadd_ps(a52,k2,step);
					   step = _mm256_fmadd_ps(a53,k3,step);
					   step = _mm256_fmadd_ps(a54,k4,step);
					   return (step);
				  }

/*
            The Runge-Kutta scheme is fourth-order, and suitable for time-varying
    systems.

    d/dx X(t,xsi) = F ( X(t,xsi), t ) + G ( X(t,xsi), t ) * w(t,xsi)
*/


                                  
				    __m256 rk1_tv_step_ymm8r4(const __m256 x,
				                               const __m256 t,
							       const __m256 h,
							       const __m256 q,
							       const __m256 vran,
							       __m256 (*fi) (const __m256, const __m256),
							       __m256 (*gi) (const __m256, const __m256) ) {
							       
							       
                                           register const  __m256 a21 = _mm256_set1_ps(1.0);
					   register const  __m256 q1  = _mm256_set1_ps(1.0);
					   register        __m256 w1;
					   register        __m256 step;
					   register        __m256 k1;
					   register        __m256 tmp0;
					   register        __m256 tmp1;
					   register        __m256 tmp2;
					   register        __m256 tmp3;
					 
		                           tmp1  = fi(t,x);                   
                                  	   tmp0   = _mm256_mul_ps(q1,_mm256_div_ps(q,h));
					   w1     = _mm256_mul_ps(vran,tmp0);
					   tmp2  = gi(t,x);
					   tmp3   = _mm256_mul_ps(h,_mm256_mul_ps(tmp2,w1));
					   k1     = _mm256_fmadd_ps(h,tmp1,tmp3);
					   step   = _mm256_fmadd_ps(a21,k1,x);
					   return (step);
				 }


				 
                                  
				    __m256 rk2_tv_step_ymm8r4(const __m256 x,
				                               const __m256 t,
							       const __m256 h,
							       const __m256 q,
							       const __m256 vran1,
							       const __m256 vran2,
							       __m256 (*fi) (const __m256, const __m256),
							       __m256 (*gi) (const __m256, const __m256)){
							      
                                        register const __m256 _0  = _mm256_setzero_ps();
                                        register const __m256 a21 = _mm256_set1_ps(1.0);
					register const __m256 a31 = _mm2556_set1_ps(0.5);
					register const __m256 a32 = a31;
					register const __m256 q1  = _mm256_set1_ps(2.0);
					register const __m256 q2  = q1;
					const __m256          qh  = _mm256_div_ps(q,h);
					register __m256 tfi       = _0;
					register __m256 tgi       = _0;
					register __m256 w1        = _0;
					register __m256 w2        = _0;
					register __m256 x2        = _0;
					register __m256 k1        = _0;
					register __m256 k2        = _0;
					register __m256 t0        = _0;
					register __m256 t1        = _0;
					register __m256 t2        = _0;
					register __m256 tt        = _0;
					register __m256 step      = _0;
				        
					tfi  = fi(t,x);
					t0   = _mm256_sqrt_ps(_mm256_mul_ps(q1,qh));
					tgi  = gi(t,x);
					w1   = _mm256_mul_ps(vran1,t0);
					t1   = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w1));
					k1   = _mm256_fmadd_ps(h,tfi,t1);
					x2   = _mm256_fmadd_ps(a21,k1,x);
					tt   = _mm256_fmadd_ps(a21,h,t1);
					tfi  = fi(tt,x2);
					t0   = _mm256_sqrt_ps(_mm256_mul_ps(q2,qh));
					w2   = _mm256_mul_ps(vran2,t0);
					tgi  = gi(tt,x2);
					t2   = _mm256_mul_ps(h,_mm256_mul_ps(tgi,w2));
					k2   = _mm256_fmadd_ps(h,tfi,t2);
					step = _mm256_fmadd_ps(a32,k2,_mm256_fmadd_ps(a31,k1,x));
					return (step);
				}


	                       
                                    
				    __m256 rk4_tv_step_ymm8r4(const __m256 x,
				                               const __m256 t,
							       const __m256 h,
							       const __m256 q,
							       const __m256 vran1,
							       const __m256 vran2,
							       const __m256 vran3,
							       const __m256 vran4,
							       __m256 (*fv) (const __m256, const __m256),
							       __m256 (*gv) (const __m256, const __m256)){

					    const __m256 _0   = _mm256_setzero_ps();
					    const __m256 a21  = _mm256_set1_ps(0.66667754298442F);
					    const __m256 a31  = _mm256_set1_ps(0.63493935027993F);
					    const __m256 a32  = _mm256_set1_ps(0.00342761715422F);
					    const __m256 a41  = _mm256_set1_ps(-2.32428921184321F);
					    const __m256 a42  = _mm256_set1_ps(2.69723745129487F);
					    const __m256 a43  = _mm256_set1_ps(0.29093673271592F);
					    const __m256 a51  = _mm256_set1_ps(0.25001351164789F);
					    const __m256 a52  = _mm256_set1_ps(0.67428574806272F);
					    const __m256 a53  = _mm256_set1_ps(-0.00831795169360F);
					    const __m256 a54  = _mm256_set1_ps(0.08401868181222F);
					    const __m256 q1   = _mm256_set1_ps(3.99956364361748F);
					    const __m256 q2   = _mm256_set1_ps(1.64524970733585F);
					    const __m256 q3   = _mm256_set1_ps(1.59330355118722F);
					    const __m256 q4   = _mm256_set1_ps(0.26330006501868F);
					    const __m256 qh   = _mm256_div_pd(q,h);
					    register __m256 k1 = _0;
					    register __m256 k2 = _0;
					    register __m256 k3 = _0;
					    register __m256 k4 = _0;
					    register __m256 t2 = _0;
					    register __m256 t3 = _0;
					    register __m256 t4 = _0;
					    register __m256 x2 = _0;
					    register __m256 x3 = _0;
					    register __m256 x4 = _0;
					    register __m256 tt2=_0;
					    register __m256 tt3=_0;
					    register __m256 tt4=_0;
					    __m256          tgv=_0;
					    __m256          tfv=_0;
					    __m256          step=_0;
					    __m256          tmp =_0;
					    tfv = fv(t,x);
					    w1  = _mm256_mul_ps(vran1,_mm256_sqrt_ps(_mm256_mul_ps(q1,qh)));
					    tgv = gv(t,x);
					    tmp = _mm256_mul_ps(h,_mm256_mul_ps(tgv,w1));
					    k1  = _mm256_fmadd_ps(h,tfv,tmp);
					    tt2 = _mm256_fmadd_ps(a21,h,t);
					    x2  = _mm256_fmadd_ps(a21,k1,x);
					    tfv = fv(tt2,x2);
					    w2  = _mm256_mul_ps(vran2,_mm256_sqrt_ps(_mm256_mul_ps(q2,qh)));
					    tgv = gv(tt2,x2);
					    tmp = _mm256_mul_ps(h,_mm256_mul_ps(tgv,w2));
					    k2  = _mm256_fmadd_ps(h,tfv,tmp);
					    tt3 = _mm256_fmadd_ps(a32,h,_mm256_fmadd_ps(a31,h,t));
					    x3  = _mm256_fmadd_ps(a32,k2,_mm256_fmadd_ps(a31,k1,x));
					    tfv = fv(tt3,x3);
					    w3  = _mm256_mul_ps(vran3,_mm256_sqrt_ps(_mm256_mul_ps(q3,qh)));
					    tgv = gv(tt3,x3);
					    tmp = _mm256_mul_ps(h,_mm256_mul_ps(tgv,w3));
					    k3  = _mm256_fmadd_ps(h,tfv,tmp);
					    tt4 = _mm256_fmadd_ps(a43,h,_mm256_fmadd_ps(a42,h,_mm256_fmadd_ps(a41,h,t)));
					    x4  = _mm256_fmadd_ps(a43,k3,_mm256_fmadd_ps(a42,k2,_mm256_fmadd_ps(a41,k1,x)));
					    tgv = gv(tt4,x4);
					    w4  = _mm256_mul_ps(vran4,_mm256_sqrt_ps(_mm256_mul_ps(q4,qh)));
					    tfv = fv(tt4,x4);
					    tmp = _mm256_mul_ps(h,_mm256_mul_ps(tgv,w4));
					    k4  = _mm256_fmadd_ps(h,tfv,tmp);
					    step = _mm256_fmadd_ps(a54,k4,_mm256_fmadd_ps(a53,k3,
					                       _mm256_fmadd_ps(a52,k2,_mm256_fmadd_ps(a51,k1,x))));
					    return (step);
				   }

