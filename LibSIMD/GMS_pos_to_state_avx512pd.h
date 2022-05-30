

#ifndef __GMS_POS_TO_STATE_AVX512PD_H__
#define __GMS_POS_TO_STATE_AVX512PD_H__


/*LICENSE:
*
*The source code is in the public domain and not licensed or under
*copyright. The information and software may be used freely by the public.
*As required by 17 U.S.C. 403, third parties producing copyrighted works
*consisting predominantly of the material produced by U.S. government
*agencies must provide notice with such work(s) identifying the U.S.
*Government material incorporated and stating that such material is not
*subject to copyright protection.
*
*Derived works shall not identify themselves in a manner that implies an
*endorsement by or an affiliation with the Naval Research Laboratory.
*
*RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF THE
*SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY THE NAVAL
*RESEARCH LABORATORY FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE ACTIONS
*OF RECIPIENT IN THE USE OF THE SOFTWARE.
@@Modified by Bernard Gingold, on 22-05-2022 15:52 +00200 (SUN 22 MAY 2022 15:25 GMT+2)
  contact: beniekg@gmail.com
*/



 const unsigned int GMS_POS_TO_STATE_AVX512PD_MAJOR = 1U;
 const unsigned int GMS_POS_TO_STATE_AVX512PD_MINOR = 0U;
 const unsigned int GMS_POS_TO_STATE_AVX512PD_MICRO = 0U;
 const unsigned int GMS_POS_TO_STATE_AVX512PD_FULLVER =
  1000U*GMS_POS_TO_STATE_AVX512PD_MAJOR+100U*GMS_POS_TO_STATE_AVX512PD_MINOR+10U*GMS_POS_TO_STATE_AVX512PD_MICRO;
 const char * const GMS_POS_TO_STATE_AVX512PD_CREATION_DATE = "22-05-2022 15:52 +00200 (SUN 22 MAY 2022 15:52 GMT+2)";
 const char * const GMS_POS_TO_STATE_AVX512PD_BUILD_DATE    = __DATE__ " " __TIME__ ;
 const char * const GMS_POS_TO_STATE_AVX512PD_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
 const char * const GMS_POS_TO_STATE_AVX512PD_SYNOPSIS      = "AVX512 based position [2D] to state convertion functions (vectorized)."


#include <immintrin.h>
#include <stdint.h>

/*
https://github.com/USNavalResearchLaboratory/TrackerComponentLibrary/tree/27317126c57c19f57f6d14eb6aa35700f57b869a/Coordinate_Systems/State_Conversion
 %%CART2DSSTATE2POLARSTATE Transform a 2D Cartesian state into a state
%                         consisting of position, heading and speed as well
%                         as possibly a turn rate and a linear
%                         acceleration, depending on the choice of
%                         systemType.
%
%INPUTS: xCart A Cartesian state vector consisting of position velocity and
%              possibly acceleration into a state where heading and speed
%              have been separated. xCart has the form
%              [x;y;xdot;ydot;xddot;yddot], where the acceleration terms
%              xddot;yddot can be omitted if the system type is 'ConstVel'.
%   systemType A string constant specifying the desired type of output. In
%              all instances, the heading is measured in terms of radians
%              counterclockwise from the x-axis. Possible values are:
%              'ConstVel'     The target state is [position;heading;speed]
%                             and xCart is [position;velocity]
%              'ConstAccel'   The target state is [position;heading;speed;
%                             speed derivative] and xCart is
%                             [position;velocity;acceleration]
%              'ConstTurn'    The target state is [position;heading;speed;
%                             turn rate] and xCart is
%                             [position;velocity;acceleration]
%              'TurnAndAccel' The target state is [position;heading;speed;
%                             turnrate; speed derivative] and xCart is
%                             [position;velocity;acceleration]
%
%OUTPUTS: xPol The state converted from 2D Cartesian coordinates into the
%              selected 2D coordinate system.
%
%When the system type is 'ConstVel' or 'TurnAndAccel', only a single
%solution is mathematically observable. When the system type is
%'ConstAccel' or 'ConstTurn', the system is overdetermined, but only a
%simple solution is used, not a least squares solution.
%
%The use of 2D states where the heading and speed have been separated is
%discussed in [1] and [2].
%
%The opposite of this function is polar2DState2CartState.
%
%REFERENCES:
%[1] M. Busch and S. Blackman, "Evaluation of IMM filtering for an air
%    defense system application," in Proceedings of SPIE: Signal and Data
%    Processing of Small Targets, vol. 2561, 9 Jul. 1995, pp. 435-447.
%[1] J. L. Gertz, "Multisensor surveillance for improved aircraft
%    tracking," The Lincoln Laboratory Journal, vol. 2, no. 3, pp. 381-396,
%    1989.
%
%July 2014 David F. Crouse, Naval Research Laboratory, Washington D.C.
**@@Modified Bernard Gingold May 2022 ,beniekg@gmail.com
%%(UNCLASSIFIED) DISTRIBUTION STATEMENT A. Approved for public release.
*/

void
const_velocity_zmm8r8(const __m512d,
                      const __m512d,
		      __m512d * __restrict,
		      __m512d * __restrict)    __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
const_velocity_zmm8r8_a(const __m512d,
                        const __m512d,
		        double * __restrict,
		        double * __restrict)   __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
const_velocity_zmm8r8_u(const __m512d,
                        const __m512d,
		        double * __restrict,
		        double * __restrict)   __attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
const_acceleration_zmm8r8(const __m512d,
                          const __m512d,
			  const __m512d,
			  const __m512d,
			  __m512d * __restrict,
			  __m512d * __restrict,
			  __m512d * __restrict)__attribute__((noinline))
			                       __attribute__((hot))
					       __attribute__((regcall))
					       __attribute__((aligned(32)));


void
const_acceleration_zmm8r8_a(const __m512d,
                            const __m512d,
			    const __m512d,
			    const __m512d,
			    double * __restrict,
			    double * __restrict,
			    double * __restrict)__attribute__((noinline))
			                        __attribute__((hot))
					        __attribute__((regcall))
					        __attribute__((aligned(32)));

void
const_acceleration_zmm8r8_u(const __m512d,
                            const __m512d,
			    const __m512d,
			    const __m512d,
			    double * __restrict,
			    double * __restrict,
			    double * __restrict)__attribute__((noinline))
			                        __attribute__((hot))
					        __attribute__((regcall))
					        __attribute__((aligned(32)));


void
const_turn_zmm8r8(const __m512d,
                  const __m512d,
		  const __m512d,
		  const __m512d,
		  __m512d * __restrict,
		  __m512d * __restrict,
		  __m512d * __restrict)__attribute__((noinline))
			               __attribute__((hot))
				       __attribute__((regcall))
				       __attribute__((aligned(32)));


void
const_turn_zmm8r8_a(const __m512d,
                    const __m512d,
		    const __m512d,
		    const __m512d,
		    double * __restrict,
		    double * __restrict,
		    double * __restrict)__attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));

void
const_turn_zmm8r8_u(const __m512d,
                    const __m512d,
		    const __m512d,
		    const __m512d,
		    double * __restrict,
		    double * __restrict,
		    double * __restrict)__attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));
					
						
void
turn_accelerate_zmm8r8(const __m512d,
                       const __m512d,
		       const __m512d,
		       const __m512d,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict)__attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));


void
turn_accelerate_zmm8r8_a(const __m512d,
                         const __m512d,
		         const __m512d,
		         const __m512d,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict)__attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((regcall))
				             __attribute__((aligned(32)));


void
turn_accelerate_zmm8r8_u(const __m512d,
                         const __m512d,
		         const __m512d,
		         const __m512d,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict)__attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((regcall))
				             __attribute__((aligned(32)));


/*
  %%POLAR2DSTATE2CARTSTATE Convert a 2D target state where the velocity had
%               been decomposed into a direction angle (heading) and speed
%               components into Cartesian components. Depending on the
%               system type chosen, the state can have components for a
%               linear acceleration and/ or a turn rate.
%
%INPUTS: xPol  A 4X1, 5X1 or 6X1 target state where the first four
%              components are [position;heading;speed] in 2D. The other
%              components depend on the value of systemType.
%   systemType A string constant specifying the desired type of input and
%              output. In all instances, the heading is measured in terms
%              of radians counterclockwise from the x-axis. Possible values
%              are:
%              'ConstVel'     The target state is [position;heading;speed]
%                             and xCart is [position;velocity]
%              'ConstAccel'   The target state is [position;heading;speed;
%                             speed derivative] and xCart is
%                             [position;velocity;acceleration]
%              'ConstTurn'    The target state is [position;heading;speed;
%                             turn rate] and xCart is
%                             [position;velocity;acceleration]
%              'TurnAndAccel' The target state is [position;heading;speed;
%                             turnrate; speed derivative] and xCart is
%                             [position;velocity;acceleration]
%
%%OUTPUTS: xCart The state converted into 2D Cartesian coordinates
%                consisting of position and velocity and, depending on
%                systemType, possibly acceleration components.
%
%The use of 2D states where the heading and speed have been separated is
%discussed in [1] and [2].
%
%The opposite of this function is Cart2DState2PolarState.
%
%REFERENCES:
%[1] M. Busch and S. Blackman, "Evaluation of IMM filtering for an air
%    defense system application," in Proceedings of SPIE: Signal and Data
%    Processing of Small Targets, vol. 2561, 9 Jul. 1995, pp. 435-447.
%[2] J. L. Gertz, "Multisensor surveillance for improved aircraft
%    tracking," The Lincoln Laboratory Journal, vol. 2, no. 3, pp. 381-396,
%    1989.
%
%July 2014 David F. Crouse, Naval Research Laboratory, Washington D.C.
@@Modified by Bernard Gingold, on 29-05-2022 09:12 +00200 (SUN 29 MAY 2022 09:12 GMT+2)
%(UNCLASSIFIED) DISTRIBUTION STATEMENT A. Approved for public release.
*/	           


void
const_pol_vel_zmm8r8(const __m512d,
                     const __m512d,
		     __m512d * __restrict,
		     __m512d * __restrict) __attribute__((noinline))
			                   __attribute__((hot))
				           __attribute__((regcall))
				           __attribute__((aligned(32)));


void
const_pol_vel_zmm8r8_a(const __m512d,
                       const __m512d,
		       double * __restrict,
		       double * __restrict) __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));


void
const_pol_vel_zmm8r8_u(const __m512d,
                       const __m512d,
		       double * __restrict,
		       double * __restrict) __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));


void
const_pol_accel_zmm8r8(const __m512d,
                       const __m512d,
		       const __m512d,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict) __attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((regcall))
				             __attribute__((aligned(32)));


void
const_pol_accel_zmm8r8_a(const __m512d,
                         const __m512d,
		         const __m512d,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict) __attribute__((noinline))
			                      __attribute__((hot))
				              __attribute__((regcall))
				              __attribute__((aligned(32)));


void
const_pol_accel_zmm8r8_u(const __m512d,
                         const __m512d,
		         const __m512d,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict) __attribute__((noinline))
			                      __attribute__((hot))
				              __attribute__((regcall))
				              __attribute__((aligned(32)));


void
const_pol_turn_zmm8r8( const __m512d,
                       const __m512d,
		       const __m512d,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict,
		       __m512d * __restrict) __attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((regcall))
				             __attribute__((aligned(32)));


void
const_pol_turn_zmm8r8_a( const __m512d,
                       const __m512d,
		       const __m512d,
		       double * __restrict,
		       double * __restrict,
		       double * __restrict,
		       double * __restrict) __attribute__((noinline))
			                     __attribute__((hot))
				             __attribute__((regcall))
				             __attribute__((aligned(32)));


void
const_pol_turn_zmm8r8_u( const __m512d,
                         const __m512d,
		         const __m512d,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict,
		         double * __restrict) __attribute__((noinline))
			                      __attribute__((hot))
				              __attribute__((regcall))
				              __attribute__((aligned(32)));











#endif /*__GMS_POS_TO_STATE_AVX512PD_H__*/
