
#ifndef __GMS_RANGE_RATE_AVX512PD_H__
#define __GMS_RANGE_RATE_AVX512PD_H__ 140520221111





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
@@Modified by Bernard Gingold, on 14-05-2022 11:11 +00200 (SAT 14 MAY 2022 11:11 GMT+2)
  contact: beniekg@gmail.com
*/



 const unsigned int gGMS_RANGE_RATE_AVX512_MAJOR = 1U;
 const unsigned int gGMS_RANGE_RATE_AVX512_MINOR = 0U;
 const unsigned int gGMS_RANGE_RATE_AVX512_MICRO = 0U;
 const unsigned int gGMS_RANGE_RATE_AVX512_FULLVER =
  1000U*gGMS_RANGE_RATE_AVX512_MAJOR+100U*gGMS_RANGE_RATE_AVX512_MINOR+10U*gGMS_RANGE_RATE_AVX512_MICRO;
 const char * const pgGMS_RANGE_RATE_AVX512_CREATION_DATE = "14-05-2022 11:11 +00200 (SAT 14 MAY 2022 11:11 GMT+2)";
 const char * const pgGMS_RANGE_RATE_AVX512_BUILD_DATE    = __DATE__ " " __TIME__ ;
 const char * const pgGMS_RANGE_RATE_AVX512_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
 const char * const pgGMS_RANGE_RATE_AVX512_SYNOPSIS      = "AVX512 based range-rate functions (vectorized)."





#include <immintrin.h>
#include <stdbool.h>


/*GETRANGERATE2DGENCPP A C++ function to convert a Cartesian state in 2D
 *        into a non-relativistic range rate, ignoring atmospheric effects.
 *
 *INPUTS: xTar The 4X1 Cartesian position and velocity vectors
 *             [x;y;xDot;yDot].
 *  useHalfRange A boolean value specifying whether the bistatic (round-
 *             trip) range value (and hence the range rate) has been
 *             divided by two. 
 *         xTx The 4X1 [x;y;xDot;yDot] position and velocity vector of
 *             the transmitter in global Cartesian coordinates.
 *         xRx The 4X1 [x;y;xDot;yDot] position and velocity vector of
 *             the receiver in global Cartesian coordinates.
 *
 *OUTPUTS: rr The range rate as a double.
 *
 *See the comments to the Matlab function getRangeRate for more information
 *on how this function works.
 *
 *April 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
 *@@Modified by Bernard Gingold, on May 2022
 **/

  __m512d
  range_rate_2d_zmm8r8(const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const __m512d,
		       const bool) __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


/*GETRANGERATE3DGENCPP A C++ function to convert a Cartesian state in 3D
 *        into a non-relativistic range rate, ignoring atmospheric effects.
 *
 *INPUTS: xTar The 6X1 Cartesian position and velocity vectors
 *             [x;y;z;xDot;yDot;zDot].
 * useHalfRange A boolean value specifying whether the bistatic (round-
 *             trip) range value (and hence the range rate) has been
 *             divided by two. 
 *         xTx The 6X1 [x;y;z;xDot;yDot;zDot] position and velocity
 *             vector of the transmitter in global Cartesian coordinates.
 *         xTx The 6X1 [x;y;z;xDot;yDot;zDot] position and velocity
 *             vector of the receiver in global Cartesian coordinates.
 *
 *OUTPUTS: rr The range rate as a double.
 *
 *See the comments to the Matlab function getRangeRate for more information
 *on how this function works.
 *
 *April 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
 *@@Modified by Bernard Gingold, on May 2022
 **/

 __m512d
 range_rate_3d_zmm8r8(const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const bool)  __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


/*A C++-only implementations of a functions for computing the gradient of
 *bistatic range.  See the Matlab equivalents for more comments.
 *
*February 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
**@@Modified by Bernard Gingold, on May 2022
**/
/*(UNCLASSIFIED) DISTRIBUTION STATEMENT A. Approved for public release.*/

 __m512d
 range_grad_zmm8r8(const __m512d,
		   const __m512d,
		   const __m512d,
		   const bool)   __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


/**RANGEHESSIANCPP A C++-only implementations of a function for computing
 *the Hessian of bistatic range.  See the Matlab equivalent for more
 *comments.
 *
 *June 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
 *@@Modified by Bernard Gingold, on May 2022
 */
/*(UNCLASSIFIED) DISTRIBUTION STATEMENT A. Approved for public release.*/

 __m512d
 range_hessian_1d_zmm8r8()         __attribute__((noinline))
			           __attribute__((hot))
				   __attribute__((regcall))
				   __attribute__((aligned(32)));


 void
 range_hessian_2d_zmm8r8_a(double * __restrict ,
		           double * __restrict ,
			   double * __restrict ,
			   double * __restrict ,
			   const __m512d,
			   const __m512d,
			   const bool)  __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


 void
 range_hessian_2d_zmm8r8_u(double * __restrict ,
		           double * __restrict ,
			   double * __restrict ,
			   double * __restrict ,
			   const __m512d,
			   const __m512d,
			   const bool)  __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


 void
 range_hessian_3d_zmm8r8_a(double * __restrict,
		           double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const bool)  __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


 void
 range_hessian_3d_zmm8r8_u(double * __restrict,
		           double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const bool)  __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


 void
 range_hessian_3d_zmm8r8(__m512d * __restrict,
		         const __m512d,
			 const __m512d,
			 const __m512d,
			 const bool ) __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


 __m512d
 range_hess_gen_1d_zmm8r8()             __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


void
range_hess_gen_2d_zmm8r8(__m512d * __restrict,
		         __m512d * __restrict,
			 __m512d * __restrict,
			 __m512d * __restrict,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 const __m512d,
			 const bool )  __attribute__((noinline))
			                __attribute__((hot))
				        __attribute__((regcall))
				        __attribute__((aligned(32)));


void
range_hess_gen_2d_zmm8r8_a(double * __restrict,
		           double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const bool )  __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


void
range_hess_gen_2d_zmm8r8_u(double * __restrict,
		           double * __restrict,
			   double * __restrict,
			   double * __restrict,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const __m512d,
			   const bool )  __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


void
range_hess_3d_zmm8r8(__m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     __m512d * __restrict,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const bool )        __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


void
range_hess_3d_zmm8r8_a( double * __restrict,
		        double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const bool )     __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


void
range_hess_3d_zmm8r8_u( double * __restrict,
		        double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			double * __restrict,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const bool )     __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


/*CART2RUVGENCPP A C++ function to convert a Cartesian point into range,
 *           and direction cosines, possibly including the w component.
 *
 *INPUTS: retData A pointer to an array of doubles with 3 elements to
 *                hold the result in [r;u;v] order or with 4 elements if
 *                includeW is true to hold [r;u;v;w].
 *             zC The 3X1 Cartesian points [x;y;z] to be converted.
 *   useHalfRange A boolean value specifying whether the bistatic (round-
 *                trip) range value has been divided by two. 
 *            zTx The 3X1 [x;y;z] location vector of the transmitter in
 *                global Cartesian coordinates.
 *            zRx The 3X1 [x;y;z] location vector of the receiver in global
 *                Cartesian coordinates.
 *             M  A 3X3 rotation matrices to go from the alignment of the
 *                global coordinate system to that at the receiver. It is
 *                stored one columns after the other, consistent with how
 *                Matlab indexes matrices.
 *       includeW A boolean value indicating whether retData has space for
 *                a fourth component and the fourth component should be
 *                included.
 *
 *OUTPUTS: None. The results are placed in retData.
 *
 *See the comments to the Matlab function Cart2ruv for more information
 *on how this function works.
 *
 *April 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
 *@@Modified by Bernard Gingold, on May 2022
 **/


void
cart_to_ruv_zmm8r8(__m512d * __restrict,
		   __m512d * __restrict,
		   __m512d * __restrict,
		   __m512d * __restrict,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d,
		   const __m512d * __restrict , //flattened 3x3 matrix
		   const bool )          __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));

void
cart_to_ruv_zmm8r8_a(double * __restrict ,
		     double * __restrict ,
		     double * __restrict ,
		     double * __restrict ,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d * __restrict , //flattened 3x3 matrix
		     const bool )        __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));




void
cart_to_ruv_zmm8r8_u(double * __restrict ,
		     double * __restrict ,
		     double * __restrict ,
		     double * __restrict ,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d,
		     const __m512d * __restrict , //flattened 3x3 matrix
		     const bool )        __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


/*CART2SPHEREGENCPP A C++ function to convert Cartesian points to bistatic
 *            range, azimuth and elevation.
 *
 *INPUTS: retData A pointer to an array of doubles with 3 elements to
 *                hold the result in [range;azimuth;elevation]. order.
 *     cartPoints A pointer to the 3X1 Cartesian points [x;y;z] to be
 *                converted.
 *     systemType An integer specifying the axis from which the angles are
 *                measured. Possible values are
 *                0 Azimuth is measured counterclockwise from the x-axis in
 *                  the x-y plane. Elevation is measured up from the x-y
 *                  plane (towards the z-axis). This is consistent with
 *                  common spherical coordinate systems for specifying
 *                  longitude (azimuth) and geocentric latitude
 *                  (elevation).
 *                1 Azimuth is measured counterclockwise from the z-axis in
 *                  the z-x plane. Elevation is measured up from the z-x
 *                  plane (towards the y-axis). This is consistent with
 *                  some spherical coordinate systems that use the z-axis
 *                  as the boresight direction of the radar.
 *                2 This is the same as 0 except instead of being given
 *                  elevation, one desires the angle away from the z-axis,
 *                  which is (pi/2-elevation).
 *                3 This is the same as 0 except azimuth is measured
 *                  clockwise from the y-axis in the x-y plane instead of
 *                  counterclockwise from the x-axis. This coordinate
 *                  system often arises when given "bearings" in a local
 *                  East-North-Up coordinate system, where the bearing
 *                  directions are measured East of North.
 *   useHalfRange A boolean value specifying whether the bistatic (round-
 *                trip) range value has been divided by two. 
 *            zTx The 3X1 [x;y;z] location vector of the transmitter in
 *                global Cartesian coordinates.
 *            zRx The 3X1 [x;y;z] location vector of the receiver in global
 *                Cartesian coordinates.
 *             M  A 3X3  rotation matrices to go from the alignment of the
 *                global coordinate system to that at the receiver. It is
 *                stored one columns after the other, consistent with how
 *                Matlab indexes matrices.
 *
 *OUTPUTS: None. The results are placed in retData.
 *
 *See the comments to the Matlab function Cart2Sphere for more information
 *on how this function works.
 *
 *April 2017 David F. Crouse, Naval Research Laboratory, Washington D.C.
 *@@Modified by Bernard Gingold, on May 2022
 **/


void
cart_to_sphere_zmm8r8(__m512d * __restrict,
		      __m512d * __restrict,
		      __m512d * __restrict,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d,
		      const __m512d * __restrict,
		      const int,
		      const bool)        __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));


void
cart_to_sphere_zmm8r8_a(double * __restrict,
		        double * __restrict,
			double * __restrict,
			const __m512d,
			const __m512d,
			const __m512d,
		        const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d * __restrict,
			const int,
			const bool )     __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));



void
cart_to_sphere_zmm8r8_u(double * __restrict,
		        double * __restrict,
			double * __restrict,
			const __m512d,
			const __m512d,
			const __m512d,
		        const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d,
			const __m512d * __restrict,
			const int,
			const bool )     __attribute__((noinline))
			                 __attribute__((hot))
				         __attribute__((regcall))
				         __attribute__((aligned(32)));







#endif /*__GMS_RANGE_RATE_AVX512PD_H__*/
