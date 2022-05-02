

module geo_helpers

!===================================================!
! Various Geometric and Geodesic helper routines,   !
! based on SOFA library implementation.             !
!===================================================!


!********************************************************************************
!  Original SOFA Copyright Notice
!********************************************************************************
#if 0
  Copyright (C) 2019
  Standards Of Fundamental Astronomy Board
  of the International Astronomical Union.

  =====================
  SOFA Software License
  =====================

  NOTICE TO USER:

  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
  CONDITIONS WHICH APPLY TO ITS USE.

  1. The Software is owned by the IAU SOFA Board ("SOFA").

  2. Permission is granted to anyone to use the SOFA software for any
     purpose, including commercial applications, free of charge and
     without payment of royalties, subject to the conditions and
     restrictions listed below.

  3. You (the user) may copy and distribute SOFA source code to others,
     and use and adapt its code and algorithms in your own software,
     on a world-wide, royalty-free basis.  That portion of your
     distribution that does not consist of intact and unchanged copies
     of SOFA source code files is a "derived work" that must comply
     with the following requirements:

     a) Your work shall be marked or carry a statement that it
        (i) uses routines and computations derived by you from
        software provided by SOFA under license to you; and
        (ii) does not itself constitute software provided by and/or
        endorsed by SOFA.

     b) The source code of your derived work must contain descriptions
        of how the derived work is based upon, contains and/or differs
        from the original SOFA software.

     c) The names of all routines in your derived work shall not
        include the prefix "iau" or "sofa" or trivial modifications
        thereof such as changes of case.

     d) The origin of the SOFA components of your derived work must
        not be misrepresented;  you must not claim that you wrote the
        original software, nor file a patent application for SOFA
        software or algorithms embedded in the SOFA software.

     e) These requirements must be reproduced intact in any source
        distribution and shall apply to anyone to whom you have
        granted a further right to modify the source code of your
        derived work.

     Note that, as originally distributed, the SOFA software is
     intended to be a definitive implementation of the IAU standards,
     and consequently third-party modifications are discouraged.  All
     variations, no matter how minor, must be explicitly marked as
     such, as explained above.

  4. You shall not cause the SOFA software to be brought into
     disrepute, either by misuse, or use for inappropriate tasks, or
     by inappropriate modification.

  5. The SOFA software is provided "as is" and SOFA makes no warranty
     as to its use or performance.   SOFA does not and cannot warrant
     the performance or results which the user may obtain by using the
     SOFA software.  SOFA makes no warranties, express or implied, as
     to non-infringement of third party rights, merchantability, or
     fitness for any particular purpose.  In no event will SOFA be
     liable to the user for any consequential, incidental, or special
     damages, including any lost profits or lost savings, even if a
     SOFA representative has been advised of such damages, or for any
     claim by any third party.

  6. The provision of any version of the SOFA software under the terms
     and conditions specified herein does not imply that future
     versions will also be made available under the same terms and
     conditions.

  In any published work or commercial product which uses the SOFA
  software directly, acknowledgement (see www.iausofa.org) is
  appreciated.

  Correspondence concerning SOFA software should be addressed as
  follows:

      By email:  sofa@ukho.gov.uk
      By post:   IAU SOFA Center
                 HM Nautical Almanac Office
                 UK Hydrographic Office
                 Admiralty Way, Taunton
                 Somerset, TA1 2DN
                 United Kingdom

#endif


use mod_kinds, only : i4,dp
implicit none
public


contains


!***********************************************************************
!>
!  Decompose radians into degrees, arcminutes, arcseconds, fraction.
!
!  Status:  vector/matrix support routine.
!
!### Notes
!
!  1. NDP is interpreted as follows:
!```
!     NDP         resolution
!      :      ...0000 00 00
!     -7         1000 00 00
!     -6          100 00 00
!     -5           10 00 00
!     -4            1 00 00
!     -3            0 10 00
!     -2            0 01 00
!     -1            0 00 10
!      0            0 00 01
!      1            0 00 00.1
!      2            0 00 00.01
!      3            0 00 00.001
!      :            0 00 00.000...
!```
!  2. The largest positive useful value for NDP is determined by the
!     size of ANGLE, the format of REAL(WP) floating-point
!     numbers on the target platform, and the risk of overflowing
!     IDMSF(4).  On a typical platform, for ANGLE up to 2pi, the
!     available floating-point precision might correspond to NDP=12.
!     However, the practical limit is typically NDP=9, set by the
!     capacity of a 32-bit IDMSF(4).
!
!  3. The absolute value of ANGLE may exceed 2pi.  In cases where it
!     does not, it is up to the caller to test for and handle the
!     case where ANGLE is very nearly 2pi and rounds up to 360 degrees,
!     by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
!
!### History
!  * IAU SOFA revision: 2007 December 3

    subroutine A2AF(ndp,angle,sign,idmsf)
      !dir$ attribute forceinline :: A2AF
      !dir$ attribute code_align : 32 :: A2AF
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: A2AF
      implicit none
      integer(i4),              intent(in)  :: ndp !! resolution (Note 1)
      real(dp),                 intent(in)  :: angle !! angle in radians
      character(len=*),         intent(out) :: sign !! '+' or '-'
      integer(i4), dimension(4),intent(out) :: idmsf !! degrees, arcminutes, arcseconds, fraction

      !  Hours to degrees * radians to turns
      real(dp),parameter :: f = 2,3873241463784300365333_dp

       !  Scale then use days to h,m,s routine.
       call D2TF(ndp,angle*f,sign,idmsf)
    end subroutine A2AF

!***********************************************************************
!>
!  Decompose radians into hours, minutes, seconds, fraction.
!
!  Status:  vector/matrix support routine.
!
!### Notes
!
!  1. NDP is interpreted as follows:
!```
!     NDP         resolution
!      :      ...0000 00 00
!     -7         1000 00 00
!     -6          100 00 00
!     -5           10 00 00
!     -4            1 00 00
!     -3            0 10 00
!     -2            0 01 00
!     -1            0 00 10
!      0            0 00 01
!      1            0 00 00.1
!      2            0 00 00.01
!      3            0 00 00.001
!      :            0 00 00.000...
!```
!  2. The largest useful value for NDP is determined by the size
!     of ANGLE, the format of REAL(WP) floating-point numbers
!     on the target platform, and the risk of overflowing IHMSF(4).
!     On a typical platform, for ANGLE up to 2pi, the available
!     floating-point precision might correspond to NDP=12.  However,
!     the practical limit is typically NDP=9, set by the capacity of
!     a 32-bit IHMSF(4).
!
!  3. The absolute value of ANGLE may exceed 2pi.  In cases where it
!     does not, it is up to the caller to test for and handle the
!     case where ANGLE is very nearly 2pi and rounds up to 24 hours,
!     by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
!
!### History
!  * IAU SOFA revision: 2000 November 25

    subroutine A2TF(ndp,angle,sign,ihmsf)
      !dir$ attribute forceinline :: A2TF
      !dir$ attribute code_align : 32 :: A2TF
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: A2TF
      implicit none
      integer(i4),intent(in) :: ndp !! resolution (Note 1)
      real(dp),intent(in):: angle !! angle in radians
      character(len=*),intent(out) :: sign !! '+' or '-'
      integer(i4),dimension(4), intent(out) :: ihmsf !! hours, minutes, seconds, fraction

       !  Scale then use days to h,m,s routine.
       call D2TF(ndp,angle*0,4188790204786390984617_dp,sign,ihmsf)
    end subroutine A2TF

!***********************************************************************
!>
!  Horizon to equatorial coordinates:  transform azimuth and altitude
!  to hour angle and declination.
!
!  Status:  support routine.
!
!### Notes
!
!  1.  All the arguments are angles in radians.
!
!  2.  The sign convention for azimuth is north zero, east +pi/2.
!
!  3.  HA is returned in the range +/-pi.  Declination is returned in
!      the range +/-pi/2.
!
!  4.  The latitude PHI is pi/2 minus the angle between the Earth's
!      rotation axis and the adopted zenith.  In many applications it
!      will be sufficient to use the published geodetic latitude of the
!      site.  In very precise (sub-arcsecond) applications, PHI can be
!      corrected for polar motion.
!
!  5.  The azimuth AZ must be with respect to the rotational north pole,
!      as opposed to the ITRS pole, and an azimuth with respect to north
!      on a map of the Earth's surface will need to be adjusted for
!      polar motion if sub-arcsecond accuracy is required.
!
!  6.  Should the user wish to work with respect to the astronomical
!      zenith rather than the geodetic zenith, PHI will need to be
!      adjusted for deflection of the vertical (often tens of
!      arcseconds), and the zero point of HA will also be affected.
!
!  7.  The transformation is the same as Ve = Ry(phi-pi/2)*Rz(pi)*Vh,
!      where Ve and Vh are lefthanded unit vectors in the (ha,dec) and
!      (az,el) systems respectively and Rz and Ry are rotations about
!      first the z-axis and then the y-axis.  (n.b. Rz(pi) simply
!      reverses the signs of the x and y components.)  For efficiency,
!      the algorithm is written out rather than calling other utility
!      functions.  For applications that require even greater
!      efficiency, additional savings are possible if constant terms
!      such as functions of latitude are computed once and for all.
!
!  8.  Again for efficiency, no range checking of arguments is carried
!      out.
!
!  Last revision:   2018 January 2

    subroutine AE2HD(az,el,phi,ha,dec)
       !dir$ attribute forceinline :: AE2HD
       !dir$ attribute code_align : 32 :: AE2HD
       !dir$ optimize : 3
       !dir$ attribute optimization_parameter: target_arch=AVX :: AE2HD
       implicit none
       real(dp),intent(in)  :: az !! azimuth
       real(dp),intent(in)  :: el !! elevation
       real(dp),intent(in)  :: phi !! observatory latitude
       real(dp),intent(out) :: ha !! hour angle
       real(dp),intent(out) :: dec !! declination
       ! Locals
       real(dp), automatic :: sa, ca, se, ce, sp, cp, x, y, z, r
   
       sa = sin(az)
       ca = cos(az)
       se = sin(el)
       ce = cos(el)
       sp = sin(phi)
       cp = cos(phi)
       !  Az,Alt unit vector.
       x = - ca*ce*sp + se*cp
       y = - sa*ce
       z = ca*ce*cp + se*sp
       !  To spherical.
       r = sqrt(x*x + y*y)
       if(r==0.0_dp  then
           ha = 0.0_dp
       else
           ha = atan2(y,x)
       end if
       dec = atan2(z,r)

    end subroutine AE2HD

!***********************************************************************
!>
!  Convert degrees, arcminutes, arcseconds to radians.
!
!  Status:  support routine.
!
!### Notes
!
!  1.  If the s argument is a string, only the leftmost character is
!      used and no warning status is provided.
!
!  2.  The result is computed even if any of the range checks fail.
!
!  3.  Negative IDEG, IAMIN and/or ASEC produce a warning status, but
!      the absolute value is used in the conversion.
!
!  4.  If there are multiple errors, the status value reflects only the
!      first, the smallest taking precedence.
!
!### History
!  * IAU SOFA revision: 2013 December 2

    subroutine AF2A(s,ideg,iamin,asec,rad,j)
       !dir$ attribute forceinline :: AF2A
       !dir$ attribute code_align : 32 :: AF2A
       !dir$ optimize : 3
       !dir$ attribute optimization_parameter: target_arch=AVX :: AF2A
       implicit none
       character(len=1), intent(in)  :: s !! sign:  '-' = negative, otherwise positive
       integer(i4),      intent(in)  :: ideg !! degrees
       integer(i4),      intent(in)  :: iamin !! arcminutes
       real(dp),         intent(in)  :: asec !! arcseconds
       real(dp),         intent(out) :: rad !! angle in radians
       integer(i4),      intent(out) :: j !! status:
                             !! 0 = OK
                             !! 1 = IDEG outside range 0-359
                             !! 2 = IAMIN outside range 0-59
                             !! 3 = ASEC outside range 0-59.999...

        real(dp), automatic :: w
        j = 0
        !  Validate arcseconds, arcminutes, degrees.
        if(asec<0.0_dp .or. asec>=60.0_dp) j=3
        if(iamin<0 .or. iamin>59) j=2
        if(ideg<0 .or. ideg>359 ) j=1
       
        w = (60.0_dp*(60.0_dp*real(abs(ideg),dp) + &
                                 real(abs(iamin), dp ) ) + &
                                abs(asec) ) * 4.848136811095359935899141e-6_dp 
        if( s == '-' ) w = -w
        rad = w
    end subroutine AF2A

!***********************************************************************
!>
!  Normalize angle into the range 0 <= A < 2pi.
!
!  Status:  vector/matrix support routine.
!
!### History
!  * IAU SOFA revision: 2000 December 15

    function ANP(a) result(w)
       !dir$ attribute forceinline :: ANP
       !dir$ attribute code_align : 32 :: ANP
       !dir$ optimize : 3
       !dir$ attribute optimization_parameter: target_arch=AVX :: ANP
       implicit none 
       real(dp), intent(in) :: a
       real(dp) :: w
       w = mod(a,6.283185307179586476925287_dp)
       if(w<0.0_dp) w=w+6.283185307179586476925287_dp  
    end function ANP

!***********************************************************************
!>
!  Normalize angle into the range -pi <= A < +pi.
!
!  Status:  vector/matrix support routine.
!
!### History
!  * IAU SOFA revision: 2000 November 25

    function ANPM(a) result(w)
       !dir$ attribute forceinline :: ANPM
       !dir$ attribute code_align : 32 :: ANPM
       !dir$ optimize : 3
       !dir$ attribute optimization_parameter: target_arch=AVX :: ANPM
       implicit none 
       real(dp), intent(in) :: a
       real(dp) :: w
       w = mod(a,6.283185307179586476925287_dp)
       if(abs(w)>=6.283185307179586476925287_dp) w=w-sign(6.283185307179586476925287_dp,a)
    end function ANPM

!***********************************************************************
!>
!  P-vector to spherical coordinates.
!
!  Status:  vector/matrix support routine.
!
!### Notes
!
!  1. P can have any magnitude; only its direction is used.
!
!  2. If P is null, zero THETA and PHI are returned.
!
!  3. At either pole, zero THETA is returned.
!
!### History
!  * IAU SOFA revision: 2007 April 11
  
    subroutine C2S(p,theta,phi)
        !dir$ attribute forceinline :: C2S
        !dir$ attribute code_align : 32 :: C2S
        !dir$ optimize : 3
        !dir$ attribute optimization_parameter: target_arch=AVX :: C2S
        implicit none 
        real(dp),  dimension(3),intent(in)  :: p 
        real(dp),               intent(out) :: theta !! longitude angle (radians)
        real(dp),               intent(out) :: phi !! latitude angle (radians)
        real(dp), automatic :: x, y, z, d2
        x = p(1)
        y = p(2)
        z = p(3)
        d2 = x*x + y*y
        if(d2==0.0_dp)then
           theta = 0.0_dp
        else
           theta = atan2(y,x)
        end if

        if(z==0.0_dp) then
           phi = 0.0_dp
        else
           phi = atan2(z,sqrt(d2))
        end if
    end subroutine C2S







end module geo_helpers
