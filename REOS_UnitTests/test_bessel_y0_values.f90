
!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module mod_test_bessel_y0_values


       use mod_kinds,     only : i4, dp 
       use mod_vectypes,  only : XMM2r8_t, Mask2_t
       use iso_c_binding, only : c_int, c_long_long 
     

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_bessel_y0_values -fp-model fast=2 -ftz -O3 -ggdb  -xHost \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_config.fpp GMS_kinds.f90 GMS_vectypes.f90 GMS_spec_func_xmm2r8.f90 GMS_mod_fpcompare.f90 GMS_intrinsics_wrappers.o test_bessel_y0_values.f90
    
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S -fp-model fast=2 -ftz -O3 -ggdb  -xHost \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_config.fpp GMS_kinds.f90 GMS_vectypes.f90 GMS_spec_funcs_xmm2r8.f90 GMS_mod_fpcompare.f90 GMS_intrinsics_wrappers.o test_bessel_y0_values.f90
      
#endif

      contains 

function besy0 ( x ) result(ret_val)

!*****************************************************************************80
!
!! BESY0 evaluates the Bessel Y0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the second kind of order zero for arguments 0 < X <= XMAX.
!
!    See comments heading CALJY0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESY0, the value of the function.
!
  implicit none

  !real ( kind = 8 ) besy0
  integer ( kind = 4 ) jint
  real ( kind = dp ) result
  real ( kind = dp ) x
  real(kind = dp ) :: ret_val 
  jint = 1
  call caljy0 ( x, result, jint )
  ret_val = result

  return
end function 

subroutine caljy0 ( arg, result, jint )

!*****************************************************************************80
!
!! CALJY0 computes various J0 and Y0 Bessel functions.
!
!  Discussion:
!
!    This routine computes zero-order Bessel functions of the first and
!    second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
!    for Y0, and |X| <= XMAX for J0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 0, ARG
!    must satisfy
!     -XMAX < ARG < XMAX;
!    If JINT = 1, then ARG must satisfy
!      0 < ARG < XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    0, RESULT = J0(x);
!    1, RESULT = Y0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    0, J0(x);
!    1, Y0(x);
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) jint
  real ( kind = 8 ) arg
  real ( kind = 8 ) ax
  real ( kind = 8 ) cons
  real ( kind = 8 ) down
  real ( kind = 8 ) eight
  real ( kind = 8 ) five5
  real ( kind = 8 ) four
  real ( kind = 8 ) one
  real ( kind = 8 ) oneov8
  real ( kind = 8 ) pi2
  real ( kind = 8 ) pj0(7)
  real ( kind = 8 ) pj1(8)
  real ( kind = 8 ) plg(4)
  real ( kind = 8 ) prod
  real ( kind = 8 ) py0(6)
  real ( kind = 8 ) py1(7)
  real ( kind = 8 ) py2(8)
  real ( kind = 8 ) p0(6)
  real ( kind = 8 ) p1(6)
  real ( kind = 8 ) p17
  real ( kind = 8 ) qj0(5)
  real ( kind = 8 ) qj1(7)
  real ( kind = 8 ) qlg(4)
  real ( kind = 8 ) qy0(5)
  real ( kind = 8 ) qy1(6)
  real ( kind = 8 ) qy2(7)
  real ( kind = 8 ) q0(5)
  real ( kind = 8 ) q1(5)
  real ( kind = 8 ) resj
  real ( kind = 8 ) result
  real ( kind = 8 ) r0
  real ( kind = 8 ) r1
  real ( kind = 8 ) sixty4
  real ( kind = 8 ) three
  real ( kind = 8 ) twopi
  real ( kind = 8 ) twopi1
  real ( kind = 8 ) twopi2
  real ( kind = 8 ) two56
  real ( kind = 8 ) up
  real ( kind = 8 ) w
  real ( kind = 8 ) wsq
  real ( kind = 8 ) xden
  real ( kind = 8 ) xinf
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xnum
  real ( kind = 8 ) xsmall
  real ( kind = 8 ) xj0
  real ( kind = 8 ) xj1
  real ( kind = 8 ) xj01
  real ( kind = 8 ) xj02
  real ( kind = 8 ) xj11
  real ( kind = 8 ) xj12
  real ( kind = 8 ) xy
  real ( kind = 8 ) xy0
  real ( kind = 8 ) xy01
  real ( kind = 8 ) xy02
  real ( kind = 8 ) xy1
  real ( kind = 8 ) xy11
  real ( kind = 8 ) xy12
  real ( kind = 8 ) xy2
  real ( kind = 8 ) xy21
  real ( kind = 8 ) xy22
  real ( kind = 8 ) z
  real ( kind = 8 ) zero
  real ( kind = 8 ) zsq
!
!  Mathematical constants
!  CONS = ln(.5) + Euler's gamma
!
  data zero / 0.0d0 /
  data one /1.0d0 /
  data three /3.0d0 /
  data four /4.0d0 /
  data eight /8.0d0/
  data five5 / 5.5d0 /
  data sixty4 /64.0d0 /
  data oneov8 /0.125d0 /
  data p17 /1.716d-1/
  data two56 /256.0d0/
  data cons / -1.1593151565841244881d-1/
  data pi2 /6.3661977236758134308d-1/
  data twopi /6.2831853071795864769d0/
  data twopi1 /6.28125d0 /
  data twopi2 / 1.9353071795864769253d-3/
!
!  Machine-dependent constants
!
  data xmax /1.07d+09/
  data xsmall /9.31d-10/
  data xinf /1.7d+38/
!
!  Zeroes of Bessel functions
!
  data xj0 /2.4048255576957727686d+0/
  data xj1 /5.5200781102863106496d+0/
  data xy0 /8.9357696627916752158d-1/
  data xy1 /3.9576784193148578684d+0/
  data xy2 /7.0860510603017726976d+0/
  data xj01 / 616.0d+0/
  data xj02 /-1.4244423042272313784d-03/
  data xj11 /1413.0d+0/
  data xj12 / 5.4686028631064959660d-04/
  data xy01 / 228.0d+0/
  data xy02 / 2.9519662791675215849d-03/
  data xy11 /1013.0d+0/
  data xy12 / 6.4716931485786837568d-04/
  data xy21 /1814.0d+0/
  data xy22 / 1.1356030177269762362d-04/
!
!  Coefficients for rational approximation to ln(x/a)
!
  data plg/-2.4562334077563243311d+01,2.3642701335621505212d+02, &
           -5.4989956895857911039d+02,3.5687548468071500413d+02/
  data qlg/-3.5553900764052419184d+01,1.9400230218539473193d+02, &
           -3.3442903192607538956d+02,1.7843774234035750207d+02/
!
!  Coefficients for rational approximation of
!  J0(X) / (X**2 - XJ0**2),  XSMALL < |X| <= 4.0
!
  data pj0/6.6302997904833794242d+06,-6.2140700423540120665d+08, &
           2.7282507878605942706d+10,-4.1298668500990866786d+11, &
          -1.2117036164593528341d-01, 1.0344222815443188943d+02, &
          -3.6629814655107086448d+04/
  data qj0/4.5612696224219938200d+05, 1.3985097372263433271d+08, &
           2.6328198300859648632d+10, 2.3883787996332290397d+12, &
           9.3614022392337710626d+02/
!
!  Coefficients for rational approximation of
!  J0(X) / (X**2 - XJ1**2), 4.0 < |X| <= 8.0
!
  data pj1/4.4176707025325087628d+03, 1.1725046279757103576d+04, &
           1.0341910641583726701d+04,-7.2879702464464618998d+03, &
          -1.2254078161378989535d+04,-1.8319397969392084011d+03, &
           4.8591703355916499363d+01, 7.4321196680624245801d+02/
  data qj1/3.3307310774649071172d+02,-2.9458766545509337327d+03, &
           1.8680990008359188352d+04,-8.4055062591169562211d+04, &
           2.4599102262586308984d+05,-3.5783478026152301072d+05, &
          -2.5258076240801555057d+01/
!
!  Coefficients for rational approximation of
!  (Y0(X) - 2 LN(X/XY0) J0(X)) / (X**2 - XY0**2),
!  XSMALL < |X| <= 3.0
!
  data py0/1.0102532948020907590d+04,-2.1287548474401797963d+06, &
           2.0422274357376619816d+08,-8.3716255451260504098d+09, &
           1.0723538782003176831d+11,-1.8402381979244993524d+01/
  data qy0/6.6475986689240190091d+02, 2.3889393209447253406d+05, &
           5.5662956624278251596d+07, 8.1617187777290363573d+09, &
           5.8873865738997033405d+11/
!
!  Coefficients for rational approximation of
!  (Y0(X) - 2 LN(X/XY1) J0(X)) / (X**2 - XY1**2),
!  3.0 < |X| <= 5.5
!
  data py1/-1.4566865832663635920d+04, 4.6905288611678631510d+06, &
           -6.9590439394619619534d+08, 4.3600098638603061642d+10, &
           -5.5107435206722644429d+11,-2.2213976967566192242d+13, &
            1.7427031242901594547d+01/
  data qy1/ 8.3030857612070288823d+02, 4.0669982352539552018d+05, &
            1.3960202770986831075d+08, 3.4015103849971240096d+10, &
            5.4266824419412347550d+12, 4.3386146580707264428d+14/
!
!  Coefficients for rational approximation of
!  (Y0(X) - 2 LN(X/XY2) J0(X)) / (X**2 - XY2**2),
!  5.5 < |X| <= 8.0
!
  data py2/ 2.1363534169313901632d+04,-1.0085539923498211426d+07, &
            2.1958827170518100757d+09,-1.9363051266772083678d+11, &
           -1.2829912364088687306d+11, 6.7016641869173237784d+14, &
           -8.0728726905150210443d+15,-1.7439661319197499338d+01/
  data qy2/ 8.7903362168128450017d+02, 5.3924739209768057030d+05, &
            2.4727219475672302327d+08, 8.6926121104209825246d+10, &
            2.2598377924042897629d+13, 3.9272425569640309819d+15, &
            3.4563724628846457519d+17/
!
!  Coefficients for Hart,s approximation, 8.0 < |X|.
!
  data p0/3.4806486443249270347d+03, 2.1170523380864944322d+04, &
          4.1345386639580765797d+04, 2.2779090197304684302d+04, &
          8.8961548424210455236d-01, 1.5376201909008354296d+02/
  data q0/3.5028735138235608207d+03, 2.1215350561880115730d+04, &
          4.1370412495510416640d+04, 2.2779090197304684318d+04, &
          1.5711159858080893649d+02/
  data p1/-2.2300261666214198472d+01,-1.1183429920482737611d+02, &
          -1.8591953644342993800d+02,-8.9226600200800094098d+01, &
          -8.8033303048680751817d-03,-1.2441026745835638459d+00/
  data q1/1.4887231232283756582d+03, 7.2642780169211018836d+03, &
          1.1951131543434613647d+04, 5.7105024128512061905d+03, &
          9.0593769594993125859d+01/
!
!  Check for error conditions.
!
  ax = abs ( arg )

  if ( jint == 1 .and. arg <= zero ) then
    result = -xinf
    return
  else if ( xmax < ax ) then
    result = zero
    return
  end if

  if ( eight < ax ) then
    go to 800
  end if

  if ( ax <= xsmall ) then
    if ( jint == 0 ) then
      result = one
    else
      result = pi2 * ( log ( ax ) + cons )
    end if
    return
  end if
!
!  Calculate J0 for appropriate interval, preserving
!  accuracy near the zero of J0.
!
  zsq = ax * ax

  if ( ax <= four ) then
    xnum = ( pj0(5) * zsq + pj0(6) ) * zsq + pj0(7)
    xden = zsq + qj0(5)
    do i = 1, 4
      xnum = xnum * zsq + pj0(i)
      xden = xden * zsq + qj0(i)
    end do
    prod = ( ( ax - xj01 / two56 ) - xj02 ) * ( ax + xj0 )
  else
    wsq = one - zsq / sixty4
    xnum = pj1(7) * wsq + pj1(8)
    xden = wsq + qj1(7)
    do i = 1, 6
      xnum = xnum * wsq + pj1(i)
      xden = xden * wsq + qj1(i)
    end do
    prod = ( ax + xj1 ) * ( ( ax - xj11 / two56 ) - xj12 )
  end if

  result = prod * xnum / xden

  if ( jint == 0 ) then
    return
  end if
!
!  Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x),
!  where xn is a zero of Y0.
!
  if ( ax <= three ) then
    up = ( ax - xy01 / two56 ) - xy02
    xy = xy0
  else if ( ax <= five5 ) then
    up = ( ax - xy11 / two56 ) - xy12
    xy = xy1
  else
    up = ( ax - xy21 / two56 ) - xy22
    xy = xy2
  end if

  down = ax + xy

  if ( abs ( up ) < p17 * down ) then
    w = up / down
    wsq = w * w
    xnum = plg(1)
    xden = wsq + qlg(1)
    do i = 2, 4
      xnum = xnum * wsq + plg(i)
      xden = xden * wsq + qlg(i)
    end do
    resj = pi2 * result * w * xnum / xden
  else
    resj = pi2 * result * log ( ax / xy )
  end if
!
!  Now calculate Y0 for appropriate interval, preserving
!  accuracy near the zero of Y0.
!
  if ( ax <= three ) then
    xnum = py0(6) * zsq + py0(1)
    xden = zsq + qy0(1)
    do i = 2, 5
      xnum = xnum * zsq + py0(i)
      xden = xden * zsq + qy0(i)
    end do
  else if ( ax <= five5 ) then
    xnum = py1(7) * zsq + py1(1)
    xden = zsq + qy1(1)
    do i = 2, 6
      xnum = xnum * zsq + py1(i)
      xden = xden * zsq + qy1(i)
    end do
  else
    xnum = py2(8) * zsq + py2(1)
    xden = zsq + qy2(1)
    do i = 2, 7
      xnum = xnum * zsq + py2(i)
      xden = xden * zsq + qy2(i)
    end do
  end if

  result = resj + up * down * xnum / xden

  return
!
!  Calculate J0 or Y0 for 8.0 < |ARG|.
!
  800 continue

  z = eight / ax
  w = ax / twopi
  w = aint ( w ) + oneov8
  w = ( ax - w * twopi1 ) - w * twopi2
  zsq = z * z
  xnum = p0(5) * zsq + p0(6)
  xden = zsq + q0(5)
  up = p1(5) * zsq + p1(6)
  down = zsq + q1(5)

  do i = 1, 4
    xnum = xnum * zsq + p0(i)
    xden = xden * zsq + q0(i)
    up = up * zsq + p1(i)
    down = down * zsq + q1(i)
  end do

  r0 = xnum / xden
  r1 = up / down

  if ( jint == 0 ) then
    result = sqrt ( pi2 / ax ) &
      * ( r0 * cos ( w ) - z * r1 * sin ( w ) )
  else
    result = sqrt ( pi2 / ax ) &
      * ( r0 * sin ( w ) + z * r1 * cos ( w ) )
  end if

  return
end subroutine 

subroutine unit_test_bessel_y0_values()
           use spec_func_xmm2r8, only : besy0_xmm2r8, caljy0_xmm2r8
           use mod_fpcompare 
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           !use , intrinsic           :: IEEE_ARITHMETIC
           implicit none 
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif
              interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
              end interface
              character(len=60),           parameter  :: header = "[TEST #1:  besy0_xmm2r8 -- START]"
              character(len=60),           parameter  :: footer = "[TEST #1:  besy0_xmm2r8 -- END]"
              integer(kind=c_long_long),   automatic  :: start, end 
              integer(kind=c_long_long),   automatic  :: start_c, end_c,tsc_elapsed
              real(kind=dp),dimension(16), parameter  :: x_args     = [&
                                                                     0.1e+00_dp, &
                                                                     1.0e+00_dp, &
                                                                     2.0e+00_dp, &
                                                                     3.0e+00_dp, &
                                                                     4.0e+00_dp, &
                                                                     5.0e+00_dp, &
                                                                     6.0e+00_dp, &
                                                                     7.0e+00_dp, &
                                                                     8.0e+00_dp, &
                                                                     9.0e+00_dp, &
                                                                     10.0e+00_dp, &
                                                                     11.0e+00_dp, &
                                                                     12.0e+00_dp, &
                                                                     13.0e+00_dp, &
                                                                     14.0e+00_dp, &
                                                                     15.0e+00_dp ]
              real(kind=dp),dimension(16), parameter  :: fx_results = [&
                                                                      -0.1534238651350367e+01_dp, &
                                                                       0.8825696421567696e-01_dp, &
                                                                       0.5103756726497451e+00_dp, &
                                                                       0.3768500100127904e+00_dp, &
                                                                      -0.1694073932506499e-01_dp, &
                                                                      -0.3085176252490338e+00_dp, &
                                                                      -0.2881946839815792e+00_dp, &
                                                                      -0.2594974396720926e-01_dp, &
                                                                       0.2235214893875662e+00_dp, &
                                                                       0.2499366982850247e+00_dp, &
                                                                       0.5567116728359939e-01_dp, &
                                                                      -0.1688473238920795e+00_dp, &
                                                                      -0.2252373126343614e+00_dp, &
                                                                      -0.7820786452787591e-01_dp, &
                                                                       0.1271925685821837e+00_dp, &
                                                                       0.2054642960389183e+00_dp ] 
             type(XMM2r8_t),                parameter  :: X_ARG1    = XMM2r8_t([0.1E+00_dp,1.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG2    = XMM2r8_t([2.0E+00_dp,3.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG3    = XMM2r8_t([4.0E+00_dp,5.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG4    = XMM2r8_t([6.0E+00_dp,7.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG5    = XMM2r8_t([8.0E+00_dp,9.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG6    = XMM2r8_t([10.0E+0_dp,11.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG7    = XMM2r8_t([12.0E+00_dp,13.0E+00_dp])
             type(XMM2r8_t),                parameter  :: X_ARG8    = XMM2r8_t([14.0E+00_dp,15.0E+00_dp])
            
             
             type(XMM2r8_t),                parameter  :: FX_RES1   = XMM2r8_t([-0.1534238651350367e+01_dp,0.8825696421567696e-01_dp])
             type(XMM2r8_t),                parameter  :: FX_RES2   = XMM2r8_t([0.5103756726497451e+00_dp,0.3768500100127904e+00_dp])
             type(XMM2r8_t),                parameter  :: FX_RES3   = XMM2r8_t([-0.1694073932506499e-01_dp,-0.3085176252490338e+00_dp])
             type(XMM2r8_t),                parameter  :: FX_RES4   = XMM2r8_t([-0.2881946839815792e+00_dp,-0.2594974396720926e-01_dp])
             type(XMM2r8_t),                parameter  :: FX_RES5   = XMM2r8_t([0.2235214893875662e+00_dp,0.2499366982850247e+00_dp])
             type(XMM2r8_t),                parameter  :: FX_RES6   = XMM2r8_t([0.5567116728359939e-01_dp,-0.1688473238920795e+00_dp])
             type(XMM2r8_t),                parameter  :: FX_RES7   = XMM2r8_t([-0.2252373126343614e+00_dp,-0.7820786452787591e-01_dp])
             type(XMM2r8_t),                parameter  :: FX_RES8   = XMM2r8_t([0.1271925685821837e+00_dp,0.2054642960389183e+00_dp])
             
             type(XMM2r8_t),                automatic  :: actual_xmm2r8
             type(Mask2_t),                 automatic  :: m_equal 
             real(kind=dp),                 automatic  :: actual_r8,sanity_r8,arg_r8,ref_r8   
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
              integer(kind=i4),             automatic  :: i__ 
              integer(kind=i4),             automatic  :: fail_count 
              print*,  header
              !Sanity check of reference function 
#if 0
              print*, "Sanity check of reference function."
              fail_count = 0
              do i__ = 1_i4, 16_i4 
                  arg_r8    = x_args(i__)
                  ref_r8    = fx_results(i__)
                  sanity_r8 = besy0(arg_r8)
                  if(ref_r8 .EqualTo. sanity_r8) then 
                     print*, "Floating-point compare #:",i__, "PASSED!!"
                     write(*,'(2x,g24.16,2x,g24.16)') sanity_r8, ref_r8 
                  else 
                     fail_count = fail_count+1_i4 
                     print*, "Floating-point compare #:",i__, "FAILED!!"
                     write(*,'(2x,g24.16,2x,g24.16)') sanity_r8, ref_r8 
                  end if 
              end do 
              if(fail_count .gt. 0_i4) then 
                 print*, "Detected: ", fail_count, "failed results!!"
                 !stop "ERRORS DETECTED!!"
              end if 
#endif 
              m_equal.m = [.false.,.false.]
              print *, "Starting main compute-comparison test."
#if 0
              ret_val =   raise(SIGTRAP)
               if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG1)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES1)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG1-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG1-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES1
              else 
                 print*, "[X_ARG1-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES1
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG2)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES2)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG2-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG2-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES2
              else 
                 print*, "[X_ARG2-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES2
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG3)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES2)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG3-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG3-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES3
              else 
                 print*, "[X_ARG3-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES3
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG4)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES4)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG4-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG4-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES4
              else 
                 print*, "[X_ARG4-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES4
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG5)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES5)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG5-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG5-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES5
              else 
                 print*, "[X_ARG5-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES5
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG6)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES6)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG6-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG6-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES6
              else 
                 print*, "[X_ARG6-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES6
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG7)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES7)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG7-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG7-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES7
              else 
                 print*, "[X_ARG7-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES7
              endif
              start         = rdtsc_wrap()
              actual_xmm2r8 = besy0_xmm2r8(X_ARG8)
              end           = rdtsc_wrap()
              m_equal       = xmm2r8_equalto_xmm2r8(actual_xmm2r8,FX_RES8)
              start_c       = start-RDTSC_LATENCY
              end_c         = end-RDTSC_LATENCY
              tsc_elapsed   = end_c-start_c 
              print*,"[X_ARG8-RDTSC:]", tsc_elapsed, "TSC cycles."
              if(any(m_equal.m)) then 
                 print*, "[X_ARG8-PASSED:] actual:",actual_xmm2r8,"expected=",FX_RES8
              else 
                 print*, "[X_ARG8-FAILED:] actual:",actual_xmm2r8,"expected=",FX_RES8
              endif
              print*, footer 
end subroutine unit_test_bessel_y0_values


end module mod_test_bessel_y0_values


program main 
   use mod_test_bessel_y0_values
   call unit_test_bessel_y0_values()
end program main 