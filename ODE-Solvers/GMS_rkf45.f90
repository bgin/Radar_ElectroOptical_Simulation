
module GSM_mod_rkf45

  

  use mod_kinds, only : i4, i8, sp, dp
  implicit none
  
  public


  contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
    subroutine r4_fehl(f,neqn,y,t,h,yp,f1,f2,f3,f4,f5,s) !GCC$ ATTRIBUTES hot :: r4_fehl !GCC$ ATTRIBUTES aligned(32) :: r4_fehl !GCC$ ATTRIBUTES inline :: r4_fehl
#elif defined __INTEL_COMPILER
      subroutine r4_fehl(f,neqn,y,t,h,yp,f1,f2,f3,f4,f5,s)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r4_fehl
        !DIR$ ATTRIBUTES INLINE :: r4_fehl
#endif
!*****************************************************************************80
!
!! R4_FEHL takes one Fehlberg fourth-fifth order step (single precision).
!
!  Discussion:
!
!    This routine integrates a system of NEQN first order ordinary differential
!    equations of the form
!      dY(i)/dT = F(T,Y(1:NEQN))
!    where the initial values Y and the initial derivatives
!    YP are specified at the starting point T.
!
!    The routine advances the solution over the fixed step H and returns
!    the fifth order (sixth order accurate locally) solution
!    approximation at T+H in array S.
!
!    The formulas have been grouped to control loss of significance.
!    The routine should be called with an H not smaller than 13 units of
!    roundoff in T so that the various independent arguments can be
!    distinguished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2004
!
!  Author:
!
!    Original FORTRAN77 version by Herman Watts, Lawrence Shampine.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Erwin Fehlberg,
!    Low-order Classical Runge-Kutta Formulas with Stepsize Control,
!    NASA Technical Report R-315, 1969.
!
!    Lawrence Shampine, Herman Watts, S Davenport,
!    Solving Non-stiff Ordinary Differential Equations - The State of the Art,
!    SIAM Review,
!    Volume 18, pages 376-411, 1976.
!
!  Parameters:
!
!    Input, external F, a user-supplied subroutine to evaluate the
!    derivatives Y'(T), of the form:
!
!      subroutine f ( t, y, yp )
!      real ( kind = 4 ) t
!      real ( kind = 4 ) y(*)
!      real ( kind = 4 ) yp(*)
!
!    Input, integer ( kind = 4 ) NEQN, the number of equations to be integrated.
!
!    Input, real ( kind = 4 ) Y(NEQN), the current value of the 
!    dependent variable.
!
!    Input, real ( kind = 4 ) T, the current value of the independent 
!    variable.
!
!    Input, real ( kind = 4 ) H, the step size to take.
!
!    Input, real ( kind = 4 ) YP(NEQN), the current value of the 
!    derivative of the dependent variable.
!
!    Output, real ( kind = 4 ) F1(NEQN), F2(NEQN), F3(NEQN), F4(NEQN), 
!    F5(NEQN), derivative values needed for the computation.
!
!    Output, real ( kind = 4 ) S(NEQN), the estimate of the solution at T+H.
!
  !implicit none

  integer ( kind = i4 ),                intent(in) :: neqn
  
  external f
  real ( kind = sp),   dimension(neqn), intent(out) :: f1
  real ( kind = sp),   dimension(neqn), intent(out) :: f2
  real ( kind = sp),   dimension(neqn), intent(out) :: f3
  real ( kind = sp),   dimension(neqn), intent(out) :: f4
  real ( kind = sp),   dimension(neqn), intent(out) :: f5
  real ( kind = sp),                    intent(in)  :: h
  real ( kind = sp),   dimension(neqn), intent(out) :: s
  real ( kind = sp),                    intent(in)  :: t
  real ( kind = sp),   dimension(neqn), intent(in)  :: y
  real ( kind = sp),   dimension(neqn), intent(in)  :: yp
  real ( kind = 4 ) :: ch
  ch = h / 4.0E+00

  f5(1:neqn) = y(1:neqn) + ch * yp(1:neqn)

  call f ( t + ch, f5, f1 )

  ch = 3.0E+00 * h / 32.0E+00

  f5(1:neqn) = y(1:neqn) + ch * ( yp(1:neqn) + 3.0E+00 * f1(1:neqn) )

  call f ( t + 3.0E+00 * h / 8.0E+00, f5, f2 )

  ch = h / 2197.0E+00

  f5(1:neqn) = y(1:neqn) + ch * &
  ( 1932.0E+00 * yp(1:neqn) &
  + ( 7296.0E+00 * f2(1:neqn) - 7200.0E+00 * f1(1:neqn) ) &
  )

  call f ( t + 12.0E+00 * h / 13.0E+00, f5, f3 )

  ch = h / 4104.0E+00

  f5(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( 8341.0E+00 * yp(1:neqn) - 845.0E+00 * f3(1:neqn) ) &
  + ( 29440.0E+00 * f2(1:neqn) - 32832.0E+00 * f1(1:neqn) ) &
  )

  call f ( t + h, f5, f4 )

  ch = h / 20520.0E+00

  f1(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( -6080.0E+00 * yp(1:neqn) &
    + ( 9295.0E+00 * f3(1:neqn) - 5643.0E+00 * f4(1:neqn) ) &
    ) &
  + ( 41040.0E+00 * f1(1:neqn) - 28352.0E+00 * f2(1:neqn) ) &
  )

  call f ( t + h / 2.0E+00, f1, f5 )
!
!  Ready to compute the approximate solution at T+H.
!
  ch = h / 7618050.0E+00

  s(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( 902880.0E+00 * yp(1:neqn) &
    + ( 3855735.0E+00 * f3(1:neqn) - 1371249.0E+00 * f4(1:neqn) ) ) &
  + ( 3953664.0E+00 * f2(1:neqn) + 277020.0E+00 * f5(1:neqn) ) &
  )

  return
end subroutine r4_fehl

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
subroutine r4_rkf45(f,neqn,y,yp,t,tout,relerr,abserr,flag)  !GCC$ ATTRIBUTES hot :: r4_rkf45 !GCC$ ATTRIBUTES aligned(32) :: r4_rkf45
#elif defined __INTEL_COMPILER
  subroutine r4_rkf45(f,neqn,y,yp,t,tout,relerr,abserr,flag)
     !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r4_rkf45
#endif

!*****************************************************************************80
!
!! R4_RKF45 carries out the Runge-Kutta-Fehlberg method (single precision).
!
!  Discussion:
!
!    This routine is primarily designed to solve non-stiff and mildly stiff
!    differential equations when derivative evaluations are inexpensive.
!    It should generally not be used when the user is demanding
!    high accuracy.
!
!    This routine integrates a system of NEQN first-order ordinary differential
!    equations of the form:
!
!      dY(i)/dT = F(T,Y(1),Y(2),...,Y(NEQN))
!
!    where the Y(1:NEQN) are given at T.
!
!    Typically the subroutine is used to integrate from T to TOUT but it
!    can be used as a one-step integrator to advance the solution a
!    single step in the direction of TOUT.  On return, the parameters in
!    the call list are set for continuing the integration.  The user has
!    only to call again (and perhaps define a new value for TOUT).
!
!    Before the first call, the user must 
!
!    * supply the subroutine F(T,Y,YP) to evaluate the right hand side;
!      and declare F in an EXTERNAL statement;
!
!    * initialize the parameters:
!      NEQN, Y(1:NEQN), T, TOUT, RELERR, ABSERR, FLAG.
!      In particular, T should initially be the starting point for integration,
!      Y should be the value of the initial conditions, and FLAG should 
!      normally be +1.
!
!    Normally, the user only sets the value of FLAG before the first call, and
!    thereafter, the program manages the value.  On the first call, FLAG should
!    normally be +1 (or -1 for single step mode.)  On normal return, FLAG will
!    have been reset by the program to the value of 2 (or -2 in single 
!    step mode), and the user can continue to call the routine with that 
!    value of FLAG.
!
!    (When the input magnitude of FLAG is 1, this indicates to the program 
!    that it is necessary to do some initialization work.  An input magnitude
!    of 2 lets the program know that that initialization can be skipped, 
!    and that useful information was computed earlier.)
!
!    The routine returns with all the information needed to continue
!    the integration.  If the integration reached TOUT, the user need only
!    define a new TOUT and call again.  In the one-step integrator
!    mode, returning with FLAG = -2, the user must keep in mind that 
!    each step taken is in the direction of the current TOUT.  Upon 
!    reaching TOUT, indicated by the output value of FLAG switching to 2,
!    the user must define a new TOUT and reset FLAG to -2 to continue 
!    in the one-step integrator mode.
!
!    In some cases, an error or difficulty occurs during a call.  In that case,
!    the output value of FLAG is used to indicate that there is a problem
!    that the user must address.  These values include:
!
!    * 3, integration was not completed because the input value of RELERR, the 
!      relative error tolerance, was too small.  RELERR has been increased 
!      appropriately for continuing.  If the user accepts the output value of
!      RELERR, then simply reset FLAG to 2 and continue.
!
!    * 4, integration was not completed because more than MAXNFE derivative 
!      evaluations were needed.  This is approximately (MAXNFE/6) steps.
!      The user may continue by simply calling again.  The function counter 
!      will be reset to 0, and another MAXNFE function evaluations are allowed.
!
!    * 5, integration was not completed because the solution vanished, 
!      making a pure relative error test impossible.  The user must use 
!      a non-zero ABSERR to continue.  Using the one-step integration mode 
!      for one step is a good way to proceed.
!
!    * 6, integration was not completed because the requested accuracy 
!      could not be achieved, even using the smallest allowable stepsize. 
!      The user must increase the error tolerances ABSERR or RELERR before
!      continuing.  It is also necessary to reset FLAG to 2 (or -2 when 
!      the one-step integration mode is being used).  The occurrence of 
!      FLAG = 6 indicates a trouble spot.  The solution is changing 
!      rapidly, or a singularity may be present.  It often is inadvisable 
!      to continue.
!
!    * 7, it is likely that this routine is inefficient for solving
!      this problem.  Too much output is restricting the natural stepsize
!      choice.  The user should use the one-step integration mode with 
!      the stepsize determined by the code.  If the user insists upon 
!      continuing the integration, reset FLAG to 2 before calling 
!      again.  Otherwise, execution will be terminated.
!
!    * 8, invalid input parameters, indicates one of the following:
!      NEQN <= 0;
!      T = TOUT and |FLAG| /= 1;
!      RELERR < 0 or ABSERR < 0;
!      FLAG == 0  or FLAG < -2 or 8 < FLAG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2011
!
!  Author:
!
!    Original FORTRAN77 version by Herman Watts, Lawrence Shampine.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Erwin Fehlberg,
!    Low-order Classical Runge-Kutta Formulas with Stepsize Control,
!    NASA Technical Report R-315, 1969.
!
!    Lawrence Shampine, Herman Watts, S Davenport,
!    Solving Non-stiff Ordinary Differential Equations - The State of the Art,
!    SIAM Review,
!    Volume 18, pages 376-411, 1976.
!
!  Parameters:
!
!    Input, external F, a user-supplied subroutine to evaluate the
!    derivatives Y'(T), of the form:
!
!      subroutine f ( t, y, yp )
!      real ( kind = 4 ) t
!      real ( kind = 4 ) y(*)
!      real ( kind = 4 ) yp(*)
!
!    Input, integer ( kind = 4 ) NEQN, the number of equations to be integrated.
!
!    Input/output, real ( kind = 4 ) Y(NEQN), the current solution vector at T.
!
!    Input/output, real ( kind = 4 ) YP(NEQN), the current value of the
!    derivative of the dependent variable.  The user should not set or alter
!    this information!
!
!    Input/output, real ( kind = 4 ) T, the current value of the independent
!    variable.
!
!    Input, real ( kind = 4 ) TOUT, the output point at which solution is
!    desired.  TOUT = T is allowed on the first call only, in which case 
!    the routine returns with FLAG = 2 if continuation is possible.
!
!    Input, real ( kind = 4 ) RELERR, ABSERR, the relative and absolute 
!    error tolerances for the local error test.  At each step the code
!    requires:
!      abs ( local error ) <= RELERR * abs ( Y ) + ABSERR
!    for each component of the local error and the solution vector Y.
!    RELERR cannot be "too small".  If the routine believes RELERR has been
!    set too small, it will reset RELERR to an acceptable value and return
!    immediately for user action.
!
!    Input/output, integer ( kind = 4 ) FLAG, indicator for status of 
!    integration.  On the first call, set FLAG to +1 for normal use, or to 
!    -1 for single step mode.  On return, a value of 2 or -2 indicates normal 
!    progress, while any other value indicates a problem that should be 
!    addressed.
!
  

  integer ( kind = sp ),       intent(in) ::  neqn
  
  real ( kind = sp ) abserr
  real ( kind = sp ), save :: abserr_save = -1.0E+00
  real ( kind = sp ) ae
  real ( kind = sp ) dt
  real ( kind = sp ) ee
  real ( kind = sp ) eeoet
  real ( kind = sp ) eps
  real ( kind = sp ) esttol
  real ( kind = sp ) et
  external f
  real ( kind = sp ),  dimension(neqn) ::  f1
  real ( kind = sp ),  dimension(neqn) ::  f2
  real ( kind = sp),   dimension(neqn) ::  f3
  real ( kind = sp ),  dimension(neqn) ::  f4
  real ( kind = sp ),  dimension(neqn) ::  f5
  real ( kind = sp ),  save :: h = -1.0E+00
  logical :: hfaild
  real ( kind = sp ) hmin
  integer ( kind = i4 ), intent(in) :: flag
  integer ( kind = i4 ), save :: flag_save = -1000
  integer ( kind = i4 ), save :: init = -1000
  integer ( kind = i4 ) :: k
  integer ( kind = i4 ), save :: kflag = -1000
  integer ( kind = i4 ), save :: kop = -1
  integer ( kind = i4 ), parameter :: maxnfe = 3000
  integer ( kind = i4 ) :: mflag
  integer ( kind = i4 ), save :: nfe = -1
  logical :: output
  real ( kind = sp ) relerr
  real ( kind = sp ) relerr_min
  real ( kind = sp ), save :: relerr_save = -1.0E+00
  real ( kind = sp), parameter :: remin = 1.0E-12
  real ( kind = sp ) :: s
  real ( kind = sp ) :: scale
  real ( kind = sp ), intent(inout) ::  t
  real ( kind = sp )  :: tol
  real ( kind = sp )  :: toln
  real ( kind = sp ), intent(inout) ::  tout
  real ( kind = sp ), dimension(neqn), intent(inout) ::  y
  real ( kind = sp ), dimension(neqn), intent(inout) ::  yp
  real ( kind = sp ) :: ypk
!
!  Check the input parameters.
!
  eps = epsilon ( eps )

  if ( neqn < 1 ) then
    flag = 8
    return
  end if

  if ( relerr < 0.0E+00 ) then
    flag = 8
    return
  end if

  if ( abserr < 0.0E+00 ) then
    flag = 8
    return
  end if

  if ( flag == 0 .or. 8 < flag .or. flag < -2 ) then
    flag = 8
    return
  end if

  mflag = abs ( flag )
!
!  Is this a continuation call?
!
  if ( mflag /= 1 ) then

    if ( t == tout .and. kflag /= 3 ) then
      flag = 8
      return
    end if

    if ( mflag == 2 ) then

      if ( kflag == 3 ) then

        flag = flag_save
        mflag = abs ( flag )

      else if ( init == 0 ) then

        flag = flag_save

      else if ( kflag == 4 ) then

        nfe = 0

      else if ( kflag == 5 .and. abserr == 0.0E+00 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  KFLAG = 5 and ABSERR = 0.0'
        stop

      else if ( &
        kflag == 6 .and. &
        relerr <= relerr_save .and. &
        abserr <= abserr_save ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  KFLAG = 6 and'
        write ( *, '(a)' ) '  RELERR <= RELERR_SAVE and'
        write ( *, '(a)' ) '  ABSERR <= ABSERR_SAVE'

        stop

      end if
!
!  FLAG = 3, 4, 5, 6, 7 or 8.
!
    else

      if ( flag == 3 ) then

        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if

      else if ( flag == 4 ) then

        nfe = 0
        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if

      else if ( flag == 5 .and. 0.0E+00 < abserr ) then

        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if
!
!  Integration cannot be continued because the user did not respond to
!  the instructions pertaining to FLAG = 5, 6, 7 or 8.
!
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  Integration cannot be continued.'
        write ( *, '(a)' ) '  The user did not respond to the output'
        write ( *, '(a)' ) '  value FLAG = 5, 6, 7, or 8.'
        stop
      end if

    end if

  end if
!
!  Save the input value of FLAG.  
!  Set the continuation flag KFLAG for subsequent input checking.
!
  flag_save = flag
  kflag = 0
!
!  Save RELERR and ABSERR for checking input on subsequent calls.
!
  relerr_save = relerr
  abserr_save = abserr
!
!  Restrict the relative error tolerance to be at least 
!
!    2 * EPS + REMIN 
!
!  to avoid limiting precision difficulties arising from impossible 
!  accuracy requests.
!
  relerr_min = 2.0E+00 * epsilon ( relerr_min ) + remin
!
!  Is the relative error tolerance too small?
!
  if ( relerr < relerr_min ) then
    relerr = relerr_min
    flag = 3
    kflag = 3
    return
  end if

  dt = tout - t
!
!  Initialization:
!
!  Set the initialization completion indicator, INIT;
!  set the indicator for too many output points, KOP;
!  evaluate the initial derivatives;
!  set the counter for function evaluations, NFE;
!  estimate the starting stepsize.
!
  if ( mflag == 1 ) then

    init = 0
    kop = 0
    call f ( t, y, yp )
    nfe = 1

    if ( t == tout ) then
      flag = 2
      return
    end if

  end if

  if ( init == 0 ) then

    init = 1
    h = abs ( dt )
    toln = 0.0E+00

    do k = 1, neqn
      tol = relerr * abs ( y(k) ) + abserr
      if ( 0.0E+00 < tol ) then
        toln = tol
        ypk = abs ( yp(k) )
        if ( tol < ypk * h**5 ) then
          h = ( tol / ypk )**0.2E+00
        end if
      end if
    end do

    if ( toln <= 0.0E+00 ) then
      h = 0.0E+00
    end if

    h = max ( h, 26.0E+00 * eps * max ( abs ( t ), abs ( dt ) ) )
    flag_save = sign ( 2, flag )

  end if
!
!  Set the stepsize for integration in the direction from T to TOUT.
!
  h = sign ( h, dt )
!
!  Test to see if too may output points are being requested.
!
  if ( 2.0E+00 * abs ( dt ) <= abs ( h ) ) then
    kop = kop + 1
  end if
!
!  Unnecessary frequency of output.
!
  if ( kop == 100 ) then
    kop = 0
    flag = 7
    return
  end if
!
!  If we are too close to the output point, then simply extrapolate and return.
!
  if ( abs ( dt ) <= 26.0E+00 * eps * abs ( t ) ) then
    t = tout
    y(1:neqn) = y(1:neqn) + dt * yp(1:neqn)
    call f ( t, y, yp )
    nfe = nfe + 1
    flag = 2
    return
  end if
!
!  Initialize the output point indicator.
!
  output = .false.
!
!  To avoid premature underflow in the error tolerance function,
!  scale the error tolerances.
!
  scale = 2.0E+00 / relerr
  ae = scale * abserr
!
!  Step by step integration.
!
  do

    hfaild = .false.
!
!  Set the smallest allowable stepsize.
!
    hmin = 26.0E+00 * eps * abs ( t )
!
!  Adjust the stepsize if necessary to hit the output point.
!
!  Look ahead two steps to avoid drastic changes in the stepsize and
!  thus lessen the impact of output points on the code.
!
    dt = tout - t

    if ( 2.0E+00 * abs ( h ) <= abs ( dt ) ) then

    else
!
!  Will the next successful step complete the integration to the output point?
!
      if ( abs ( dt ) <= abs ( h ) ) then
        output = .true.
        h = dt
      else
        h = 0.5E+00 * dt
      end if

    end if
!
!  Here begins the core integrator for taking a single step.
!
!  The tolerances have been scaled to avoid premature underflow in
!  computing the error tolerance function ET.
!  To avoid problems with zero crossings, relative error is measured
!  using the average of the magnitudes of the solution at the
!  beginning and end of a step.
!  The error estimate formula has been grouped to control loss of
!  significance.
!
!  To distinguish the various arguments, H is not permitted
!  to become smaller than 26 units of roundoff in T.
!  Practical limits on the change in the stepsize are enforced to
!  smooth the stepsize selection process and to avoid excessive
!  chattering on problems having discontinuities.
!  To prevent unnecessary failures, the code uses 9/10 the stepsize
!  it estimates will succeed.
!
!  After a step failure, the stepsize is not allowed to increase for
!  the next attempted step.  This makes the code more efficient on
!  problems having discontinuities and more effective in general
!  since local extrapolation is being used and extra caution seems
!  warranted.
!
!  Test the number of derivative function evaluations.
!  If okay, try to advance the integration from T to T+H.
!
    do
!
!  Have we done too much work?
!
      if ( maxnfe < nfe ) then
        flag = 4
        kflag = 4
        return
      end if
!
!  Advance an approximate solution over one step of length H.
!
      call r4_fehl ( f, neqn, y, t, h, yp, f1, f2, f3, f4, f5, f1 )
      nfe = nfe + 5
!
!  Compute and test allowable tolerances versus local error estimates
!  and remove scaling of tolerances.  The relative error is
!  measured with respect to the average of the magnitudes of the
!  solution at the beginning and end of the step.
!
      eeoet = 0.0E+00
 
      do k = 1, neqn

        et = abs ( y(k) ) + abs ( f1(k) ) + ae

        if ( et <= 0.0E+00 ) then
          flag = 5
          return
        end if

        ee = abs &
        ( ( -2090.0E+00 * yp(k) &
          + ( 21970.0E+00 * f3(k) - 15048.0E+00 * f4(k) ) &
          ) &
        + ( 22528.0E+00 * f2(k) - 27360.0E+00 * f5(k) ) &
        )

        eeoet = max ( eeoet, ee / et )

      end do

      esttol = abs ( h ) * eeoet * scale / 752400.0E+00

      if ( esttol <= 1.0E+00 ) then
        exit
      end if
!
!  Unsuccessful step.  Reduce the stepsize, try again.
!  The decrease is limited to a factor of 1/10.
!
      hfaild = .true.
      output = .false.

      if ( esttol < 59049.0E+00_dp ) then
        s = 0.9E+00_dp / esttol**0.2E+00_dp
      else
        s = 0.1E+00_dp
      end if

      h = s * h

      if ( abs ( h ) < hmin ) then
        flag = 6
        kflag = 6
        return
      end if

    end do
!
!  We exited the loop because we took a successful step.  
!  Store the solution for T+H, and evaluate the derivative there.
!
    t = t + h
    y(1:neqn) = f1(1:neqn)
    call f ( t, y, yp )
    nfe = nfe + 1
!
!  Choose the next stepsize.  The increase is limited to a factor of 5.
!  If the step failed, the next stepsize is not allowed to increase.
!
    if ( 0.0001889568E+00_dp < esttol ) then
      s = 0.9E+00_dp / esttol**0.2E+00_dp
    else
      s = 5.0E+00_dp
    end if

    if ( hfaild ) then
      s = min ( s, 1.0E+00 )
    end if

    h = sign ( max ( s * abs ( h ), hmin ), h )
!
!  End of core integrator
!
!  Should we take another step?
!
    if ( output ) then
      t = tout
      flag = 2
      return
    end if

    if ( flag <= 0 ) then
      exit
    end if

  end do
!
!  One step integration mode.
!
  flag = -2

  return
end subroutine r4_rkf45

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
subroutine r8_fehl(f,neqn,y,t,h,yp,f1,f2,f3,f4,f5,s) !GCC$ ATTRIBUTES hot :: r8_fehl !GCC$ ATTRIBUTES aligned(32) :: r8_fehl !GCC$ ATTRIBUTES inline :: r8_fehl
#elif defined __INTEL_COMPILER
  subroutine r8_fehl(f,neqn,y,t,h,yp,f1,f2,f3,f4,f5,s)
    !DIR$ ATTRIBUTES INLINE :: r8_fehl
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r8_fehl
#endif
!*****************************************************************************80
!
!! R8_FEHL takes one Fehlberg fourth-fifth order step (double precision).
!
!  Discussion:
!
!    This routine integrates a system of NEQN first order ordinary differential
!    equations of the form
!      dY(i)/dT = F(T,Y(1:NEQN))
!    where the initial values Y and the initial derivatives
!    YP are specified at the starting point T.
!
!    The routine advances the solution over the fixed step H and returns
!    the fifth order (sixth order accurate locally) solution
!    approximation at T+H in array S.
!
!    The formulas have been grouped to control loss of significance.
!    The routine should be called with an H not smaller than 13 units of
!    roundoff in T so that the various independent arguments can be
!    distinguished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2004
!
!  Author:
!
!    Original FORTRAN77 version by Herman Watts, Lawrence Shampine.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Erwin Fehlberg,
!    Low-order Classical Runge-Kutta Formulas with Stepsize Control,
!    NASA Technical Report R-315, 1969.
!
!    Lawrence Shampine, Herman Watts, S Davenport,
!    Solving Non-stiff Ordinary Differential Equations - The State of the Art,
!    SIAM Review,
!    Volume 18, pages 376-411, 1976.
!
!  Parameters:
!
!    Input, external F, a user-supplied subroutine to evaluate the
!    derivatives Y'(T), of the form:
!
!      subroutine f ( t, y, yp )
!      real ( kind = 8 ) t
!      real ( kind = 8 ) y(*)
!      real ( kind = 8 ) yp(*)
!
!    Input, integer ( kind = 4 ) NEQN, the number of equations to be integrated.
!
!    Input, real ( kind = 8 ) Y(NEQN), the current value of the
!    dependent variable.
!
!    Input, real ( kind = 8 ) T, the current value of the independent
!    variable.
!
!    Input, real ( kind = 8 ) H, the step size to take.
!
!    Input, real ( kind = 8 ) YP(NEQN), the current value of the
!    derivative of the dependent variable.
!
!    Output, real ( kind = 8 ) F1(NEQN), F2(NEQN), F3(NEQN), F4(NEQN),
!    F5(NEQN), derivative values needed for the computation.
!
!    Output, real ( kind = 8 ) S(NEQN), the estimate of the solution at T+H.
!
 

  integer ( kind = i4 ) :: neqn

  real ( kind = dp ) :: ch
  external f
  real ( kind = dp ), dimension(neqn) ::  f1
  real ( kind = dp ), dimension(neqn) ::  f2
  real ( kind = dp ), dimension(neqn) ::  f3
  real ( kind = dp ), dimension(neqn) ::  f4
  real ( kind = dp ), dimension(neqn) ::  f5
  real ( kind = dp ) ::  h
  real ( kind = dp ), dimension(neqn) ::  s
  real ( kind = dp ) :: t
  real ( kind = dp ), dimension(neqn) ::  y
  real ( kind = dp ), dimension(neqn) ::  yp

  ch = h / 4.0_dp

  f5(1:neqn) = y(1:neqn) + ch * yp(1:neqn)

  call f ( t + ch, f5, f1 )

  ch = 3.0_dp * h / 32.0_dp

  f5(1:neqn) = y(1:neqn) + ch * ( yp(1:neqn) + 3.0_dp * f1(1:neqn) )

  call f ( t + 3.0_dp * h / 8.0D+00, f5, f2 )

  ch = h / 2197.0_dp

  f5(1:neqn) = y(1:neqn) + ch * &
  ( 1932.0_dp * yp(1:neqn) &
  + ( 7296.0_dp * f2(1:neqn) - 7200.0_dp * f1(1:neqn) ) &
  )

  call f ( t + 12.0_dp * h / 13.0_dp, f5, f3 )

  ch = h / 4104.0_dp

  f5(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( 8341.0_dp * yp(1:neqn) - 845.0_dp * f3(1:neqn) ) &
  + ( 29440.0_dp * f2(1:neqn) - 32832.0_dp * f1(1:neqn) ) &
  )

  call f ( t + h, f5, f4 )

  ch = h / 20520.0_dp

  f1(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( -6080.0_dp * yp(1:neqn) &
    + ( 9295.0_dp * f3(1:neqn) - 5643.0_dp * f4(1:neqn) ) &
    ) &
  + ( 41040.0_dp * f1(1:neqn) - 28352.0_dp * f2(1:neqn) ) &
  )

  call f ( t + h / 2.0_dp, f1, f5 )
!
!  Ready to compute the approximate solution at T+H.
!
  ch = h / 7618050.0_dp

  s(1:neqn) = y(1:neqn) + ch * &
  ( &
    ( 902880.0_dp * yp(1:neqn) &
    + ( 3855735.0_dp * f3(1:neqn) - 1371249.0_dp * f4(1:neqn) ) ) &
  + ( 3953664.0_dp * f2(1:neqn) + 277020.0_dp * f5(1:neqn) ) &
  )

  return
end subroutine r8_fehl

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
subroutine r8_rkf45(f,neqn,y,yp,t,tout,relerr,abserr,flag)  !GCC$ ATTRIBUTES hot :: r4_rkf4 !GCC$ ATTRIBUTES aligned(32) :: r4_rkf45
#elif defined __INTEL_COMPILER
  subroutine r8_rkf45(f,neqn,y,yp,t,tout,relerr,abserr,flag)
     !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r8_rkf45
#endif

!*****************************************************************************80
!
!! R8_RKF45 carries out the Runge-Kutta-Fehlberg method (double precision).
!
!  Discussion:
!
!    This routine is primarily designed to solve non-stiff and mildly stiff
!    differential equations when derivative evaluations are inexpensive.
!    It should generally not be used when the user is demanding
!    high accuracy.
!
!    This routine integrates a system of NEQN first-order ordinary differential
!    equations of the form:
!
!      dY(i)/dT = F(T,Y(1),Y(2),...,Y(NEQN))
!
!    where the Y(1:NEQN) are given at T.
!
!    Typically the subroutine is used to integrate from T to TOUT but it
!    can be used as a one-step integrator to advance the solution a
!    single step in the direction of TOUT.  On return, the parameters in
!    the call list are set for continuing the integration.  The user has
!    only to call again (and perhaps define a new value for TOUT).
!
!    Before the first call, the user must
!
!    * supply the subroutine F(T,Y,YP) to evaluate the right hand side;
!      and declare F in an EXTERNAL statement;
!
!    * initialize the parameters:
!      NEQN, Y(1:NEQN), T, TOUT, RELERR, ABSERR, FLAG.
!      In particular, T should initially be the starting point for integration,
!      Y should be the value of the initial conditions, and FLAG should
!      normally be +1.
!
!    Normally, the user only sets the value of FLAG before the first call, and
!    thereafter, the program manages the value.  On the first call, FLAG should
!    normally be +1 (or -1 for single step mode.)  On normal return, FLAG will
!    have been reset by the program to the value of 2 (or -2 in single
!    step mode), and the user can continue to call the routine with that
!    value of FLAG.
!
!    (When the input magnitude of FLAG is 1, this indicates to the program
!    that it is necessary to do some initialization work.  An input magnitude
!    of 2 lets the program know that that initialization can be skipped,
!    and that useful information was computed earlier.)
!
!    The routine returns with all the information needed to continue
!    the integration.  If the integration reached TOUT, the user need only
!    define a new TOUT and call again.  In the one-step integrator
!    mode, returning with FLAG = -2, the user must keep in mind that
!    each step taken is in the direction of the current TOUT.  Upon
!    reaching TOUT, indicated by the output value of FLAG switching to 2,
!    the user must define a new TOUT and reset FLAG to -2 to continue
!    in the one-step integrator mode.
!
!    In some cases, an error or difficulty occurs during a call.  In that case,
!    the output value of FLAG is used to indicate that there is a problem
!    that the user must address.  These values include:
!
!    * 3, integration was not completed because the input value of RELERR, the
!      relative error tolerance, was too small.  RELERR has been increased
!      appropriately for continuing.  If the user accepts the output value of
!      RELERR, then simply reset FLAG to 2 and continue.
!
!    * 4, integration was not completed because more than MAXNFE derivative
!      evaluations were needed.  This is approximately (MAXNFE/6) steps.
!      The user may continue by simply calling again.  The function counter
!      will be reset to 0, and another MAXNFE function evaluations are allowed.
!
!    * 5, integration was not completed because the solution vanished,
!      making a pure relative error test impossible.  The user must use
!      a non-zero ABSERR to continue.  Using the one-step integration mode
!      for one step is a good way to proceed.
!
!    * 6, integration was not completed because the requested accuracy
!      could not be achieved, even using the smallest allowable stepsize.
!      The user must increase the error tolerances ABSERR or RELERR before
!      continuing.  It is also necessary to reset FLAG to 2 (or -2 when
!      the one-step integration mode is being used).  The occurrence of
!      FLAG = 6 indicates a trouble spot.  The solution is changing
!      rapidly, or a singularity may be present.  It often is inadvisable
!      to continue.
!
!    * 7, it is likely that this routine is inefficient for solving
!      this problem.  Too much output is restricting the natural stepsize
!      choice.  The user should use the one-step integration mode with
!      the stepsize determined by the code.  If the user insists upon
!      continuing the integration, reset FLAG to 2 before calling
!      again.  Otherwise, execution will be terminated.
!
!    * 8, invalid input parameters, indicates one of the following:
!      NEQN <= 0;
!      T = TOUT and |FLAG| /= 1;
!      RELERR < 0 or ABSERR < 0;
!      FLAG == 0  or FLAG < -2 or 8 < FLAG.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2011
!
!  Author:
!
!    Original FORTRAN77 version by Herman Watts, Lawrence Shampine.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Erwin Fehlberg,
!    Low-order Classical Runge-Kutta Formulas with Stepsize Control,
!    NASA Technical Report R-315, 1969.
!
!    Lawrence Shampine, Herman Watts, S Davenport,
!    Solving Non-stiff Ordinary Differential Equations - The State of the Art,
!    SIAM Review,
!    Volume 18, pages 376-411, 1976.
!
!  Parameters:
!
!    Input, external F, a user-supplied subroutine to evaluate the
!    derivatives Y'(T), of the form:
!
!      subroutine f ( t, y, yp )
!      real ( kind = 8 ) t
!      real ( kind = 8 ) y(*)
!      real ( kind = 8 ) yp(*)
!
!    Input, integer ( kind = 4 ) NEQN, the number of equations to be integrated.
!
!    Input/output, real ( kind = 8 ) Y(NEQN), the current solution vector at T.
!
!    Input/output, real ( kind = 8 ) YP(NEQN), the current value of the
!    derivative of the dependent variable.  The user should not set or alter
!    this information!
!
!    Input/output, real ( kind = 8 ) T, the current value of the independent
!    variable.
!
!    Input, real ( kind = 8 ) TOUT, the output point at which solution is
!    desired.  TOUT = T is allowed on the first call only, in which case
!    the routine returns with FLAG = 2 if continuation is possible.
!
!    Input, real ( kind = 8 ) RELERR, ABSERR, the relative and absolute
!    error tolerances for the local error test.  At each step the code
!    requires:
!      abs ( local error ) <= RELERR * abs ( Y ) + ABSERR
!    for each component of the local error and the solution vector Y.
!    RELERR cannot be "too small".  If the routine believes RELERR has been
!    set too small, it will reset RELERR to an acceptable value and return
!    immediately for user action.
!
!    Input/output, integer ( kind = 4 ) FLAG, indicator for status of 
!    integration.  On the first call, set FLAG to +1 for normal use, or to -1 
!    for single step mode.  On return, a value of 2 or -2 indicates normal 
!    progress, while any other value indicates a problem that should 
!    be addressed.
!
  

  integer ( kind = i4 ) :: neqn

  real ( kind = dp ) :: abserr
  real ( kind = dp ), save :: abserr_save = -1.0_dp
  real ( kind = dp ) :: ae
  real ( kind = dp ) :: dt
  real ( kind = dp ) :: ee
  real ( kind = dp ) :: eeoet
  real ( kind = dp ) :: eps
  real ( kind = dp ) :: esttol
  real ( kind = dp ) :: et
  external f
  real ( kind = dp ), dimension(neqn) ::  f1
  real ( kind = dp ), dimension(neqn) ::  f2
  real ( kind = dp ), dimension(neqn) ::  f3
  real ( kind = dp ), dimension(neqn) ::  f4
  real ( kind = dp ), dimension(neqn) ::  f5
  real ( kind = dp ), save :: h = -1.0_dp
  logical :: hfaild
  real ( kind = dp ) :: hmin
  integer ( kind = i4 ) :: flag
  integer ( kind = i4 ), save :: flag_save = -1000
  integer ( kind = i4 ), save :: init = -1000
  integer ( kind = i4 ) :: k
  integer ( kind = i4 ), save :: kflag = -1000
  integer ( kind = i4 ), save :: kop = -1
  integer ( kind = i4 ), parameter :: maxnfe = 3000
  integer ( kind = i4 ) :: mflag
  integer ( kind = i4 ), save :: nfe = -1
  logical :: output
  real ( kind = dp ) relerr
  real ( kind = dp ) relerr_min
  real ( kind = dp ), save :: relerr_save = -1.0_dp
  real ( kind = dp ), parameter :: remin = 1.0E-12_dp
  real ( kind = dp ) :: s
  real ( kind = dp ) :: scale
  real ( kind = dp ) :: t
  real ( kind = dp ) :: tol
  real ( kind = dp ) :: toln
  real ( kind = dp ) :: tout
  real ( kind = dp ), dimension(neqn) ::  y
  real ( kind = dp ), dimension(neqn) ::  yp
  real ( kind = dp ) :: ypk
!
!  Check the input parameters.
!
  eps = epsilon ( eps )

  if ( neqn < 1 ) then
    flag = 8
    return
  end if

  if ( relerr < 0.0_dp ) then
    flag = 8
    return
  end if

  if ( abserr < 0.0_dp ) then
    flag = 8
    return
  end if

  if ( flag == 0 .or. 8 < flag .or. flag < -2 ) then
    flag = 8
    return
  end if

  mflag = abs ( flag )
!
!  Is this a continuation call?
!
  if ( mflag /= 1 ) then

    if ( t == tout .and. kflag /= 3 ) then
      flag = 8
      return
    end if

    if ( mflag == 2 ) then

      if ( kflag == 3 ) then

        flag = flag_save
        mflag = abs ( flag )

      else if ( init == 0 ) then

        flag = flag_save

      else if ( kflag == 4 ) then

        nfe = 0

      else if ( kflag == 5 .and. abserr == 0.0_dp ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  KFLAG = 5 and ABSERR = 0.0'
        stop

      else if ( &
        kflag == 6 .and. &
        relerr <= relerr_save .and. &
        abserr <= abserr_save ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  KFLAG = 6 and'
        write ( *, '(a)' ) '  RELERR <= RELERR_SAVE and'
        write ( *, '(a)' ) '  ABSERR <= ABSERR_SAVE'

        stop

      end if
!
!  FLAG = 3, 4, 5, 6, 7 or 8.
!
    else

      if ( flag == 3 ) then

        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if

      else if ( flag == 4 ) then

        nfe = 0
        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if

      else if ( flag == 5 .and. 0.0_dp < abserr ) then

        flag = flag_save
        if ( kflag == 3 ) then
          mflag = abs ( flag )
        end if
!
!  Integration cannot be continued because the user did not respond to
!  the instructions pertaining to FLAG = 5, 6, 7 or 8.
!
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_RKF45 - Fatal error!'
        write ( *, '(a)' ) '  Integration cannot be continued.'
        write ( *, '(a)' ) '  The user did not respond to the output'
        write ( *, '(a)' ) '  value FLAG = 5, 6, 7, or 8.'
        stop
      end if

    end if

  end if
!
!  Save the input value of FLAG.
!  Set the continuation flag KFLAG for subsequent input checking.
!
  flag_save = flag
  kflag = 0
!
!  Save RELERR and ABSERR for checking input on subsequent calls.
!
  relerr_save = relerr
  abserr_save = abserr
!
!  Restrict the relative error tolerance to be at least
!
!    2 * EPS + REMIN
!
!  to avoid limiting precision difficulties arising from impossible
!  accuracy requests.
!
  relerr_min = 2.0_dp * epsilon ( relerr_min ) + remin
!
!  Is the relative error tolerance too small?
!
  if ( relerr < relerr_min ) then
    relerr = relerr_min
    flag = 3
    kflag = 3
    return
  end if

  dt = tout - t
!
!  Initialization:
!
!  Set the initialization completion indicator, INIT;
!  set the indicator for too many output points, KOP;
!  evaluate the initial derivatives;
!  set the counter for function evaluations, NFE;
!  estimate the starting stepsize.
!
  if ( mflag == 1 ) then

    init = 0
    kop = 0
    call f ( t, y, yp )
    nfe = 1

    if ( t == tout ) then
      flag = 2
      return
    end if

  end if

  if ( init == 0 ) then

    init = 1
    h = abs ( dt )
    toln = 0.0_dp

    do k = 1, neqn
      tol = relerr * abs ( y(k) ) + abserr
      if ( 0.0_dp < tol ) then
        toln = tol
        ypk = abs ( yp(k) )
        if ( tol < ypk * h**5 ) then
          h = ( tol / ypk )**0.2_dp
        end if
      end if
    end do

    if ( toln <= 0.0_dp ) then
      h = 0.0_dp
    end if

    h = max ( h, 26.0_dp * eps * max ( abs ( t ), abs ( dt ) ) )
    flag_save = sign ( 2, flag )

  end if
!
!  Set the stepsize for integration in the direction from T to TOUT.
!
  h = sign ( h, dt )
!
!  Test to see if too may output points are being requested.
!
  if ( 2.0_dp * abs ( dt ) <= abs ( h ) ) then
    kop = kop + 1
  end if
!
!  Unnecessary frequency of output.
!  Seems like an error I'm willing to tolerate!
!
! if ( kop == 100 ) then
  if ( kop == 10000 ) then
    kop = 0
    flag = 7
    return
  end if
!
!  If we are too close to the output point, then simply extrapolate and return.
!
  if ( abs ( dt ) <= 26.0_dp * eps * abs ( t ) ) then
    t = tout
    y(1:neqn) = y(1:neqn) + dt * yp(1:neqn)
    call f ( t, y, yp )
    nfe = nfe + 1
    flag = 2
    return
  end if
!
!  Initialize the output point indicator.
!
  output = .false.
!
!  To avoid premature underflow in the error tolerance function,
!  scale the error tolerances.
!
  scale = 2.0_dp / relerr
  ae = scale * abserr
!
!  Step by step integration.
!
  do

    hfaild = .false.
!
!  Set the smallest allowable stepsize.
!
    hmin = 26.0_dp * eps * abs ( t )
!
!  Adjust the stepsize if necessary to hit the output point.
!
!  Look ahead two steps to avoid drastic changes in the stepsize and
!  thus lessen the impact of output points on the code.
!
    dt = tout - t

    if ( 2.0_dp * abs ( h ) <= abs ( dt ) ) then

    else
!
!  Will the next successful step complete the integration to the output point?
!
      if ( abs ( dt ) <= abs ( h ) ) then
        output = .true.
        h = dt
      else
        h = 0.5_dp * dt
      end if

    end if
!
!  Here begins the core integrator for taking a single step.
!
!  The tolerances have been scaled to avoid premature underflow in
!  computing the error tolerance function ET.
!  To avoid problems with zero crossings, relative error is measured
!  using the average of the magnitudes of the solution at the
!  beginning and end of a step.
!  The error estimate formula has been grouped to control loss of
!  significance.
!
!  To distinguish the various arguments, H is not permitted
!  to become smaller than 26 units of roundoff in T.
!  Practical limits on the change in the stepsize are enforced to
!  smooth the stepsize selection process and to avoid excessive
!  chattering on problems having discontinuities.
!  To prevent unnecessary failures, the code uses 9/10 the stepsize
!  it estimates will succeed.
!
!  After a step failure, the stepsize is not allowed to increase for
!  the next attempted step.  This makes the code more efficient on
!  problems having discontinuities and more effective in general
!  since local extrapolation is being used and extra caution seems
!  warranted.
!
!  Test the number of derivative function evaluations.
!  If okay, try to advance the integration from T to T+H.
!
    do
!
!  Have we done too much work?
!
      if ( maxnfe < nfe ) then
        flag = 4
        kflag = 4
        return
      end if
!
!  Advance an approximate solution over one step of length H.
!
      call r8_fehl ( f, neqn, y, t, h, yp, f1, f2, f3, f4, f5, f1 )
      nfe = nfe + 5
!
!  Compute and test allowable tolerances versus local error estimates
!  and remove scaling of tolerances.  The relative error is
!  measured with respect to the average of the magnitudes of the
!  solution at the beginning and end of the step.
!
      eeoet = 0.0_dp

      do k = 1, neqn

        et = abs ( y(k) ) + abs ( f1(k) ) + ae

        if ( et <= 0.0_dp ) then
          flag = 5
          return
        end if

        ee = abs &
        ( ( -2090.0_dp * yp(k) &
          + ( 21970.0_dp * f3(k) - 15048.0_dp * f4(k) ) &
          ) &
        + ( 22528.0_dp * f2(k) - 27360.0_dp * f5(k) ) &
        )

        eeoet = max ( eeoet, ee / et )

      end do

      esttol = abs ( h ) * eeoet * scale / 752400.0_dp

      if ( esttol <= 1.0_dp ) then
        exit
      end if
!
!  Unsuccessful step.  Reduce the stepsize, try again.
!  The decrease is limited to a factor of 1/10.
!
      hfaild = .true.
      output = .false.

      if ( esttol < 59049.0_dp ) then
        s = 0.9_dp / esttol**0.2_dp
      else
        s = 0.1_dp
      end if

      h = s * h

      if ( abs ( h ) < hmin ) then
        flag = 6
        kflag = 6
        return
      end if

    end do
!
!  We exited the loop because we took a successful step.
!  Store the solution for T+H, and evaluate the derivative there.
!
    t = t + h
    y(1:neqn) = f1(1:neqn)
    call f ( t, y, yp )
    nfe = nfe + 1
!
!  Choose the next stepsize.  The increase is limited to a factor of 5.
!  If the step failed, the next stepsize is not allowed to increase.
!
    if ( 0.0001889568_dp < esttol ) then
      s = 0.9_dp / esttol**0.2_dp
    else
      s = 5.0_dp
    end if

    if ( hfaild ) then
      s = min ( s, 1.0_dp )
    end if

    h = sign ( max ( s * abs ( h ), hmin ), h )
!
!  End of core integrator
!
!  Should we take another step?
!
    if ( output ) then
      t = tout
      flag = 2
      return
    end if

    if ( flag <= 0 ) then
      exit
    end if

  end do
!
!  One step integration mode.
!
  flag = -2

  return
end subroutine r8_rkf45

!subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
!  implicit none

!  character ( len = 8 ) ampm
!  integer ( kind = 4 ) d
!  integer ( kind = 4 ) h
!  integer ( kind = 4 ) m
!  integer ( kind = 4 ) mm
!  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
!    'January  ', 'February ', 'March    ', 'April    ', &
!    'May      ', 'June     ', 'July     ', 'August   ', &
!    'September', 'October  ', 'November ', 'December ' /)
!  integer ( kind = 4 ) n
!  integer ( kind = 4 ) s
!  integer ( kind = 4 ) values(8)
!  integer ( kind = 4 ) y

!  call date_and_time ( values = values )

!  y = values(1)
 ! m = values(2)
 ! d = values(3)
 ! h = values(5)
 ! n = values(6)
 ! s = values(7)
!  mm = values(8)

!  if ( h < 12 ) then
!    ampm = 'AM'
!  else if ( h == 12 ) then
!    if ( n == 0 .and. s == 0 ) then
!      ampm = 'Noon'
!    else
!      ampm = 'PM'
!    end if
!  else
!!    h = h - 12
!    if ( h < 12 ) then
!      ampm = 'PM'
 !   else if ( h == 12 ) then
!      if ( n == 0 .and. s == 0 ) then
!        ampm = 'Midnight'
!      else
!        ampm = 'AM'
!      end if
!    end if
!  end if
!
!  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
!    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )
!
!  return
!end subroutine timestamp


end module GSM_mod_rkf45

