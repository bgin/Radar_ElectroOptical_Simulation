!*****************************************************************************************
!>
!  Module for [[fmin]] 1D derative-free function minimizer.
!
!### License
!  * [BSD-3](https://github.com/jacobwilliams/fmin/blob/master/LICENSE)
!    Slightly modified by Bernard Gingold, beniekg@gmail.com
!
!@note The default real kind (`wp`) can be
!      changed using optional preprocessor flags.
!      This library was built with real kind:


    module fmin_module

      !use iso_fortran_env
      use mod_kinds, only : sp,dp

    implicit none

    private

!#ifdef REAL32
!    integer,parameter,public :: fmin_rk = real32   !! real kind used by this module [4 bytes]
!#elif REAL64
!    integer,parameter,public :: fmin_rk = real64   !! real kind used by this module [8 bytes]
!#elif REAL128
!    integer,parameter,public :: fmin_rk = real128  !! real kind used by this module [16 bytes]
!#else
!    integer,parameter,public :: fmin_rk = real64   !! real kind used by this module [8 bytes]
!#endif

    !integer,parameter :: wp = fmin_rk  !! local copy of `fmin_rk` with a shorter name

    abstract interface
        function func_r4(x) result(f)
        !! interface for user function
        import :: sp
        implicit none
        real(sp),intent(in) :: x  !! indep. variable
        real(sp)            :: f  !! function value `f(x)`
        end function func_r4
    end interface

    abstract interface
        function func_r8(x) result(f)
        !! interface for user function
        import :: dp
        implicit none
        real(dp),intent(in) :: x  !! indep. variable
        real(dp)            :: f  !! function value `f(x)`
        end function func_r8
    end interface

    public :: fmin_r4,fmin_r8

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  An approximation x to the point where `f` attains a minimum on
!  the interval `(ax,bx)` is determined.
!
!  the method used is a combination of golden section search and
!  successive parabolic interpolation. convergence is never much slower
!  than that for a fibonacci search. if `f` has a continuous second
!  derivative which is positive at the minimum (which is not at `ax` or
!  `bx`), then convergence is superlinear, and usually of the order of
!  about 1.324.
!
!  the function `f` is never evaluated at two points closer together
!  than `eps*abs(fmin) + (tol/3)`, where `eps` is approximately the square
!  root of the relative machine precision. if `f` is a unimodal
!  function and the computed values of `f` are always unimodal when
!  separated by at least `eps*abs(x) + (tol/3)`, then fmin approximates
!  the abcissa of the global minimum of `f` on the interval `ax,bx` with
!  an error less than `3*eps*abs(fmin) + tol`. if `f` is not unimodal,
!  then `fmin` may approximate a local, but perhaps non-global, minimum to
!  the same accuracy.
!
!### Reference
!  * Richard brent, "algorithms for minimization without derivatives",
!    prentice - hall, inc. (1973).
!
!### See also
!  * [fmin from Netlib](http://www.netlib.org/fmm/fmin.f)

    function fmin_r4(f,ax,bx,tol) result(xmin)
      !dir$ attributes forceinline :: fmin_r4
      !dir$ attributes code_align : 32 :: fmin_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: fmin_r4
    implicit none

    procedure(func_r4)     :: f    !! the function to minimize
    real(sp),intent(in)    :: ax   !! left endpoint of initial interval
    real(sp),intent(in)    :: bx   !! right endpoint of initial interval
    real(sp),intent(in)    :: tol  !! desired length of the interval of
                                !! uncertainty of the final result (>=0)
    real(sp)            :: xmin !! abcissa approximating the point where
                                !! f attains a minimum

    real(sp) :: a,b,d,e,xm,p,q,r,tol1,tol2,u,v,w
    real(sp) :: fu,fv,fw,fx,x
    real(sp) :: abs,sqrt,sign

    real(sp),parameter :: c = (3.0_sp-sqrt(5.0_sp))/2.0_sp  !! squared inverse of golden ratio
    real(sp),parameter :: half = 0.5_sp
    real(sp),parameter :: sqrteps = sqrt(epsilon(1.0_sp))

    ! initialization

    a = ax
    b = bx
    v = a + c*(b - a)
    w = v
    x = v
    e = 0.0_dp
    fx = f(x)
    fv = fx
    fw = fx

    do    !  main loop starts here

        xm = half*(a + b)
        tol1 = sqrteps*abs(x) + tol*0.33333333333333333333333333333_sp
        tol2 = 2.0_sp*tol1

        !  check stopping criterion

        if (abs(x - xm) <= (tol2 - half*(b - a))) then
            ! write(*,*) 'x             = ', x
            ! write(*,*) 'xm            = ', xm
            ! write(*,*) 'abs(x - xm)   = ', abs(x - xm)
            ! write(*,*) 'tol2          = ', tol2
            ! write(*,*) 'half*(b - a)  = ', half*(b - a)
            exit
        end if

        ! is golden-section necessary

        if (abs(e) <= tol1) then

            !  a golden-section step

            if (x >= xm) then
                e = a - x
            else
                e = b - x
            end if
            d = c*e

        else

            !  fit parabola

            r = (x - w)*(fx - fv)
            q = (x - v)*(fx - fw)
            p = (x - v)*q - (x - w)*r
            q = 2.0_sp*(q - r)
            if (q > 0.0_sp) p = -p
            q =  abs(q)
            r = e
            e = d

            !  is parabola acceptable

            if ((abs(p) >= abs(half*q*r)) .or. (p <= q*(a - x)) .or. (p >= q*(b - x))) then

                !  a golden-section step

                if (x >= xm) then
                    e = a - x
                else
                    e = b - x
                end if
                d = c*e

            else

                !  a parabolic interpolation step

                d = p/q
                u = x + d

                !  f must not be evaluated too close to ax or bx

                if (((u - a) < tol2) .or. ((b - u) < tol2)) d = sign(tol1, xm - x)

            end if

        end if

        !  f must not be evaluated too close to x

        if (abs(d) >= tol1) then
            u = x + d
        else
            u = x + sign(tol1, d)
        end if
        fu = f(u)

        !  update a, b, v, w, and x

        if (fu <= fx) then
            if (u >= x) a = x
            if (u < x) b = x
            v = w
            fv = fw
            w = x
            fw = fx
            x = u
            fx = fu
        else
            if (u < x) a = u
            if (u >= x) b = u
            if (fu <= fw .or. w == x) then
                v = w
                fv = fw
                w = u
                fw = fu
            else if (fu <= fv .or. v == x .or. v == w ) then
                v = u
                fv = fu
            end if
        end if

    end do    !  end of main loop

    xmin = x

  end function fmin_r4


   function fmin_r8(f,ax,bx,tol) result(xmin)
      !dir$ attributes forceinline :: fmin_r8
      !dir$ attributes code_align : 32 :: fmin_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: fmin_r8
    implicit none

    procedure(func_r8)     :: f    !! the function to minimize
    real(dp),intent(in)    :: ax   !! left endpoint of initial interval
    real(dp),intent(in)    :: bx   !! right endpoint of initial interval
    real(dp),intent(in)    :: tol  !! desired length of the interval of
                                !! uncertainty of the final result (>=0)
    real(dp)                :: xmin !! abcissa approximating the point where
                                !! f attains a minimum

    real(dp) :: a,b,d,e,xm,p,q,r,tol1,tol2,u,v,w
    real(dp) :: fu,fv,fw,fx,x
    real(dp) :: abs,sqrt,sign

    real(dp),parameter :: c = (3.0_dp-sqrt(5.0_dp))/2.0_dp  !! squared inverse of golden ratio
    real(dp),parameter :: half = 0.5_dp
    real(dp),parameter :: sqrteps = sqrt(epsilon(1.0_dp))

    ! initialization

    a = ax
    b = bx
    v = a + c*(b - a)
    w = v
    x = v
    e = 0.0_dp
    fx = f(x)
    fv = fx
    fw = fx

    do    !  main loop starts here

        xm = half*(a + b)
        tol1 = sqrteps*abs(x) + tol*0.33333333333333333333333333333_dp
        tol2 = 2.0_dp*tol1

        !  check stopping criterion

        if (abs(x - xm) <= (tol2 - half*(b - a))) then
            ! write(*,*) 'x             = ', x
            ! write(*,*) 'xm            = ', xm
            ! write(*,*) 'abs(x - xm)   = ', abs(x - xm)
            ! write(*,*) 'tol2          = ', tol2
            ! write(*,*) 'half*(b - a)  = ', half*(b - a)
            exit
        end if

        ! is golden-section necessary

        if (abs(e) <= tol1) then

            !  a golden-section step

            if (x >= xm) then
                e = a - x
            else
                e = b - x
            end if
            d = c*e

        else

            !  fit parabola

            r = (x - w)*(fx - fv)
            q = (x - v)*(fx - fw)
            p = (x - v)*q - (x - w)*r
            q = 2.0_dp*(q - r)
            if (q > 0.0_dp) p = -p
            q =  abs(q)
            r = e
            e = d

            !  is parabola acceptable

            if ((abs(p) >= abs(half*q*r)) .or. (p <= q*(a - x)) .or. (p >= q*(b - x))) then

                !  a golden-section step

                if (x >= xm) then
                    e = a - x
                else
                    e = b - x
                end if
                d = c*e

            else

                !  a parabolic interpolation step

                d = p/q
                u = x + d

                !  f must not be evaluated too close to ax or bx

                if (((u - a) < tol2) .or. ((b - u) < tol2)) d = sign(tol1, xm - x)

            end if

        end if

        !  f must not be evaluated too close to x

        if (abs(d) >= tol1) then
            u = x + d
        else
            u = x + sign(tol1, d)
        end if
        fu = f(u)

        !  update a, b, v, w, and x

        if (fu <= fx) then
            if (u >= x) a = x
            if (u < x) b = x
            v = w
            fv = fw
            w = x
            fw = fx
            x = u
            fx = fu
        else
            if (u < x) a = u
            if (u >= x) b = u
            if (fu <= fw .or. w == x) then
                v = w
                fv = fw
                w = u
                fw = fu
            else if (fu <= fv .or. v == x .or. v == w ) then
                v = u
                fv = fu
            end if
        end if

    end do    !  end of main loop

    xmin = x

    end function fmin_r8
!*****************************************************************************************

!*****************************************************************************************
    end module fmin_module
!*****************************************************************************************
