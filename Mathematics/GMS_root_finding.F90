!*****************************************************************************************
!>
!  Root solver methods for:
!
!  * Bracked interval
!  * Without derivatives
!
!### Author
!  * Jacob Williams
!==========================================================================!
!### Modified by Bernard Gingold on 19/04/2022                             !
!    The 'Object-Oriented' design was completely removed in order          !
!    to fully exploit procedural design.                                   !
!==========================================================================!


    module root_finding

    use mod_kinds, only : i4, sp, dp

    implicit none

    public

  
    abstract interface
      
        function func_r4(x) result(f)
            !! Interface to the function to be minimized
            !! (Functional version).
            !! It should evaluate f(x) for any x in the interval (ax,bx)
            import :: sp
            implicit none
            real(sp),intent(in) :: x !! independant variable
            real(sp) :: f !! f(x)
        end function func_r4

        function func_r8(x) result(f)
            !! Interface to the function to be minimized
            !! (Functional version).
            !! It should evaluate f(x) for any x in the interval (ax,bx)
            import :: dp
            implicit none
            real(dp),intent(in) :: x !! independant variable
            real(dp) :: f !! f(x)
        end function func_r4  
     
    end interface


     real(sp),    parameter, private :: ftol4 = 0.0_sp       !! absolute tolerance for `f=0`
     real(sp),    parameter, private :: rtol4 = 1.0e-6_sp    !! relative tol for x
     real(sp),    parameter, private :: atol4 = 1.0e-12_sp   !! absolute tol for x [not used by all methods]
     real(dp),    parameter, private :: ftol8 = 0.0_dp       !! absolute tolerance for `f=0`
     real(dp),    parameter, private :: rtol8 = 1.0e-6_dp    !! relative tol for x
     real(dp),    parameter, private :: atol8 = 1.0e-12_dp   !! absolute tol for x [not used by all methods]
     integer(i4), parameter, private :: maxiter = 2000      !! maximum number of iterations [not used by all methods]
     
    
    contains





   

       
              

!*****************************************************************************************
!>
!  Find a zero of the function \( f(x) \) in the given interval
!  \( [a_x,b_x] \) to within a tolerance \( 4 \epsilon |x| + tol \),
!  where \( \epsilon \) is the relative machine precision defined as
!  the smallest representable number such that \( 1.0 + \epsilon > 1.0 \).
!
!### References
!  * R. P. Brent, "[An algorithm with guaranteed convergence for
!    finding a zero of a function](http://maths-people.anu.edu.au/~brent/pd/rpb005.pdf)",
!    The Computer Journal, Vol 14, No. 4., 1971.
!  * R. P. Brent, "[Algorithms for minimization without derivatives](http://maths-people.anu.edu.au/~brent/pub/pub011.html)",
!    Prentice-Hall, Inc., 1973.
!
!### See also
!  * [zeroin.f](http://www.netlib.org/go/zeroin.f) from Netlib

   subroutine brent_r4(fx,me,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: brent_r4
      !dir$ attributes code_align : 32 :: brent_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: brent_r4
    implicit none

    procedure(func_r4)     :: fx
    real(sp),intent(in)    :: ax    !! left endpoint of initial interval
    real(sp),intent(in)    :: bx    !! right endpoint of initial interval
    real(sp),intent(in)    :: fax   !! `f(ax)`
    real(sp),intent(in)    :: fbx   !! `f(ax)`
    real(sp),intent(out)   :: xzero !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out)   :: fzero !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag !! status flag (`0`=root found)

    real(sp),parameter :: eps = epsilon(1.0_sp)  !! d1mach(4) in original code
    real(sp) :: a,b,c,d,e,fa,fb,fc,tol1,xm,p,q,r,s
    integer :: i !! iteration counter

    ! initialize:
    iflag = 0
    tol1  = eps+1.0_sp
    a     = ax
    b     = bx
    fa    = fax
    fb    = fbx
    c     = a
    fc    = fa
    d     = b-a
    e     = d

    do i=1,maxiter

        if (abs(fc)<abs(fb)) then
            a=b
            b=c
            c=a
            fa=fb
            fb=fc
            fc=fa
        end if

        tol1 = 2.0_sp*eps*abs(b)+0.5_sp*rtol4
        xm = 0.5_sp*(c-b)
        if (abs(xm)<=tol1) exit

        ! see if a bisection is forced
        if ((abs(e)>=tol1) .and. (abs(fa)>abs(fb))) then
            s=fb/fa
            if (a/=c) then
                ! inverse quadratic interpolation
                q=fa/fc
                r=fb/fc
                p=s*(2.0_sp*xm*q*(q-r)-(b-a)*(r-1.0_sp))
                q=(q-1.0_sp)*(r-1.0_sp)*(s-1.0_sp)
            else
                ! linear interpolation
                p=2.0_sp*xm*s
                q=1.0_sp-s
            end if
            if (p<=0.0_sp) then
                p=-p
            else
                q=-q
            end if
            s=e
            e=d
            if (((2.0_sp*p)>=(3.0_sp*xm*q-abs(tol1*q))) .or. (p>=abs(0.5_sp*s*q))) then
                d=xm
                e=d
            else
                d=p/q
            end if
        else
            d=xm
            e=d
        end if

        a=b
        fa=fb
        if (abs(d)<=tol1) then
            if (xm<=0.0_sp) then
                b=b-tol1
            else
                b=b+tol1
            end if
        else
            b=b+d
        end if
        fb=fx(b)
        if (abs(fb)<=ftol4) exit  ! absolute convergence in f
        if ((fb*(fc/abs(fc)))>0.0_sp) then
            c=a
            fc=fa
            d=b-a
            e=d
        end if

        if (i==maxiter) iflag = -2  ! max iterations reached

    end do

    xzero = b
    fzero = fb

  end subroutine brent_r4


    subroutine brent_r8(fx,me,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: brent_r8
      !dir$ attributes code_align : 32 :: brent_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: brent_r8
    implicit none

    procedure(func_r8)     :: fx
    real(dp),intent(in)    :: ax    !! left endpoint of initial interval
    real(dp),intent(in)    :: bx    !! right endpoint of initial interval
    real(dp),intent(in)    :: fax   !! `f(ax)`
    real(dp),intent(in)    :: fbx   !! `f(ax)`
    real(dp),intent(out)   :: xzero !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out)   :: fzero !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag !! status flag (`0`=root found)

    real(dp),parameter :: eps = epsilon(1.0_dp)  !! d1mach(4) in original code
    real(dp) :: a,b,c,d,e,fa,fb,fc,tol1,xm,p,q,r,s
    integer :: i !! iteration counter

    ! initialize:
    iflag = 0
    tol1  = eps+1.0_dp
    a     = ax
    b     = bx
    fa    = fax
    fb    = fbx
    c     = a
    fc    = fa
    d     = b-a
    e     = d

    do i=1,maxiter

        if (abs(fc)<abs(fb)) then
            a=b
            b=c
            c=a
            fa=fb
            fb=fc
            fc=fa
        end if

        tol1 = 2.0_dp*eps*abs(b)+0.5_dp*rtol8
        xm = 0.5_dp*(c-b)
        if (abs(xm)<=tol1) exit

        ! see if a bisection is forced
        if ((abs(e)>=tol1) .and. (abs(fa)>abs(fb))) then
            s=fb/fa
            if (a/=c) then
                ! inverse quadratic interpolation
                q=fa/fc
                r=fb/fc
                p=s*(2.0_dp*xm*q*(q-r)-(b-a)*(r-1.0_dp))
                q=(q-1.0_dp)*(r-1.0_dp)*(s-1.0_dp)
            else
                ! linear interpolation
                p=2.0_dp*xm*s
                q=1.0_dp-s
            end if
            if (p<=0.0_dp) then
                p=-p
            else
                q=-q
            end if
            s=e
            e=d
            if (((2.0_dp*p)>=(3.0_dp*xm*q-abs(tol1*q))) .or. (p>=abs(0.5_dp*s*q))) then
                d=xm
                e=d
            else
                d=p/q
            end if
        else
            d=xm
            e=d
        end if

        a=b
        fa=fb
        if (abs(d)<=tol1) then
            if (xm<=0.0_dp) then
                b=b-tol1
            else
                b=b+tol1
            end if
        else
            b=b+d
        end if
        fb=fx(b)
        if (abs(fb)<=ftol8) exit  ! absolute convergence in f
        if ((fb*(fc/abs(fc)))>0.0_dp) then
            c=a
            fc=fa
            d=b-a
            e=d
        end if

        if (i==maxiter) iflag = -2  ! max iterations reached

    end do

    xzero = b
    fzero = fb

  end subroutine brent_r8


  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Compute the zero of the function f(x) in the interval ax,bx using the bisection method.
!
!### See also
!  * G.E. Mullges & F. Uhlig, "Numerical Algorithms with Fortran",
!    Springer, 1996. Section 2.8.1, p 32-34.

    subroutine bisection_r4(fx,me,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: bisection_r4
      !dir$ attributes code_align : 32 :: bisection_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: bisection_r4
    implicit none

    procedure(func_r4)     :: fx
    real(sp),intent(in)    :: ax      !! left endpoint of initial interval
    real(sp),intent(in)    :: bx      !! right endpoint of initial interval
    real(sp),intent(in)    :: fax     !! `f(ax)`
    real(sp),intent(in)    :: fbx     !! `f(ax)`
    real(sp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(sp) :: x1,x2,x3,f1,f2,f3
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        ! bisection of the inclusion interval:
        !  x1------x3------x2
        x3 = bisect_r4(x1,x2)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r4(x3,f3,ftol4,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_sp) then
            ! root lies between x2 and x3
            x1 = x3
            x2 = x2
            f1 = f3
            f2 = f2
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r4(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r4(x1,x2,f1,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine bisection_r4


   subroutine bisection_r8(fx,me,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: bisection_r8
      !dir$ attributes code_align : 32 :: bisection_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: bisection_r8
    implicit none

    procedure(func_r8)     :: fx
    real(dp),intent(in)    :: ax      !! left endpoint of initial interval
    real(dp),intent(in)    :: bx      !! right endpoint of initial interval
    real(dp),intent(in)    :: fax     !! `f(ax)`
    real(dp),intent(in)    :: fbx     !! `f(ax)`
    real(dp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(dp) :: x1,x2,x3,f1,f2,f3
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        ! bisection of the inclusion interval:
        !  x1------x3------x2
        x3 = bisect_r8(x1,x2)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r8(x3,f3,ftol8,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_dp) then
            ! root lies between x2 and x3
            x1 = x3
            x2 = x2
            f1 = f3
            f2 = f2
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r8(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r8(x1,x2,f1,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine bisection_r8

  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Compute the zero of the function f(x) in the interval ax,bx using the regula falsi method.

    subroutine regula_falsi_r4(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: regula_falsi_r4
      !dir$ attributes code_align : 32 :: regula_false_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: regula_falsi_r4
    implicit none

    procedure(func_r4)     :: fx
    real(sp),intent(in)    :: ax      !! left endpoint of initial interval
    real(sp),intent(in)    :: bx      !! right endpoint of initial interval
    real(sp),intent(in)    :: fax     !! `f(ax)`
    real(sp),intent(in)    :: fbx     !! `f(ax)`
    real(sp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(sp) :: x1,x2,x3,f1,f2,f3
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        x3 = regula_falsi_step_r4(x1,x2,f1,f2,ax,bx)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r4(x3,f3,ftol4,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_sp) then
            ! root lies between x2 and x3
            x1 = x3
            x2 = x2
            f1 = f3
            f2 = f2
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r4(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r4(x1,x2,f1,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine regula_falsi_r4

  subroutine regula_falsi_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: regula_falsi_r8
      !dir$ attributes code_align : 32 :: regula_false_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: regula_falsi_r8
    implicit none

    procedure(func_r8)     :: fx
    real(dp),intent(in)    :: ax      !! left endpoint of initial interval
    real(dp),intent(in)    :: bx      !! right endpoint of initial interval
    real(dp),intent(in)    :: fax     !! `f(ax)`
    real(dp),intent(in)    :: fbx     !! `f(ax)`
    real(dp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(dp) :: x1,x2,x3,f1,f2,f3
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        x3 = regula_falsi_step_r8(x1,x2,f1,f2,ax,bx)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r8(x3,f3,ftol8,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_dp) then
            ! root lies between x2 and x3
            x1 = x3
            x2 = x2
            f1 = f3
            f2 = f2
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r8(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r8(x1,x2,f1,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine regula_falsi_r8


  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Illinois method.
!
!### Reference
!  * M. Dowell, P. Jarratt, "A modified regula falsi method for computing the root
!    of an equation', BIT 11 (1971), 168-174.

    subroutine illinois_r4(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: illinois_r4
      !dir$ attributes code_align : 32 :: illinois_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: illinois_r4
    implicit none

    procedure(func_r4)     :: fx
    real(sp),intent(in)    :: ax      !! left endpoint of initial interval
    real(sp),intent(in)    :: bx      !! right endpoint of initial interval
    real(sp),intent(in)    :: fax     !! `f(ax)`
    real(sp),intent(in)    :: fbx     !! `f(ax)`
    real(sp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(sp) :: x1,x2,x3,f1,f2,f3,delta,f1tmp
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        x3 = regula_falsi_step_r4(x1,x2,f1,f2,ax,bx)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r4(x3,f3,ftol4,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_sp) then
            ! root lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f1tmp = f1
            f2 = f3
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
            f1tmp = f1 ! actual function eval
            f1 = 0.5_sp * f1
        end if

        ! check for convergence:
        root_found = converged_r4(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r4(x1,x2,f1tmp,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine illinois_r4


     subroutine illinois_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: illinois_r8
      !dir$ attributes code_align : 32 :: illinois_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: illinois_r8
    implicit none

    procedure(func_r8)     :: fx
    real(dp),intent(in)    :: ax      !! left endpoint of initial interval
    real(dp),intent(in)    :: bx      !! right endpoint of initial interval
    real(dp),intent(in)    :: fax     !! `f(ax)`
    real(dp),intent(in)    :: fbx     !! `f(ax)`
    real(dp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(dp) :: x1,x2,x3,f1,f2,f3,delta,f1tmp
    integer :: i !! iteration counter
    logical :: root_found !! convergence in x

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop
    do i=1,maxiter

        x3 = regula_falsi_step_r8(x1,x2,f1,f2,ax,bx)

        ! calculate the new function value:
        f3 = fx(x3)
        if (solution_r4(x3,f3,ftol8,xzero,fzero)) return

        ! determine new inclusion interval:
        if (f2*f3<0.0_dp) then
            ! root lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f1tmp = f1
            f2 = f3
        else
            ! root lies between x1 and x3
            x2 = x3
            f2 = f3
            f1tmp = f1 ! actual function eval
            f1 = 0.5_dp * f1
        end if

        ! check for convergence:
        root_found = converged_r8(x1,x2)
        if (root_found .or. i==maxiter) then
            call choose_best_r8(x1,x2,f1tmp,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine illinois_r8


  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Compute the zero of the function f(x) in the interval ax,bx using the Anderson-Bjorck method.
!
!### See also
!  * G.E. Mullges & F. Uhlig, "Numerical Algorithms with Fortran",
!    Springer, 1996. Section 2.8.2, p 36.

    subroutine anderson_bjorck_r4(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: anderson_bjorck_r4
      !dir$ attributes code_align : 32 :: anderson_bjorck_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: anderson_bjorck_r4
    implicit none

    procedure(func_r4)     :: fx
    real(sp),intent(in)    :: ax      !! left endpoint of initial interval
    real(sp),intent(in)    :: bx      !! right endpoint of initial interval
    real(sp),intent(in)    :: fax     !! `f(ax)`
    real(sp),intent(in)    :: fbx     !! `f(ax)`
    real(sp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer :: i !! counter
    logical :: root_found !! convergence in x
    real(sp) :: x1,x2,x3,f1,f2,f3,g,f1tmp

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop:
    do i = 1,maxiter

        x3 = secant_r4(x1,x2,f1,f2,ax,bx)
        f3 = fx(x3)
        if (solution_r4(x3,f3,ftol4,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<0.0_sp) then
            ! zero lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f2 = f3
            f1tmp = f1
        else
            ! zero lies between x1 and x3
            g = 1.0_sp - f3/f2
            if (g<=0.0_sp) g = 0.5_sp
            x2 = x3
            f1tmp = f1
            f1 = g*f1
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r4(x1,x2)
        if (root_found .or. i == maxiter) then
            call choose_best_r4(x1,x2,f1tmp,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

  end subroutine anderson_bjorck_r4

   subroutine anderson_bjorck_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: anderson_bjorck_r8
      !dir$ attributes code_align : 32 :: anderson_bjorck_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: anderson_bjorck_r8
    implicit none

    procedure(func_r8)     :: fx
    real(dp),intent(in)    :: ax      !! left endpoint of initial interval
    real(dp),intent(in)    :: bx      !! right endpoint of initial interval
    real(dp),intent(in)    :: fax     !! `f(ax)`
    real(dp),intent(in)    :: fbx     !! `f(ax)`
    real(dp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer :: i !! counter
    logical :: root_found !! convergence in x
    real(dp) :: x1,x2,x3,f1,f2,f3,g,f1tmp

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop:
    do i = 1,maxiter

        x3 = secant_r8(x1,x2,f1,f2,ax,bx)
        f3 = fx(x3)
        if (solution_r8(x3,f3,ftol8,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<0.0_dp) then
            ! zero lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f2 = f3
            f1tmp = f1
        else
            ! zero lies between x1 and x3
            g = 1.0_dp - f3/f2
            if (g<=0.0_dp) g = 0.5_dp
            x2 = x3
            f1tmp = f1
            f1 = g*f1
            f2 = f3
        end if

        ! check for convergence:
        root_found = converged_r8(x1,x2)
        if (root_found .or. i == maxiter) then
            call choose_best_r8(x1,x2,f1tmp,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

    end subroutine anderson_bjorck_r8
!*****************************************************************************************

!*****************************************************************************************
!>
!  Ridders method to find a root of f(x).
!
!### See also
!  * Ridders, C., "A new algorithm for computing a single root of a real continuous function",
!    IEEE Trans. on Circuits and Systems, Vol 26, Issue 11, Nov 1979.

    subroutine ridders_r4(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: ridders_r4
      !dir$ attributes code_align : 32 :: ridders_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: ridders_r4
    implicit none

    procedure(func_r4)   :: fx
    real(sp),intent(in)  :: ax      !! left endpoint of initial interval
    real(sp),intent(in)  :: bx      !! right endpoint of initial interval
    real(sp),intent(in)  :: fax     !! `f(ax)`
    real(sp),intent(in)  :: fbx     !! `f(ax)`
    real(sp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached, `-3`=singularity in the algorithm)

    integer  :: i !! counter
    real(sp) :: fh,fl,fm,fnew,denom,xh,xl,xm,xnew

    ! initialize:
    iflag = 0
    fl    = fax
    fh    = fbx
    xl    = ax
    xh    = bx
    xzero = huge(1.0_sp)

    do i = 1, maxiter

        xm = bisect_r4(xl,xh)
        fm = fx(xm)
        if (solution_r4(xm,fm,ftol4,xzero,fzero)) return

        denom = sqrt(fm**2-fl*fh)
        if (denom == 0.0_sp) then
            xzero = xm
            fzero = fm
            iflag = -3        ! can't proceed: denominator is zero [TODO: add a bisection if this happens]
            exit
        end if

        xnew = xm + (xm-xl)*(sign(1.0_sp,fl-fh)*fm/denom)
        if (converged_r4(xzero,xnew)) then  ! relative convergence in x
            ! additional check to prevent false convergence
            if (converged_r4(xl,xm) .or. converged_r4(xm,xh)) exit
        end if

        xzero = xnew
        fnew  = fx(xzero)
        fzero = fnew
        if (abs(fnew) <= ftol4) exit    ! abs convergence in f

        ! to keep the root bracketed:
        if (sign(fm,fnew) /= fm) then
            xl = xm
            fl = fm
            xh = xzero
            fh = fnew
        else if (sign(fl,fnew) /= fl) then
            xh = xzero
            fh = fnew
        else if (sign(fh,fnew) /= fh) then
            xl = xzero
            fl = fnew
        end if

        if (converged_r4(xl,xh)) exit    ! relative convergence in x
        if (i == maxiter) iflag = -2  ! max iterations exceeded

    end do

  end subroutine ridders_r4


    subroutine ridders_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: ridders_r8
      !dir$ attributes code_align : 32 :: ridders_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: ridders_r8
    implicit none

    procedure(func_r8)   :: fx
    real(dp),intent(in)  :: ax      !! left endpoint of initial interval
    real(dp),intent(in)  :: bx      !! right endpoint of initial interval
    real(dp),intent(in)  :: fax     !! `f(ax)`
    real(dp),intent(in)  :: fbx     !! `f(ax)`
    real(dp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached, `-3`=singularity in the algorithm)

    integer  :: i !! counter
    real(dp) :: fh,fl,fm,fnew,denom,xh,xl,xm,xnew

    ! initialize:
    iflag = 0
    fl    = fax
    fh    = fbx
    xl    = ax
    xh    = bx
    xzero = huge(1.0_dp)

    do i = 1, maxiter

        xm = bisect_r8(xl,xh)
        fm = fx(xm)
        if (solution_r8(xm,fm,ftol8,xzero,fzero)) return

        denom = sqrt(fm**2-fl*fh)
        if (denom == 0.0_dp) then
            xzero = xm
            fzero = fm
            iflag = -3        ! can't proceed: denominator is zero [TODO: add a bisection if this happens]
            exit
        end if

        xnew = xm + (xm-xl)*(sign(1.0_sp,fl-fh)*fm/denom)
        if (converged_r8(xzero,xnew)) then  ! relative convergence in x
            ! additional check to prevent false convergence
            if (converged_r8(xl,xm) .or. converged_r8(xm,xh)) exit
        end if

        xzero = xnew
        fnew  = fx(xzero)
        fzero = fnew
        if (abs(fnew) <= ftol8) exit    ! abs convergence in f

        ! to keep the root bracketed:
        if (sign(fm,fnew) /= fm) then
            xl = xm
            fl = fm
            xh = xzero
            fh = fnew
        else if (sign(fl,fnew) /= fl) then
            xh = xzero
            fh = fnew
        else if (sign(fh,fnew) /= fh) then
            xl = xzero
            fl = fnew
        end if

        if (converged_r8(xl,xh)) exit    ! relative convergence in x
        if (i == maxiter) iflag = -2  ! max iterations exceeded

    end do

    end subroutine ridders_r8
!*****************************************************************************************

!*****************************************************************************************
!>
!  Pegasus method to find a root of f(x).
!
!### See also
!  * G.E. Mullges & F. Uhlig, "Numerical Algorithms with Fortran",
!    Springer, 1996. Section 2.8.2, p 35.

    subroutine pegasus_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: pegasus_r4
      !dir$ attributes code_align : 32 :: pegasus_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: pegasus_r4 
    implicit none

    procedure(func_r4)   :: fx
    real(sp),intent(in)  :: ax      !! left endpoint of initial interval
    real(sp),intent(in)  :: bx      !! right endpoint of initial interval
    real(sp),intent(in)  :: fax     !! `f(ax)`
    real(sp),intent(in)  :: fbx     !! `f(ax)`
    real(sp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(sp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer :: i !! counter
    real(sp) :: x1,x2,x3,f1,f2,f3,f1tmp,denom

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop:
    do i = 1, maxiter

        ! secant step
        x3 = secant_r4(x1,x2,f1,f2,ax,bx)

        f3  = fx(x3)  ! calculate f3
        if (solution_r4(x3,f3,ftol4,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<=0.0_sp) then  ! root on (x2,x3)
            x1 = x2
            f1 = f2
            f1tmp = f1
        else  ! root on (x1,x3)
            f1tmp = f1
            denom = f2 + f3
            if (denom /= 0.0_sp) then
                ! proceed as normal
                f1 = f1 * f2 / denom
            else
                ! can't proceed, keep as is.
                ! [need a find a test case where this happens -TODO]
            end if
        end if

        x2 = x3
        f2 = f3

        call choose_best_r4(x1,x2,f1tmp,f2,xzero,fzero)

        if (converged_r4(x1,x2)) exit   ! check for convergence
        if (i == maxiter) iflag = -2 ! max iterations exceeded

    end do

  end subroutine pegasus_r4


    subroutine pegasus_r8(fx,ax,bx,fax,fbx,xzero,fzero,iflag)
      !dir$ attributes forceinline :: pegasus_r8
      !dir$ attributes code_align : 32 :: pegasus_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: pegasus_r8
    implicit none

    procedure(func_r8)   :: fx
    real(dp),intent(in)  :: ax      !! left endpoint of initial interval
    real(dp),intent(in)  :: bx      !! right endpoint of initial interval
    real(dp),intent(in)  :: fax     !! `f(ax)`
    real(dp),intent(in)  :: fbx     !! `f(ax)`
    real(dp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(dp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer :: i !! counter
    real(dp) :: x1,x2,x3,f1,f2,f3,f1tmp,denom

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop:
    do i = 1, maxiter

        ! secant step
        x3 = secant_r8(x1,x2,f1,f2,ax,bx)

        f3  = fx(x3)  ! calculate f3
        if (solution_r8(x3,f3,ftol4,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<=0.0_dp) then  ! root on (x2,x3)
            x1 = x2
            f1 = f2
            f1tmp = f1
        else  ! root on (x1,x3)
            f1tmp = f1
            denom = f2 + f3
            if (denom /= 0.0_dp) then
                ! proceed as normal
                f1 = f1 * f2 / denom
            else
                ! can't proceed, keep as is.
                ! [need a find a test case where this happens -TODO]
            end if
        end if

        x2 = x3
        f2 = f3

        call choose_best_r8(x1,x2,f1tmp,f2,xzero,fzero)

        if (converged_r8(x1,x2)) exit   ! check for convergence
        if (i == maxiter) iflag = -2 ! max iterations exceeded

    end do

  end subroutine pegasus_r8


  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Bisected Direct Quadratic Regula Falsi (BDQRF) root solver method
!  to find the root of a 1D function.
!
!### See also
!  * R. G. Gottlieb, B. F. Thompson, "Bisected Direct Quadratic Regula Falsi",
!    Applied Mathematical Sciences, Vol. 4, 2010, no. 15, 709-718.

    subroutine bdqrf(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(bdqrf_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: xdn,ydn,xup,yup,d,xm,ym,a,b,y2
    integer :: i !! counter

    ! initialize:
    iflag = 0
    xzero = ax
    fzero = fax
    y2    = fbx

    if (fzero<0.0_wp) then
        xdn = ax
        ydn = fzero
        xup = bx
        yup = y2
    else
        xup = ax
        yup = fzero
        xdn = bx
        ydn = y2
    end if

    ! main loop:
    do i = 1, me%maxiter

        xm = bisect(xup,xdn)
        ym = me%f(xm)
        if (abs(ym)<=me%ftol) then
            xzero = xm
            fzero = ym
            exit ! Convergence
        end if

        d = (xup - xdn) / 2.0_wp
        a = (yup + ydn - 2.0_wp*ym)/(2.0_wp*d**2)
        b = (yup - ydn)/(2.0_wp*d)

        xzero = xm - 2.0_wp*ym / (b * (1.0_wp + sqrt(1.0_wp - 4.0_wp*a*ym/b**2)))
        fzero = me%f(xzero)
        if (abs(fzero)<=me%ftol) exit ! Convergence

        if (fzero>0.0_wp) then
            yup = fzero
            xup = xzero
            if (ym<0.0_wp) then
                ydn = ym
                xdn = xm
            end if
        else
            ydn = fzero
            xdn = xzero
            if (ym>0.0_wp) then
                yup = ym
                xup = xm
            end if
        end if

        if (me%converged(xdn,xup) .or. i==me%maxiter) then
            call choose_best(xdn,xup,ydn,yup,xzero,fzero)
            if (i==me%maxiter) iflag = -2 ! maximum number of iterations
            exit
        end if

    end do

    end subroutine bdqrf
!*****************************************************************************************

!*****************************************************************************************
!>
!  Improved Muller method (for real roots only).
!  Will fall back to bisection if any step fails.
!
!### Reference
!  * D. E. Muller, "A Method for Solving Algebraic Equations Using an Automatic Computer",
!    Mathematical Tables and Other Aids to Computation, 10 (1956), 208-215.
!  * Regular Muller here (Julia version):
!    https://github.com/JuliaMath/Roots.jl/blob/97dbe2e178656e39b7f646cff278e4e985d60116/src/simple.jl

    subroutine muller (me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(muller_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: a,b,c,cx,fa,fb,fc,fcx,x,q,q2,q1,aa,bb,cc,delta,dp,dm,denon,bprev,fbprev
    integer  :: i    !! iteration counter
    logical  :: x_ok !! the new estimate `x` is ok to use

    iflag = 0

    ! pick a third point in the middle [this could also be an optional input]
    cx  = bisect(ax,bx)
    fcx = me%f(cx)
    if (solution(cx,fcx,me%ftol,xzero,fzero)) return

    ! [a,b,c]
    a = ax; fa = fax
    b = cx; fb = fcx
    c = bx; fc = fbx

    bprev = huge(1.0_wp)
    fbprev = huge(1.0_wp)

    do i = 1, me%maxiter

        ! muller step:
        q     = (c - b)/(b - a)
        q2    = q**2
        q1    = q + 1.0_wp
        aa    = q*fc - q*q1*fb + q2*fa
        bb    = (q1+q)*fc - q1**2*fb + q2*fa
        cc    = q1*fc
        delta = sqrt(max(0.0_wp, bb**2 - 4.0_wp * aa*cc)) ! to avoid complex roots
        dp    = bb + delta
        dm    = bb - delta
        if (abs(dp) > abs(dm)) then
            denon = dp
        else
            denon = dm
        end if
        x_ok = denon /= 0.0_wp
        if (x_ok) x = c - 2.0_wp*(c - b)*cc/denon

        ! make sure that x is ok, in the correct interval, and distinct.
        ! if not, fall back to bisection on that interval
        if (fa*fb < 0.0_wp) then  ! root in (a,b)
            if (.not. x_ok .or. x<=a .or. x>=b) x = bisect(a,b)
            c  = b
            fc = fb
            b  = x
        else  ! root in (b,c)
            if (.not. x_ok .or. x<=b .or. x>=c) x = bisect(b,c)
            a  = b
            fa = fb
            b  = x
        end if
        ! values are now [a,b,c], with b being the new estimate

        ! function evaluation for next estimate:
        fb = me%f(b)
        if (abs(fb)<=me%ftol) exit

        ! stopping criterion
        if (me%converged(a,c) .or. i == me%maxiter) then
            if ( i == me%maxiter ) iflag = -2 ! max iterations exceeded
            exit
        end if

        bprev = b
        fbprev = fb

    end do

    call choose_best(b,bprev,fb,fbprev,xzero,fzero)

    end subroutine muller
!*****************************************************************************************

!*****************************************************************************************
!>
!  Brent's method with hyperbolic extrapolation.
!
!  A variation on the classic Brent routine to find a zero of the function f
!  between the arguments ax and bx that uses hyperbolic extrapolation instead
!  of inverse quadratic extrapolation.
!
!### Reference
!  * SciPy `brenth.c`

    subroutine brenth(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(brenth_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: xpre,xcur,xblk,fpre,fcur,fblk,spre,&
                scur,sbis,delta,stry,dpre,dblk,xdelta
    integer :: i !! iteration counter

    iflag = 0
    xpre = ax
    xcur = bx
    fpre = fax
    fcur = fbx

    do i = 1, me%maxiter

        if (fpre*fcur < 0.0_wp) then
            xblk = xpre
            fblk = fpre
            scur = xcur - xpre
            spre = scur
        end if
        if (abs(fblk) < abs(fcur)) then
            xpre = xcur
            xcur = xblk
            xblk = xpre
            fpre = fcur
            fcur = fblk
            fblk = fpre
        end if

        delta = (me%atol + me%rtol*abs(xcur))/2.0_wp
        sbis = (xblk - xcur)/2.0_wp
        if (abs(fcur) <= me%ftol .or. abs(sbis) < delta) exit ! converged

        if (abs(spre) > delta .and. abs(fcur) < abs(fpre)) then
            if (xpre == xblk) then
                ! interpolate
                stry = -fcur*(xcur - xpre)/(fcur - fpre)
            else
                ! extrapolate
                dpre = (fpre - fcur)/(xpre - xcur)
                dblk = (fblk - fcur)/(xblk - xcur)
                stry = -fcur*(fblk - fpre)/(fblk*dpre - fpre*dblk)  ! only difference from brentq
            end if

            if (2.0_wp*abs(stry) < min(abs(spre), 3.0_wp*abs(sbis) - delta)) then
                ! accept step
                spre = scur
                scur = stry
            else
                ! bisect
                spre = sbis
                scur = sbis
            end if
        else
            ! bisect
            spre = sbis
            scur = sbis
        end if

        xpre = xcur
        fpre = fcur
        if (abs(scur) > delta) then
            xcur = xcur + scur
        else
            if (sbis > 0.0_wp) then
                xdelta = delta
            else
                xdelta = -delta
            end if
            xcur = xcur + xdelta
        end if

        fcur = me%f(xcur)
        if (abs(fcur) <= me%ftol) exit ! converged
        if (i == me%maxiter) iflag = -2 ! max iterations reached

    end do

    xzero = xcur
    fzero = fcur

    end subroutine brenth
!*****************************************************************************************

!*****************************************************************************************
!>
!  Classic Brent's method to find a zero of the function f on the sign
!  changing interval [ax, bx], but with a different formula for the extrapolation step.
!
!### Reference
!  * SciPy brentq.c

    subroutine brentq(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(brentq_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: xpre,xcur,xblk,fpre,fcur,fblk,spre,&
                scur,sbis,delta,stry,dpre,dblk,xdelta
    integer :: i !! iteration counter

    iflag = 0
    xpre = ax
    xcur = bx
    fpre = fax
    fcur = fbx

    do i = 1, me%maxiter

        if (fpre*fcur < 0.0_wp) then
            xblk = xpre
            fblk = fpre
            scur = xcur - xpre
            spre = scur
        end if
        if (abs(fblk) < abs(fcur)) then
            xpre = xcur
            xcur = xblk
            xblk = xpre
            fpre = fcur
            fcur = fblk
            fblk = fpre
        end if

        delta = (me%atol + me%rtol*abs(xcur))/2.0_wp
        sbis = (xblk - xcur)/2.0_wp
        if (abs(fcur) <= me%ftol .or. abs(sbis) < delta) exit ! converged

        if (abs(spre) > delta .and. abs(fcur) < abs(fpre)) then
            if (xpre == xblk) then
                ! interpolate
                stry = -fcur*(xcur - xpre)/(fcur - fpre)
            else
                ! extrapolate
                dpre = (fpre - fcur)/(xpre - xcur)
                dblk = (fblk - fcur)/(xblk - xcur)
                stry = -fcur*(fblk*dblk - fpre*dpre)/(dblk*dpre*(fblk - fpre))  ! only difference from brenth
            end if

            if (2.0_wp*abs(stry) < min(abs(spre), 3.0_wp*abs(sbis) - delta)) then
                ! accept step
                spre = scur
                scur = stry
            else
                ! bisect
                spre = sbis
                scur = sbis
            end if
        else
            ! bisect
            spre = sbis
            scur = sbis
        end if

        xpre = xcur
        fpre = fcur
        if (abs(scur) > delta) then
            xcur = xcur + scur
        else
            if (sbis > 0.0_wp) then
                xdelta = delta
            else
                xdelta = -delta
            end if
            xcur = xcur + xdelta
        end if

        fcur = me%f(xcur)
        if (abs(fcur) <= me%ftol) exit ! converged
        if (i == me%maxiter) iflag = -2 ! max iterations reached

    end do

    xzero = xcur
    fzero = fcur

    end subroutine brentq
!*****************************************************************************************

!*****************************************************************************************
!>
!  Chandrupatla's method.
!
!### Reference
!  * T.R. Chandrupatla, "A new hybrid quadratic/bisection algorithm for
!    finding the zero of a nonlinear function without derivatives," Advances in
!    Engineering Software, Vol 28, 1997, pp. 145-149.
!  * P. Scherer, "Computational Physics: Simulation of Classical and Quantum Systems",
!    Section 6.1.7.3. [this routine was coded from that description]
!  * Python version: https://www.embeddedrelated.com/showarticle/855.php

    subroutine chandrupatla(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(chandrupatla_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: a,b,c,fa,fb,fc,t,xt,ft,tol,tl,xi,phi,xm,fm
    integer :: i !! iteration counter

    ! initialization:
    iflag = 0
    b  = ax
    a  = bx
    c  = bx
    fa = fbx
    fb = fax
    fc = fb
    t  = 0.5_wp

    ! main loop:
    do i = 1, me%maxiter

        xt = a + t*(b-a)
        ft = me%f(xt)
        if (solution(xt,ft,me%ftol,xzero,fzero)) return

        if (ft*fa>0.0_wp) then
            c = a
            fc = fa
        else
            c = b
            b = a
            fc = fb
            fb = fa
        end if
        a = xt
        fa = ft

        if (abs(fb) < abs(fa)) then
            xm = b
            fm = fb
        else
            xm = a
            fm = fa
        end if

        if (i == me%maxiter) then
            iflag = -2 ! max iterations reached
            exit
        end if

        tol = 2.0_wp*me%rtol*abs(xm) + me%atol
        tl = tol/abs(b-c)
        if (tl > 0.5_wp) exit
        t = 0.5_wp ! use bisection unless we can use inverse quadratic below

        if (fa/=fb .and. fb/=fc) then
            xi  = (a-b)/(c-b)
            phi = (fa-fb)/(fc-fb)
            if (1.0_wp - sqrt(1.0_wp - xi) < phi .and. phi < sqrt(xi)) then
                ! inverse quadratic interpolation
                t = (fa/(fb-fa)) * (fc/(fb-fc)) + ((c-a)/(b-a)) * (fa/(fc-fa)) * (fb/(fc-fb))
            end if
        end if

        t = min(1.0_wp-tl, max(tl, t))

    end do

    xzero = xm
    fzero = fm

    end subroutine chandrupatla
!*****************************************************************************************

!*****************************************************************************************
!>
!  TOMS748 rootfinding method.
!
!  Finds either an exact solution or an approximate solution of the
!  equation `f(x)=0` in the interval [ax,bx]. At the begining of each
!  iteration, the current enclosing interval is recorded as [a0,b0].
!  The first iteration is simply a secant step. Starting with the
!  second iteration, three steps are taken in each iteration. First
!  two steps are either quadratic interpolation or cubic inverse
!  interpolation. The third step is a double-size secant step. If the
!  diameter of the enclosing interval obtained after those three steps
!  is larger than 0.5*(b0-a0), then an additional bisection step will
!  be taken.
!
!### References
!  * http://www.netlib.org/toms/748
!  * G. E. Alefeld, F. A. Potra and Yixun Shi,
!    "Algorithm 748: Enclosing Zeros of Continuous Functions",
!    ACM Transactions on Mathematical Software,
!    Vol. 21. No. 3. September 1995. Pages 327-344.

    subroutine toms748(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(toms748_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer  :: itnum
    real(wp) :: a,b,fa,fb,c,u,fu,a0,b0,tol,d,fd
    real(wp) :: prof,e,fe,tmpc

    a = ax
    b = bx
    fa = fax
    fb = fbx

    ! initialization. set the number of iteration as 0.
    ! set dumb values for the variables "e" and "fe".
    e  = huge(1.0_wp)
    fe = huge(1.0_wp)

    ! iteration starts. the enclosing interval before executing the
    ! iteration is recorded as [a0, b0].
    do itnum = 1, me%maxiter

        a0 = a
        b0 = b

        ! calculates the termination criterion. stops the procedure if the
        ! criterion is satisfied.
        if (abs(fb) <= abs(fa)) then
            tol = get_tolerance(b)
        else
            tol = get_tolerance(a)
        end if
        if ((b-a)<=tol) exit

        ! for the first iteration, secant step is taken.
        if (itnum == 1) then

            c=a-(fa/(fb-fa))*(b-a)

            ! call subroutine "bracket" to get a shrinked enclosing interval as
            ! well as to update the termination criterion. stop the procedure
            ! if the criterion is satisfied or the exact solution is obtained.
            call bracket(a,b,c,fa,fb,tol,d,fd)
            if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

            cycle

        end if

        ! starting with the second iteration, in the first two steps, either
        ! quadratic interpolation is used by calling the subroutine "newqua"
        ! or the cubic inverse interpolation is used by calling the subroutine
        ! "pzero". in the following, if "prof" is not equal to 0, then the
        ! four function values "fa", "fb", "fd", and "fe" are distinct, and
        ! hence "pzero" will be called.
        prof=(fa-fb)*(fa-fd)*(fa-fe)*(fb-fd)*(fb-fe)*(fd-fe)
        if ((itnum == 2) .or. (prof == 0.0_wp)) then
            call newqua(a,b,d,fa,fb,fd,c,2)
        else
            c = pzero(a,b,d,e,fa,fb,fd,fe)
            if ((c-a)*(c-b) >= 0.0_wp) then
                call newqua(a,b,d,fa,fb,fd,c,2)
            end if
        end if
        e=d
        fe=fd

        ! call subroutine "bracket" to get a shrinked enclosing interval as
        ! well as to update the termination criterion. stop the procedure
        ! if the criterion is satisfied or the exact solution is obtained.
        call bracket(a,b,c,fa,fb,tol,d,fd)
        if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit
        prof=(fa-fb)*(fa-fd)*(fa-fe)*(fb-fd)*(fb-fe)*(fd-fe)
        if (prof == 0.0_wp) then
            call newqua(a,b,d,fa,fb,fd,c,3)
        else
            c = pzero(a,b,d,e,fa,fb,fd,fe)
            if ((c-a)*(c-b) >= 0.0_wp) then
                call newqua(a,b,d,fa,fb,fd,c,3)
            end if
        end if

        ! call subroutine "bracket" to get a shrinked enclosing interval as
        ! well as to update the termination criterion. stop the procedure
        ! if the criterion is satisfied or the exact solution is obtained.
        call bracket(a,b,c,fa,fb,tol,d,fd)
        if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit
        e=d
        fe=fd

        ! takes the double-size secant step.
        if (abs(fa) < abs(fb)) then
            u=a
            fu=fa
        else
            u=b
            fu=fb
        end if
        c=u-2.0_wp*(fu/(fb-fa))*(b-a)
        if (abs(c-u) > (0.5_wp*(b-a))) then
            c=a+0.5_wp*(b-a)
        end if

        ! call subroutine bracket to get a shrinked enclosing interval as
        ! well as to update the termination criterion. stop the procedure
        ! if the criterion is satisfied or the exact solution is obtained.
        call bracket(a,b,c,fa,fb,tol,d,fd)
        if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

        ! determines whether an additional bisection step is needed. and takes
        ! it if necessary.
        if ((b-a) < (0.5_wp*(b0-a0))) cycle
        e=d
        fe=fd

        ! call subroutine "bracket" to get a shrinked enclosing interval as
        ! well as to update the termination criterion. stop the procedure
        ! if the criterion is satisfied or the exact solution is obtained.
        tmpc = a+0.5_wp*(b-a)
        call bracket(a,b,tmpc,fa,fb,tol,d,fd)
        if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

        if (itnum == me%maxiter) iflag = -2    ! maximum iterations reached

    end do

    !return result:
    xzero = a
    fzero = fa

    contains
!**********************************************************************************

    !************************************************************************
    subroutine bracket(a,b,c,fa,fb,tol,d,fd)

    !!  Given current enclosing interval [a,b] and a number c in (a,b), if
    !!  f(c)=0 then sets the output a=c. Otherwise determines the new
    !!  enclosing interval: [a,b]=[a,c] or [a,b]=[c,b]. Also updates the
    !!  termination criterion corresponding to the new enclosing interval.

    implicit none

    real(wp),intent(inout)  :: a    !! input as the current left point of the
                                    !! enclosing interval and output as the shrinked
                                    !! new enclosing interval
    real(wp),intent(inout)  :: b    !! input as the current right point of the
                                    !! enclosing interval and output as the shrinked
                                    !! new enclosing interval
    real(wp),intent(inout)  :: c    !! used to determine the new enclosing interval
    real(wp),intent(inout)  :: fa   !! f(a)
    real(wp),intent(inout)  :: fb   !! f(b)
    real(wp),intent(inout)  :: tol  !! input as the current termination
                                    !! criterion and output as the updated termination
                                    !! criterion according to the new enclosing interval
    real(wp),intent(out)    :: d    !! if the new enclosing interval
                                    !! is [a,c] then d=b, otherwise d=a;
    real(wp),intent(out)    :: fd   !! f(d)

    real(wp) :: fc

    ! adjust c if (b-a) is very small or if c is very close to a or b.
    tol = 0.7_wp*tol
    if ((b-a) <= 2.0_wp*tol) then
        c = a+0.5_wp*(b-a)
    else if (c <= a+tol) then
        c = a+tol
    else
        if (c >= b-tol) c = b-tol
    end if

    ! call subroutine to obtain f(c)
    fc = me%f(c)

    ! if c is a root, then set a=c and return. this will terminate the
    ! procedure in the calling routine.
    if (abs(fc) <= me%ftol) then

        a   = c
        fa  = fc
        d   = 0.0_wp
        fd  = 0.0_wp

    else

        ! if c is not a root, then determine the new enclosing interval.
        if ((isign(fa)*isign(fc)) < 0) then
            d   = b
            fd  = fb
            b   = c
            fb  = fc
        else
            d   = a
            fd  = fa
            a   = c
            fa  = fc
        end if

        ! update the termination criterion according to the new enclosing interval.
        if (abs(fb) <= abs(fa)) then
            tol = get_tolerance(b)
        else
            tol = get_tolerance(a)
        end if

    end if

    end subroutine bracket
    !************************************************************************

    !************************************************************************
    pure function isign(x) result(i)

    !! sign of the variable `x` (note: return `0` if `x=0`)

    implicit none

    integer :: i
    real(wp),intent(in) :: x

    if (x > 0.0_wp) then
        i = 1
    else if (x == 0.0_wp) then
        i = 0
    else
        i = -1
    end if

    end function isign
    !************************************************************************

    !************************************************************************
    pure function get_tolerance(b) result(tol)

    !! determines the termination criterion.

    implicit none

    real(wp),intent(in) :: b
    real(wp) :: tol  !! termination criterion: 2*(2*rtol*|b| + atol)

    tol = 2.0_wp * (me%atol + 2.0_wp*abs(b)*me%rtol)

    end function get_tolerance
    !************************************************************************

    !************************************************************************
    pure subroutine newqua(a,b,d,fa,fb,fd,c,k)

    !! uses k newton steps to approximate the zero in (a,b) of the
    !! quadratic polynomial interpolating f(x) at a, b, and d.
    !! safeguard is used to avoid overflow.

    implicit none

    real(wp),intent(in)  :: a
    real(wp),intent(in)  :: b
    real(wp),intent(in)  :: d  !! d lies outside the interval [a,b]
    real(wp),intent(in)  :: fa !! f(a), f(a)f(b)<0
    real(wp),intent(in)  :: fb !! f(b), f(a)f(b)<0
    real(wp),intent(in)  :: fd !! f(d)
    real(wp),intent(out) :: c  !! the approximate zero
                               !! in (a,b) of the quadratic polynomial.
    integer,intent(in)   :: k  !! number of newton steps to take.

    integer  :: ierror,i
    real(wp) :: a0,a1,a2,pc,pdc

    ! initialization
    ! find the coefficients of the quadratic polynomial
    ierror = 0
    a0 = fa
    a1 = (fb-fa)/(b-a)
    a2 = ((fd-fb)/(d-b)-a1)/(d-a)

    do    ! main loop

        ! safeguard to avoid overflow
        if ((a2 == 0.0_wp) .or. (ierror == 1)) then
            c=a-a0/a1
            return
        end if

        ! determine the starting point of newton steps
        if (isign(a2)*isign(fa) > 0) then
            c=a
        else
            c=b
        end if

        ! start the safeguarded newton steps
        do i=1,k
            if (ierror == 0) then
                pc=a0+(a1+a2*(c-b))*(c-a)
                pdc=a1+a2*((2.0_wp*c)-(a+b))
                if (pdc == 0.0_wp) then
                    ierror=1
                else
                    c=c-pc/pdc
                end if
            end if
        end do
        if (ierror/=1) exit

    end do

    end subroutine newqua
    !************************************************************************

    !************************************************************************
    pure function pzero(a,b,d,e,fa,fb,fd,fe) result(c)

    !! uses cubic inverse interpolation of f(x) at a, b, d, and e to
    !! get an approximate root of f(x). this procedure is a slight
    !! modification of aitken-neville algorithm for interpolation
    !! described by stoer and bulirsch in "Intro. to numerical analysis"
    !! springer-verlag. new york (1980).

    implicit none

    real(wp),intent(in) :: a,b,d,e,fa,fb,fd,fe
    real(wp) :: c

    real(wp) :: q11,q21,q31,d21,d31,q22,q32,d32,q33

    q11 = (d-e)*fd/(fe-fd)
    q21 = (b-d)*fb/(fd-fb)
    q31 = (a-b)*fa/(fb-fa)
    d21 = (b-d)*fd/(fd-fb)
    d31 = (a-b)*fb/(fb-fa)

    q22 = (d21-q11)*fb/(fe-fb)
    q32 = (d31-q21)*fa/(fd-fa)
    d32 = (d31-q21)*fd/(fd-fa)
    q33 = (d32-q22)*fa/(fe-fa)

    c = a + q31+q32+q33

    end function pzero
    !************************************************************************

    end subroutine toms748
!*****************************************************************************************

!*****************************************************************************************
!>
!  Zhang's method (with corrections from Stage).
!
!### Reference
!  * A. Zhang, "An Improvement to the Brent's Method",
!    International Journal of Experimental Algorithms (IJEA), Volume (2) : Issue (1) : 2011.
!    https://www.cscjournals.org/download/issuearchive/IJEA/Volume2/IJEA_V2_I1.pdf
!  * S. A. Stage, "Comments on An Improvement to the Brent's Method",
!    International Journal of Experimental Algorithms (IJEA), Volume (4) : Issue (1) : 2013.
!    https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.740.923&rep=rep1&type=pdf

    subroutine zhang(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(zhang_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: a,b,c,fa,fb,fc,s,fs
    integer :: i !! iteration counter

    iflag = 0
    a  = ax
    b  = bx
    fa = fax
    fb = fbx

    do i = 1, me%maxiter

        c = bisect(a,b)
        fc = me%f(c)
        if (solution(c,fc,me%ftol,xzero,fzero)) return

        if (fa/=fc .and. fb/=fc) then
            ! inverse quadratic interpolation
            s = a*fb*fc/((fa-fb)*(fa-fc)) + &
                b*fa*fc/((fb-fa)*(fb-fc)) + &
                c*fa*fb/((fc-fa)*(fc-fb))
            if (a<s .and. s<b) then
                fs = me%f(s)
                if (abs(fs)<=me%ftol) then
                    xzero = s
                    fzero = fs
                    return
                end if
            else
                ! s is not in (a,b)
                s = c ! just use this (there are 3 options in the reference)
                fs = fc
            end if
        else
            ! secant
            if (fa*fc<0.0_wp) then ! root in [a,c]
                s = secant(a,c,fa,fc,ax,bx)
            else ! root in [c,b]
                s = secant(c,b,fc,fb,ax,bx)
            end if
            fs = me%f(s)
            if (solution(s,fs,me%ftol,xzero,fzero)) return
        end if

        if (c>s) then
            ! ensures a <= c <= s <= b
            call swap(s,c)
            call swap(fs,fc)
        end if

        if (fc*fs<0.0_wp) then       ! root on [c,s]
            a = c
            b = s
            fa = fc
            fb = fs
        else if (fa*fc<0.0_wp) then  ! root on [a,c]
            b = c
            fb = fc
        else                         ! root on [s,b]
            a = s
            fa = fs
        end if

        if (me%converged(a,b)) exit
        if (i == me%maxiter) iflag = -2 ! max iterations reached

    end do

    ! pick the one closest to the root:
    call choose_best(a,b,fa,fb,xzero,fzero)

    end subroutine zhang
!*****************************************************************************************

!*****************************************************************************************
!>
!  Modified anderson-bjorck-king method. Same as [[anderson_bjorck]], but with
!  an extra initial bisection step.
!
!### See also
!  * Kroger & Torsten, "On-Line Trajectory Generation in Robotic Systems", 2010.
!    https://link.springer.com/content/pdf/bbm%3A978-3-642-05175-3%2F1.pdf

    subroutine anderson_bjorck_king(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(anderson_bjorck_king_solver),intent(inout) :: me
    real(wp),intent(in)    :: ax      !! left endpoint of initial interval
    real(wp),intent(in)    :: bx      !! right endpoint of initial interval
    real(wp),intent(in)    :: fax     !! `f(ax)`
    real(wp),intent(in)    :: fbx     !! `f(ax)`
    real(wp),intent(out)   :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out)   :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)    :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    integer :: i !! counter
    logical :: root_found !! convergence in x
    real(wp) :: x1,x2,x3,f1,f2,f3,g,f1tmp

    ! initialize:
    iflag = 0
    x1    = ax
    x2    = bx
    f1    = fax
    f2    = fbx

    ! main loop:
    do i = 1,me%maxiter

        ! bisection step:
        x3 = bisect(x1,x2)

        ! calculate f3:
        f3 = me%f(x3)
        if (solution(x3,f3,me%ftol,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<0.0_wp) then
            ! zero lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f2 = f3
        else
            ! zero lies between x1 and x3
            x2 = x3
            f2 = f3
        end if

        ! secant step:
        x3 = secant(x1,x2,f1,f2,ax,bx)

        ! calculate f3:
        f3 = me%f(x3)
        if (solution(x3,f3,me%ftol,xzero,fzero)) return

        ! determine a new inclusion interval:
        if (f2*f3<0.0_wp) then
            ! zero lies between x2 and x3
            x1 = x2
            x2 = x3
            f1 = f2
            f2 = f3
            f1tmp = f1
        else
            ! zero lies between x1 and x3
            g = 1.0_wp-f3/f2
            if (g<=0.0_wp) g = 0.5_wp
            x2 = x3
            f1tmp = f1
            f1 = g*f1
            f2 = f3
        end if

        ! check for convergence:
        root_found = me%converged(x1,x2)
        if (root_found .or. i == me%maxiter) then
            call choose_best(x1,x2,f1tmp,f2,xzero,fzero)
            if (.not. root_found) iflag = -2  ! max iterations reached
            exit
        end if

    end do

    end subroutine anderson_bjorck_king
!*****************************************************************************************

!*****************************************************************************************
!>
!  BlendTF blended method of trisection and false position methods.
!
!### Reference
!  * E Badr, S Almotairi, A El Ghamry,
!    "A Comparative Study among New Hybrid Root Finding
!    Algorithms and Traditional Methods", Mathematics 2021, 9, 1306.

    subroutine blendtf(me,ax,bx,fax,fbx,xzero,fzero,iflag)

    implicit none

    class(blendtf_solver),intent(inout) :: me
    real(wp),intent(in)  :: ax      !! left endpoint of initial interval
    real(wp),intent(in)  :: bx      !! right endpoint of initial interval
    real(wp),intent(in)  :: fax     !! `f(ax)`
    real(wp),intent(in)  :: fbx     !! `f(ax)`
    real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
    real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
    integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

    real(wp) :: a1,a2,b1,b2,fa,fb,a,b,xt1,xt2,xf,x,fx,fxt2,&
                fxf,xprev,fxt1,fxprev,fa1,fa2,fb1,fb2
    integer :: i !! iteration counter

    iflag = 0
    a = ax; b = bx
    fa = fax; fb = fbx
    a1 = a; a2 = a
    b1 = b; b2 = b
    fa1 = fa; fa2 = fa
    fb1 = fb; fb2 = fb
    xprev = huge(1.0_wp)
    fxprev = huge(1.0_wp)

    do i = 1, me%maxiter

        if (fa == fb) then
            ! should fallback to bisection if this happens -TODO
            iflag = -3
            return
        end if

        xt1  = (b + 2.0_wp * a) / 3.0_wp
        xt2  = (2.0_wp * b + a) / 3.0_wp
        xf   = a - (fa*(b-a))/(fb-fa)
        x    = xt1
        fxt1 = me%f(xt1); if (solution(xt1,fxt1,me%ftol,xzero,fzero)) return
        fxt2 = me%f(xt2); if (solution(xt2,fxt2,me%ftol,xzero,fzero)) return
        fxf  = me%f(xf);  if (solution(xf,fxf,me%ftol,xzero,fzero))   return
        fx   = fxt1

        if (abs(fxf) < abs(fxt1)) then
            x = xf
            fx = fxf
        elseif (abs(fxt2) < abs(fxt1)) then
            x = xt2
            fx = fxt2
        end if

        ! apply the convergence tols to [a,b]
        if (me%converged(a, b) .or. i == me%maxiter) then
            call choose_best(x,xprev,fx,fxprev,xzero,fzero)
            if (i == me%maxiter) iflag = -2 ! max iterations reached
            exit
        end if
        xprev = x
        fxprev = fx

        if ((fa * fxt1) < 0.0_wp) then
            b1 = xt1; fb1 = fxt1
        else if ((fxt1 * fxt2) < 0.0_wp) then
            a1 = xt1; fa1 = fxt1
            b1 = xt2; fb1 = fxt2
        else
            a1 = xt2; fa1 = fxt2
        end if

        if (fa*fxf < 0.0_wp) then
            b2 = xf; fb2 = fxf
        else
            a2 = xf; fa2 = fxf
        end if

        if (a1>a2) then
            a = a1
            fa = fa1
        else
            a = a2
            fa = fa2
        end if
        if (b1<b2) then
            b = b1
            fb = fb1
        else
            b = b2
            fb = fb2
        end if

    end do

    end subroutine blendtf
!*****************************************************************************************

!*****************************************************************************************
!>
!  Barycentric interpolation method.
!
!### Reference
!  * Enrique Aguilar-Mendez, Roxana Mitzaye Del Castillo,
!    "A highly efficient numerical method to solve non-linear functions using barycentric interpolation"
!    January 2021, Applied Mathematical Sciences 15(7):321-336
!  * Python implemention here: https://github.com/EnriqueAguilarM/

    subroutine barycentric(me,ax,bx,fax,fbx,xzero,fzero,iflag)

        implicit none

        class(barycentric_solver),intent(inout) :: me
        real(wp),intent(in)  :: ax      !! left endpoint of initial interval
        real(wp),intent(in)  :: bx      !! right endpoint of initial interval
        real(wp),intent(in)  :: fax     !! `f(ax)`
        real(wp),intent(in)  :: fbx     !! `f(ax)`
        real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
        real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
        integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)

        integer :: i !! iteration counter
        real(wp) :: k !! a real number in the interval (0, 0.5)
        real(wp) :: x0,x1,x2,x3,x4,x5,x6,&
                    f0,f1,f2,f3,f4,f5,f6,&
                    w0,w1,w2,w3,w22,w32

        real(wp),parameter :: bisection_tol = 1.0e-17_wp  !! if `f0` or `f1` are below this value,
                                                          !! the method switches to bisection.
                                                          !! (should be an input or computed from `epsilon`?)
        real(wp),parameter :: k0 =  1.0_wp / 6.0_wp !! initial value of `k` (should be an input?)

        iflag = 0
        x0 = ax; f0 = fax
        x1 = bx; f1 = fbx
        k = k0

        do i = 1, me%maxiter

            if (i>1) then
                f0 = me%f(x0)
                if (solution(x0,f0,me%ftol,xzero,fzero)) return
                f1 = me%f(x1)
                if (solution(x1,f1,me%ftol,xzero,fzero)) return
            end if

            if (me%converged(x0, x1) .or. i == me%maxiter) then
                if (i == me%maxiter) iflag = -2 ! max iterations reached
                exit
            end if

            if (abs(f0)<bisection_tol .or. abs(f1)<bisection_tol) then
                ! if f is too small, switch to bisection
                k = 0.5_wp
            end if

            x2 = x0 + k*(x1-x0)   ! first point for interpolation
            f2 = me%f(x2)
            if (solution(x2,f2,me%ftol,xzero,fzero)) return

            if (f0*f2 < 0.0_wp) then
                x1 = x2
            else if (f0 == f2) then
                x0 = x2
            else
                x3 = x1 + k*(x0-x1) ! second point for interpolation
                f3 = me%f(x3)
                if (solution(x3,f3,me%ftol,xzero,fzero)) return

                if (f3*f1 < 0.0_wp) then
                    x0 = x3
                else if (f3 == f1) then
                    x0 = x2        ! note: typo in python version here?
                    x1 = x3
                else

                    w0 = 1.0_wp/(f0*(f0-f2)*(f0-f3))   ! first quadratic interpolation
                    w2 = 1.0_wp/(f2*(f2-f0)*(f2-f3))
                    w3 = 1.0_wp/(f3*(f3-f0)*(f3-f2))
                    x4 = (w0*x0+w2*x2+w3*x3)/(w0+w3+w2)
                    f4 = me%f(x4)
                    if (solution(x4,f4,me%ftol,xzero,fzero)) return

                    w1  = 1.0_wp/(f1*(f1-f2)*(f1-f3))  ! second quadratic interpolation
                    w22 = 1.0_wp/(f2*(f2-f1)*(f2-f3))
                    w32 = 1.0_wp/(f3*(f3-f1)*(f3-f2))
                    x5  = (w1*x1+w22*x2+w32*x3)/(w1+w32+w22)
                    f5  = me%f(x5)
                    if (solution(x5,f5,me%ftol,xzero,fzero)) return

                    ! determine if x3,x4 are in the interval [x2,x3]
                    if (x4<x2 .or. x4>x3) then
                        if (x5<x2 .or. x5>x3) then
                            x0 = x2
                            x1 = x3
                        else
                            if (f5*f2<0.0_wp) then
                                x0 = x2
                                x1 = x5
                            else
                                x0 = x5
                                x1 = x3
                            end if
                        end if
                    else
                        if (x5<x2 .or. x5>x3) then
                            if (f4*f2<0.0_wp) then
                                x0 = x2
                                x1 = x4
                            else
                                x0 = x4
                                x1 = x3
                            end if
                        else

                            if (f5*f4>0.0_wp) then
                                if (f4*f2<0.0_wp) then
                                    x0 = x2
                                    x1 = min(x4,x5)
                                else
                                    x0 = max(x4,x5)
                                    x1 = x3
                                end if
                            else
                                ! cubic interpolation if f4 and f5 have opposite signs
                                w0 = w0/(f0-f1)
                                w1 = w1/(f1-f0)
                                w2 = w2/(f2-f1)
                                w3 = w3/(f3-f1)
                                x6 = (w0*x0+w1*x1+w2*x2+w3*x3)/(w0+w1+w3+w2)
                                f6 = me%f(x6)
                                if (solution(x6,f6,me%ftol,xzero,fzero)) return

                                ! verify that x6 is in the interval [x2,x3]
                                if (x6<=x2 .or. x6>=x3) then
                                    x0 = min(x4,x5)
                                    x1 = max(x4,x5)
                                else
                                    if (f6*f4<0.0_wp) then
                                        x0 = min(x4,x6)
                                        x1 = max(x4,x6)
                                    else
                                        x0 = min(x5,x6)
                                        x1 = max(x5,x6)
                                    end if
                                end if
                            end if
                        end if
                    end if
                end if
            end if

        end do

        call choose_best(x0,x1,f0,f1,xzero,fzero)

    end subroutine barycentric
!*****************************************************************************************

!*****************************************************************************************
!>
!  Determines convergence in x based on if the reltol or abstol is satisfied.

    function converged_r4(a,b)
        !dir$ attributes forceinline :: converged_r4
      !dir$ attributes code_align : 32 :: converged_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: converged_r4
    implicit none

    
    real(sp),intent(in) :: a !! old value
    real(sp),intent(in) :: b !! new value
    logical :: converged

    real(sp) :: d

    ! original way:
    ! converged = (abs(b-a) <= abs(b)*me%rtol + me%atol) exit

    d = abs(b-a)

    if (d <= atol4) then
        ! absolute
        converged = .true.
    else
        ! relative
        if (a /= 0.0_sp) then
            converged = d / abs(a) <= rtol4
        else
            converged = .false.
        end if
    end if

  end function converged_r4


    function converged_r8(a,b)
        !dir$ attributes forceinline :: converged_r8
      !dir$ attributes code_align : 32 :: converged_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: converged_r8
    implicit none

    
    real(dp),intent(in) :: a !! old value
    real(dp),intent(in) :: b !! new value
    logical :: converged

    real(dp) :: d

    ! original way:
    ! converged = (abs(b-a) <= abs(b)*me%rtol + me%atol) exit

    d = abs(b-a)

    if (d <= atol8) then
        ! absolute
        converged = .true.
    else
        ! relative
        if (a /= 0.0_dp) then
            converged = d / abs(a) <= rtol8
        else
            converged = .false.
        end if
    end if

  end function converged_r8

  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Given two points with two function evaluations, choose the best one
!  (the one closest to the root).

    pure subroutine choose_best_r4(x1,x2,f1,f2,xbest,fbest)
      !dir$ attributes forceinline :: choose_best_r4
      !dir$ attributes code_align : 32 :: choose_best_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: choose_best_r4
    implicit none

    real(sp),intent(in) :: x1,x2
    real(sp),intent(in) :: f1,f2
    real(sp),intent(out) :: xbest
    real(sp),intent(out) :: fbest

    if (abs(f1)<abs(f2)) then
        xbest = x1
        fbest = f1
    else
        xbest = x2
        fbest = f2
    end if

  end subroutine choose_best_r4

  pure subroutine choose_best_r8(x1,x2,f1,f2,xbest,fbest)
      !dir$ attributes forceinline :: choose_best_r8
      !dir$ attributes code_align : 32 :: choose_best_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: choose_best_r8
    implicit none

    real(sd),intent(in) :: x1,x2
    real(dp),intent(in) :: f1,f2
    real(dp),intent(out) :: xbest
    real(dp),intent(out) :: fbest

    if (abs(f1)<abs(f2)) then
        xbest = x1
        fbest = f1
    else
        xbest = x2
        fbest = f2
    end if

  end subroutine choose_best_r8
!*****************************************************************************************

!*****************************************************************************************
!>
!  Bisection step.

    pure function bisect_r4(x1,x2) result(x3)
         !dir$ attributes forceinline :: bisect_r4
      !dir$ attributes code_align : 32 :: bisect_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: bisect_r4
    implicit none

    real(sp),intent(in) :: x1,x2
    real(sp) :: x3 !! point half way between x1 and x2

    x3 = (x1 + x2) * 0.5_sp

  end function bisect_r4

  pure function bisect_r8(x1,x2) result(x3)
         !dir$ attributes forceinline :: bisect_r8
      !dir$ attributes code_align : 32 :: bisect_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: bisect_r8
    implicit none

    real(dp),intent(in) :: x1,x2
    real(dp) :: x3 !! point half way between x1 and x2

    x3 = (x1 + x2) * 0.5_dp

    end function bisect_r8
!*****************************************************************************************

!*****************************************************************************************
!>
!  Regula Falsi step.
!  With a protection to fall back to bisection if:
!
!   * the computed point is outside the original interval ([ax,bx]).
!   * f2 == f1

    function regula_falsi_step_r4(x1,x2,f1,f2,ax,bx) result(x3)
      !dir$ attributes forceinline :: regula_falsi_step_r4
      !dir$ attributes code_align : 32 :: regula_falsi_step_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: regula_falsi_step_r4
    implicit none

    real(sp),intent(in) :: x1,x2,f1,f2
    real(sp),intent(in) :: ax !! original interval lower bound
    real(sp),intent(in) :: bx !! original interval upper bound
    real(sp) :: x3 !! intersection of line connecting x1,x2 with x-axis

    real(sp) :: delta

    delta = f2-f1

    if (delta /= 0.0_sp) then
        ! intersection with x-axis of line connecting the two points:
        x3 = x1 - (f1/delta) * (x2-x1)
        if (x3>ax .and. x3<bx) return ! must be a new point in the range
    end if

    ! fall back to bisection for any problem
    x3 = bisect_r4(x1,x2)

  end function regula_falsi_step_r4

  function regula_falsi_step_r8(x1,x2,f1,f2,ax,bx) result(x3)
      !dir$ attributes forceinline :: regula_falsi_step_r8
      !dir$ attributes code_align : 32 :: regula_falsi_step_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: regula_falsi_step_r8
    implicit none

    real(dp),intent(in) :: x1,x2,f1,f2
    real(dp),intent(in) :: ax !! original interval lower bound
    real(dp),intent(in) :: bx !! original interval upper bound
    real(dp) :: x3 !! intersection of line connecting x1,x2 with x-axis
    
    real(dp) :: delta

    delta = f2-f1

    if (delta /= 0.0_dp) then
        ! intersection with x-axis of line connecting the two points:
        x3 = x1 - (f1/delta) * (x2-x1)
        if (x3>ax .and. x3<bx) return ! must be a new point in the range
    end if

    ! fall back to bisection for any problem
    x3 = bisect_r8(x1,x2)

    end function regula_falsi_step_r4
!*****************************************************************************************

!*****************************************************************************************
!>
!  Secent step.
!  With a protection to fall back to bisection if:
!
!   * the computed point is outside the original interval ([ax,bx]).
!   * f2 == f1

    pure function secant_r4(x1,x2,f1,f2,ax,bx) result(x3)
      !dir$ attributes forceinline :: secant_r4
      !dir$ attributes code_align : 32 :: secant_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: secant_r4
    implicit none

    real(sp),intent(in) :: x1,x2,f1,f2
    real(sp),intent(in) :: ax !! original interval lower bound
    real(sp),intent(in) :: bx !! original interval upper bound
    real(sp) :: x3 !! intersection of secant step with x-axis

    if (f2==f1) then
        x3 = bisect_r4(x1,x2)
    else
        ! secant step:
        x3 = x2 - f2 / ( (f2 - f1) / (x2 - x1) )
        if (x3<ax .or. x3>bx) x3 = bisect_r4(x1,x2)
    end if

  end function secant_r4

  pure function secant_r8(x1,x2,f1,f2,ax,bx) result(x3)
      !dir$ attributes forceinline :: secant_r8
      !dir$ attributes code_align : 32 :: secant_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: secant_r8
    implicit none

    real(dp),intent(in) :: x1,x2,f1,f2
    real(dp),intent(in) :: ax !! original interval lower bound
    real(dp),intent(in) :: bx !! original interval upper bound
    real(dp) :: x3 !! intersection of secant step with x-axis

    if (f2==f1) then
        x3 = bisect_r8(x1,x2)
    else
        ! secant step:
        x3 = x2 - f2 / ( (f2 - f1) / (x2 - x1) )
        if (x3<ax .or. x3>bx) x3 = bisect_r8(x1,x2)
    end if

  end function secant_r8

  
!*****************************************************************************************

!*****************************************************************************************
!>
!  Swap two real(wp) values.

    pure elemental subroutine swap_r4(a,b)
       !dir$ attributes forceinline :: swap_r4
      !dir$ attributes code_align : 32 :: swap_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: swap_r4


    implicit none

    real(sp),intent(inout) :: a
    real(sp),intent(inout) :: b

    real(sp) :: tmp

    tmp = a
    a   = b
    b   = tmp

  end subroutine swap_r4

  pure elemental subroutine swap_r8(a,b)
       !dir$ attributes forceinline :: swap_r8
      !dir$ attributes code_align : 32 :: swap_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: swap_r8


    implicit none

    real(dp),intent(inout) :: a
    real(dp),intent(inout) :: b

    real(dp) :: tmp

    tmp = a
    a   = b
    b   = tmp

  end subroutine swap_r8

  
!*****************************************************************************************

!*****************************************************************************************
!>
!  lowercase a string.

  !  pure function lowercase(str) result(s_lower)

   ! implicit none

  !  character(len=*),intent(in) :: str      !! input string
  !  character(len=(len(str)))   :: s_lower  !! lowercase version of the string

  !  integer :: i  !! counter
  !  integer :: j  !! index of uppercase character

  !  character(len=*),parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !! uppercase characters
  !  character(len=*),parameter :: lower = 'abcdefghijklmnopqrstuvwxyz' !! lowercase characters

 !   s_lower = str

  !  do i = 1, len_trim(str)
  !      j = index(upper,s_lower(i:i))
  !      if (j>0) s_lower(i:i) = lower(j:j)
  !  end do

  !  end function lowercase
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if this is a solution and sets `xzero` and `fzero`.

    logical function solution_r4(x,f,ftol,xzero,fzero)
      !dir$ attributes forceinline :: solution_r4
      !dir$ attributes code_align : 32 :: solution_r4
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: solution_r4

    implicit none

    real(sp),intent(in) :: x
    real(sp),intent(in) :: f
    real(sp),intent(in) :: ftol
    real(sp),intent(inout) :: xzero
    real(sp),intent(inout) :: fzero

    if (abs(f) <= ftol) then
        xzero = x
        fzero = f
        solution_r4 = .true.
    else
        solution_r4 = .false.
    end if

  end function solution_r4

  logical function solution_r8(x,f,ftol,xzero,fzero)
      !dir$ attributes forceinline :: solution_r8
      !dir$ attributes code_align : 32 :: solution_r8
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=AVX :: solution_r8

    implicit none

    real(dp),intent(in) :: x
    real(dp),intent(in) :: f
    real(dp),intent(in) :: ftol
    real(dp),intent(inout) :: xzero
    real(dp),intent(inout) :: fzero

    if (abs(f) <= ftol) then
        xzero = x
        fzero = f
        solution_r8 = .true.
    else
        solution_r8 = .false.
    end if

    end function solution_r8
  
!*****************************************************************************************

!*****************************************************************************************
    end module root_finding
!*****************************************************************************************
