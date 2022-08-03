
module hankel_fargs
  
  !============================================================!
  ! To set and pass to hankel transform integrand an
  ! additional argument i.e. a 'q' value.
  ! QAGE integrators does not allow to pass more than one 
  ! argument to integrated integrands.
  !============================================================!
  
  use mod_kinds, only : i4, dp
  implicit none
  public
  
  real(kind=dp)     :: q,x,a,b,c,d ! where q is a main argument to integrand
  integer(kind=i4)  :: bkind !bessel kind switch
  character(len=12) :: fname ! function name for select statement
  contains

  ! Number of basic function to be transformed by the Hankel Transform

#if 0

flist = {UnitStep[a - r], 1/r, 1/Sqrt[1 + r^2], Log[1 + a^2/r^2], 
      Sin[r]/(1 + r^2), E^(-a r), E^(-r^2), 1/(E^(2 r^2)*r)}; 
Grid[Join[{{f[r], HankelTransform[f[r], r, s]}}, 
   Transpose[{flist, Map[HankelTransform[#, r, s] &, flist]}]], 
  Background -> {None, {{None, GrayLevel[.9]}}, {{1, 1} -> 
      Hue[.6, .4, 1], {1, 2} -> Hue[.6, .4, 1]}}, 
  BaseStyle -> {FontFamily -> Times, FontSize -> 12}, Dividers -> All,
   FrameStyle -> Hue[.6, .4, .8], 
  Spacings -> {2, 1}] // TraditionalForm

#endif
  
  ! Unit step
  pure function unit_step(x) result(Hz)
    !dir$ optimize:3
    !dir$ attributes inline :: unit_step
        real(kind=dp), intent(in) :: x
        real(kind=dp) :: Hz
        if(x>0.0_dp) then
           Hz = 1.0_dp
        else if(x<=0.0_dp) then
           Hz = 0.0_dp
        end if
  end function unit_step

  ! 1/r
  pure function inv_r(r) result(y)
    !dir$ optimize:3
    !dir$ attributes inline :: inv_r
       real(kind=dp),  intent(in) :: r
       real(kind=dp) :: y
       y = 1.0_r
  end function inv_r

  ! 1/sqrt(r^2+1)
  pure function inv_sqrtr(r) result(y)
     !dir$ optimize:3
     !dir$ attributes inline :: inv_sqrtr
     real(kind=dp),  intent(in) :: r
     real(kind=dp) :: y
     y = 1.0_dp/(sqrt(1.0_dp+r*r))
  end function inv_sqrtr

  
  ! Log(1+a^2/r^2)
  pure function log_ar(a,r) result(y)
     !dir$ optimize:3
     !dir$ attributes inline :: log_ar
     real(kind=dp), intent(in) :: a
     real(kind=dp), intent(in) :: r
     real(kind=dp) :: y
     real(kind=dp), automatic :: a2,r2
     a2 = a*a;r2 = r*r
     y = log(1.0_dp+a2/r2)
  end function log_ar

  ! Sin[r]/(1 + r^2)
  pure function sinc(r) result(y)
     !dir$ optimize:3
     !dir$ attributes inline :: sinc
     real(kind=dp), intent(in) :: r
     real(kind=dp) :: y
     real(kind=dp), automatic:: r2
     r2 = r*r
     y = sin(r)/(1.0_dp+r2)
  end function sinc

  !E^(-a r)
  pure function expar(a,r) result(y)
     !dir$ optimize:3
     !dir$ attributes inline :: expar
     real(kind=dp), intent(in) :: a
     real(kind=dp), intent(in) :: r
     real(kind=dp) :: y
     y = exp(-a*r)
  end function expar

  !1/(E^(2 r^2)*r)
  pure function invexp(r) result(y)
     !dir$ optimize:3
     !dir$ attributes inline :: invexp
     real(kind=dp), intent(in) :: r
     real(kind=dp) :: y
     real(kind=dp), automatic :: r2
     r2 = r*r
     y = 1.0_dp/(exp(r2+r2)*r)
  end function invexp


  ! Few pre-defined integrands bessel kind: 0
  pure function integrand_0(r) result(y)
     !dir$ optimize:3
     real(kind=dp), intent(in) :: r
     real(kind=dp) :: y
     real(kind=dp), automatic :: t
     real(kind=dp), parameter :: twopi = 6.283185307179586476925286766559_dp
     select case(fname)
       case("unit_step")
            t = unit_step(r)
            y = t*bessel_jn(0,twopi*r*q)
       case("inv_r")
            t = inv_r(r)
            y = t*bessel_jn(0,twopi*r*q)
       case("sinc")
            t = sinc(r)
            y = t*bessel_jn(0,twopi*r*q)
       case("expar")
            t = expar(a,r)
            y = t*bessel_jn(0,twopi*r*q)
       case("invexp")
            t = invexp(r)
            y = t*bessel_jn(0,twopi*r*q)
      end select
  end function integrand_1
  

end module hankel_fargs
