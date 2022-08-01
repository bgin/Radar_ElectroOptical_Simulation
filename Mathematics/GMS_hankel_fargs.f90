
module hankel_fargs
  
  !============================================================!
  ! To set and pass to hankel transform integrand an
  ! additional argument i.e. a 'q' value.
  ! QAGE integrators does not allow to pass more than one 
  ! argument to integrated integrands.
  !============================================================!
  
  use mod_kinds, only : dp
  implicit none
  public
  
  real(kind=dp) :: q

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

end module hankel_fargs
