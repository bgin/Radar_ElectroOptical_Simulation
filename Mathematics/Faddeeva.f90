!!$ Copyright (c) 2012, Johannes Feist
!!$ licensed under the MIT open source license, see LICENSE file
!!$ This provides a Fortran interface to the open-source C/C++ codes
!!$ for computing various error functions and the Dawson integral
!!$ in the complex plane, based on algorithms for the computation
!!$ of the Faddeeva function
!!$           w(z) = exp(-z^2) * erfc(-i*z).
!!$ The original code by Steven G. Johnson is from http://ab-initio.mit.edu/Faddeeva
!!$
!!$ We call it from Fortran with the C bindings introduced in Fortran 2003. This is 
!!$ possible since Steven provides a C interface since 27 November 2012 in addition
!!$ to the original C++.
!!$ As default arguments are not supported in calling C functions from Fortran, 
!!$ we provide explicit wrappers with the missing relerr argument.
!!$
!!$   Johannes Feist, Universidad Autonoma de Madrid

module faddeeva
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: Faddeeva_w, Faddeeva_w_im, erfcx, erf, erfi, erfc, Dawson

  ! compute w(z) = exp(-z^2) erfc(-iz) [ Faddeeva / scaled complex error func ] 
  interface Faddeeva_w
     module procedure zFaddeeva_w_norelerr
     function zFaddeeva_w(z,relerr) bind(c,name='Faddeeva_w')
       use iso_c_binding
       implicit none
       complex(c_double) :: zFaddeeva_w
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function ZFaddeeva_w
  end interface Faddeeva_w

  interface
     ! special-case code for Im[w(x)] of real x
     function Faddeeva_w_im(x) bind(c,name='Faddeeva_w_im')
       use, intrinsic :: iso_c_binding
       real(c_double) :: Faddeeva_w_im
       real(c_double), value, intent(in) :: x
     end function Faddeeva_w_im
  end interface

  ! Various functions that we can compute with the help of w(z)

  ! compute erfcx(z) = exp(z^2) erfc(z)
  interface erfcx
     module procedure zerfcx_norelerr
     function zerfcx(z,relerr) bind(c,name='Faddeeva_erfcx')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(c_double) :: zerfcx
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function zerfcx
     function derfcx(x) bind(c,name='Faddeeva_erfcx_re')
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_double) :: derfcx
       real(c_double), value, intent(in) :: x
     end function derfcx
  end interface erfcx

  ! compute erf(z), the error function of complex arguments
  interface erf
     module procedure zerf_norelerr
     function zerf(z,relerr) bind(c,name='Faddeeva_erf')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(c_double) :: zerf
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function zerf
     function derf(x) bind(c,name='Faddeeva_erf_re')
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_double) :: derf
       real(c_double), value, intent(in) :: x
     end function derf
  end interface erf

  ! compute erfi(z) = -i erf(iz), the imaginary error function
  interface erfi
     module procedure zerfi_norelerr
     function zerfi(z,relerr) bind(c,name='Faddeeva_erfi')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(c_double) :: zerfi
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function zerfi
     function derfi(x) bind(c,name='Faddeeva_erfi_re')
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_double) :: derfi
       real(c_double), value, intent(in) :: x
     end function derfi
  end interface erfi

  ! compute erfc(z) = 1 - erf(z), the complementary error function
  interface erfc
     module procedure zerfc_norelerr
     function zerfc(z,relerr) bind(c,name='Faddeeva_erfc')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(c_double) :: zerfc
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function zerfc
     function derfc(x) bind(c,name='Faddeeva_erfc_re')
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_double) :: derfc
       real(c_double), value, intent(in) :: x
     end function derfc
  end interface erfc

  ! compute Dawson(z) = sqrt(pi)/2  *  exp(-z^2) * erfi(z)
  interface Dawson
     module procedure zDawson_norelerr
     function zDawson(z,relerr) bind(c,name='Faddeeva_Dawson')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(c_double) :: zDawson
       complex(c_double), value, intent(in) :: z
       real(c_double), value, intent(in) :: relerr
     end function zDawson
     function dDawson(x) bind(c,name='Faddeeva_Dawson_re')
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_double) :: dDawson
       real(c_double), value, intent(in) :: x
     end function dDawson
  end interface Dawson

contains
  function zFaddeeva_w_norelerr(z)
    complex(c_double) :: zFaddeeva_w_norelerr
    complex(c_double), intent(in) :: z
    zFaddeeva_w_norelerr = zFaddeeva_w(z,0.d0)
  end function zFaddeeva_w_norelerr

  function zerfcx_norelerr(z)
    complex(c_double) :: zerfcx_norelerr
    complex(c_double), intent(in) :: z
    zerfcx_norelerr = zerfcx(z,0.d0)
  end function zerfcx_norelerr

  function zerf_norelerr(z)
    complex(c_double) :: zerf_norelerr
    complex(c_double), intent(in) :: z
    zerf_norelerr = zerf(z,0.d0)
  end function zerf_norelerr

  function zerfi_norelerr(z)
    complex(c_double) :: zerfi_norelerr
    complex(c_double), intent(in) :: z
    zerfi_norelerr = zerfi(z,0.d0)
  end function zerfi_norelerr

  function zerfc_norelerr(z)
    complex(c_double) :: zerfc_norelerr
    complex(c_double), intent(in) :: z
    zerfc_norelerr = zerfc(z,0.d0)
  end function zerfc_norelerr

  function zDawson_norelerr(z)
    complex(c_double) :: zDawson_norelerr
    complex(c_double), intent(in) :: z
    zDawson_norelerr = zDawson(z,0.d0)
  end function zDawson_norelerr

end module faddeeva
