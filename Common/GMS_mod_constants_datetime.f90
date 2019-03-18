
!
! datetime-fortran - A Fortran library for date and time manipulation
! Copyright (c) 2013-2016, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
!
module mod_constants_datetime
!=======================================================================
!
! mod_constants: Basic constants and time conversion factors.
!
!=======================================================================

!use,intrinsic :: iso_fortran_env,only : real32,real64
 use mod_kinds, only : sp,dp

implicit none

private

public :: zero,one,d2h,h2d,d2m,m2d,m2h,s2d,d2s,h2s,s2h,m2s,s2m,MAXSTRLEN

real(kind=dp),parameter :: zero = 0.0_dp !! 0
real(kind=dp),parameter :: one  = 1.0_dp !! 1

! Constant multipliers that transform a number
! of some time unit to another:
real(kind=dp),parameter :: d2h  = 24.0_dp     !! day    -> hour
real(kind=dp),parameter :: h2d  = one/d2h         !! hour   -> day
real(kind=dp),parameter :: d2m  = d2h*60.0_dp !! day    -> minute
real(kind=dp),parameter :: m2d  = one/d2m         !! minute -> day
real(kind=dp),parameter :: m2h  = one/60.0_dp !! minute -> hour
real(kind=dp),parameter :: s2d  = m2d/60.0_dp !! second -> day
real(kind=dp),parameter :: d2s  = 86400.0_dp  !! day    -> second
real(kind=dp),parameter :: h2s  = 3600.0_dp  !! hour   -> second
real(kind=dp),parameter :: s2h  = one/h2s         !! second -> hour
real(kind=dp),parameter :: m2s  = 60.0_dp     !! minute -> second
real(kind=dp),parameter :: s2m  = one/m2s         !! second -> minute

! Maximum string length for strftime.
! Constant for now; may become a preprocessor macro later.
integer,parameter :: MAXSTRLEN = 99

!=======================================================================
endmodule mod_constants_datetime
