!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.

module mod_const

!use mod_precision,only : ik => intkind,rk => realkind
use mod_kinds, only : int4, dp

implicit none

private

integer(kind=int4),dimension(1),parameter,public :: WAVY_OMNIDIRECTIONAL = [1]
integer(kind=int4),dimension(1,1),parameter,public :: WAVY_DIRECTIONAL = reshape([1],[1,1])
integer(kind=int4),dimension(1,1,1),parameter,public :: WAVY_DIRECTIONAL_2D = reshape([1],[1,1,1])

real(kind=dp),parameter,public :: WAVY_REAL = 1._dp
integer(kind=int4),parameter,public :: WAVY_INT = 1

integer(kind=int4),parameter,public :: huge_int = huge(1_int4)
real(kind=dp),parameter,public :: tiny_real = tiny(1e0_dp)
real(kind=dp),parameter,public :: huge_real = huge(1e0_dp)

real(kind=dp),parameter,public :: eps = tiny(1e0_dp)
real(kind=dp),parameter,public :: pi = 3.14159265358979323846264338327950_dp
real(kind=dp),parameter,public :: twopi = 2*pi

integer(kind=int4),parameter,public :: stdout = 6
integer(kind=int4),parameter,public :: stderr = 0

endmodule mod_const
