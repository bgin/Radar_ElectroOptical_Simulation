module gauss_quad_formulas_mod
!!==============================================================================
! This module defines Gaussian quadrature formulas.
!  
!   An n-point formula for triangles is given as a nx4 dimensioned array on the
!   format:
!
!     weight_1  ksi_1  eta_1  zeta_1  
!     weight_2  ksi_2  eta_2  zeta_2
!         .       .      .      .   
!         .       .      .      .   
!         .       .      .      .   
!     weight_n  ksi_n  eta_n  zeta_n
!
!   Where ksi_i, eta_i, and zeta_i are normalised area coordinates.
!
! Abbreviations:
!   GQ  - Gaussian Quadrature
!   GQF - Gaussian Quadrature Formula
!
! Last edited: November 9th 2020.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp

  implicit none

  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  
  !-----------------------------------------------------------------------------
  ! Lebedev quadrature for integrating functions over a unit sphere.
  ! 
  ! Computes and returns a Lebedev angular grid including x, y, z coordinates
  ! and weight w, given its order.
  !  Input: (order). Output: (x(order), y(order), z(order), w(order))
  public :: ld_by_order
  ! The following orders are available through the following routines
  ! LD0006 computes the 6 point Lebedev angular grid.
  ! LD0014 computes the 14 point Lebedev angular grid.
  ! LD0026 computes the 26 point Lebedev angular grid.
  ! LD0038 computes the 38 point Lebedev angular grid.
  ! LD0050 computes the 50 point Lebedev angular grid.
  ! LD0074 computes the 74 point Lebedev angular grid.
  ! LD0086 computes the 86 point Lebedev angular grid.
  ! LD0110 computes the 110 point Lebedev angular grid.
  ! LD0146 computes the 146 point Lebedev angular grid.
  ! LD0170 computes the 170 point Lebedev angular grid.
  ! LD0194 computes the 194 point Lebedev angular grid.
  ! LD0230 computes the 230 point Lebedev angular grid.
  ! LD0266 computes the 266 point Lebedev angular grid.
  ! LD0302 computes the 302 point Lebedev angular grid.
  ! LD0350 computes the 350 point Lebedev angular grid.
  ! LD0434 computes the 434 point Lebedev angular grid.
  ! LD0590 computes the 590 point Lebedev angular grid.
  ! LD0770 computes the 770 point Lebedev angular grid.
  ! LD0974 computes the 974 point Lebedev angular grid.
  
  !-----------------------------------------------------------------------------
  ! Gaussian quadrature formulas for triangles
  real(wp), parameter, dimension(1, 4), public :: GQF_triangle_1pnt = &
       transpose(reshape([&
       ! Triangle. 1-point formula. Degree of precision 1 (?)
       1._wp, 1._wp/3._wp, 1._wp/3._wp, 1._wp/3._wp ], [4, 1]))

  real(wp), parameter, dimension(3, 4), public :: GQF_triangle_3pnt = &
       transpose(reshape([&
       ! Triangle. 3-point formula. Degree of precision 2
       1._wp/3._wp, 2._wp/3._wp, 1._wp/6._wp, 1._wp/6._wp, &
       1._wp/3._wp, 1._wp/6._wp, 2._wp/3._wp, 1._wp/6._wp, &
       1._wp/3._wp, 1._wp/6._wp, 1._wp/6._wp, 2._wp/3._wp ], [4, 3]))
  
  real(wp), parameter, dimension(4, 4), public :: GQF_triangle_4pnt = &
       transpose(reshape([&
       ! Triangle. 4-point formula. Degree of precision 3
       -0.5625_wp, 1._wp/3._wp, 1._wp/3._wp, 1._wp/3._wp, &
       0.520833333333333_wp,      0.6_wp,      0.2_wp,      0.2_wp, &
       0.520833333333333_wp,      0.2_wp,      0.6_wp,      0.2_wp, &
       0.520833333333333_wp,      0.2_wp,      0.2_wp,      0.6_wp ], [4, 4]))
  
  real(wp), parameter, dimension(6, 4), public :: GQF_triangle_6pnt = &
       transpose(reshape([&
       ! Triangle. 6-point formula. Degree of precision 3
1._wp/6._wp, 0.659027622374092_wp, 0.231933368553031_wp, 0.109039009072877_wp, & 
1._wp/6._wp, 0.659027622374092_wp, 0.109039009072877_wp, 0.231933368553031_wp, &
1._wp/6._wp, 0.109039009072877_wp, 0.231933368553031_wp, 0.659027622374092_wp, &
1._wp/6._wp, 0.109039009072877_wp, 0.659027622374092_wp, 0.231933368553031_wp, &
1._wp/6._wp, 0.231933368553031_wp, 0.109039009072877_wp, 0.659027622374092_wp, &
1._wp/6._wp, 0.231933368553031_wp, 0.659027622374092_wp, 0.109039009072877_wp ] &
       , [4, 6]))
  
  real(wp), parameter, dimension(7, 4), public :: GQF_triangle_7pnt = &
       transpose(reshape([&
       ! Triangle. 7-point formula. Degree of precision 4
       0.375_wp            , & ! Multiplicity 1
       1._wp/3._wp         , &
       1._wp/3._wp         , &
       1._wp/3._wp         , &
       !
       !
       0.104166666666667_wp, & ! 1/6 multiplicity
       0.736712498968435_wp, &
       0.237932366472434_wp, &
       0.025355134559132_wp, &
       !
       0.104166666666667_wp, & ! 2/6 multiplicity
       0.736712498968435_wp, &
       0.025355134559132_wp, &
       0.237932366472434_wp, &
       !
       0.104166666666667_wp, & ! 3/6 multiplicity
       0.025355134559132_wp, &
       0.237932366472434_wp, &
       0.736712498968435_wp, &
       !
       0.104166666666667_wp, & ! 4/6 multiplicity
       0.025355134559132_wp, &
       0.736712498968435_wp, &
       0.237932366472434_wp, &
       !
       0.104166666666667_wp, & ! 5/6 multiplicity
       0.237932366472434_wp, &
       0.736712498968435_wp, &
       0.025355134559132_wp, &
       !
       0.104166666666667_wp, & ! 6/6 multiplicity
       0.237932366472434_wp, &
       0.025355134559132_wp, &
       0.736712498968435_wp  ], [4, 7]))
  
  real(wp), parameter, dimension(9, 4), public :: GQF_triangle_9pnt = &
       transpose(reshape([&
       ! Triangle. 9-point formula. Degree of precision 5
       0.205950504760887_wp, & ! Multiplicity 3
       0.124949503233232_wp, &
       0.437525248383384_wp, &
       0.437525248383384_wp, &
       !
       0.205950504760887_wp, &
       0.437525248383384_wp, &
       0.124949503233232_wp, &
       0.437525248383384_wp, &
       !
       0.205950504760887_wp, &
       0.437525248383384_wp, &
       0.437525248383384_wp, &
       0.124949503233232_wp, &
       !
       !
       0.063691414286223_wp, & ! Multiplicity 6
       0.797112651860071_wp, &
       0.165409927389841_wp, &
       0.037477420750088_wp, &
       !
       0.063691414286223_wp, & 
       0.797112651860071_wp, &
       0.037477420750088_wp, &
       0.165409927389841_wp, &
       !
       0.063691414286223_wp, & 
       0.037477420750088_wp, &
       0.165409927389841_wp, &
       0.797112651860071_wp, &
       !
       0.063691414286223_wp, & 
       0.037477420750088_wp, &
       0.797112651860071_wp, &
       0.165409927389841_wp, &
       !
       0.063691414286223_wp, & 
       0.165409927389841_wp, &
       0.797112651860071_wp, &
       0.037477420750088_wp, &
       !
       0.063691414286223_wp, & 
       0.165409927389841_wp, &
       0.037477420750088_wp, &
       0.797112651860071_wp  ], [4, 9]))
   
  real(wp), parameter, dimension(12, 4), public :: GQF_triangle_12pnt = &
       transpose(reshape([&
       ! Triangle. 12-point formula. Degree of precision 6
       0.050844906370207_wp, & ! Multiplicity 3
       0.873821971016996_wp, &
       0.063089014491502_wp, &
       0.063089014491502_wp, &
       !
       0.050844906370207_wp, & 
       0.063089014491502_wp, &
       0.873821971016996_wp, &
       0.063089014491502_wp, &
       !
       0.050844906370207_wp, &
       0.063089014491502_wp, &
       0.063089014491502_wp, &
       0.873821971016996_wp, &
       !
       !
       0.116786275726379_wp, & ! Multipliciy 3
       0.501426509658179_wp, &
       0.249286745170910_wp, &
       0.249286745170911_wp, &
       !
       0.116786275726379_wp, &
       0.249286745170910_wp, &
       0.501426509658179_wp, &
       0.249286745170911_wp, &
       !
       0.116786275726379_wp, &
       0.249286745170910_wp, &
       0.249286745170911_wp, &
       0.501426509658179_wp, &
       !
       !
       0.082851075618374_wp, & ! Multiplicity 6
       0.636502499121399_wp, &
       0.310352451033785_wp, &
       0.053145049844816_wp, &
       !
       0.082851075618374_wp, &
       0.636502499121399_wp, &
       0.053145049844816_wp, &
       0.310352451033785_wp, &
       !
       0.082851075618374_wp, &
       0.310352451033785_wp, &
       0.636502499121399_wp, &
       0.053145049844816_wp, &
       !
       0.082851075618374_wp, &
       0.310352451033785_wp, &
       0.053145049844816_wp, &
       0.636502499121399_wp, &
       !
       0.082851075618374_wp, &
       0.053145049844816_wp, &
       0.636502499121399_wp, &
       0.310352451033785_wp, &
       !
       0.082851075618374_wp, &
       0.053145049844816_wp, &
       0.310352451033785_wp, &
       0.636502499121399_wp  ], [4, 12]))

  real(wp), parameter, dimension(13, 4), public :: GQF_triangle_13pnt = &
       transpose(reshape([&
       ! Triangle. 13-point formula. Degree of precision 7
       -0.149570044467670_wp, & ! Multiplicity 1
       1._wp/3._wp          , &
       1._wp/3._wp          , &
       1._wp/3._wp          , &
       !
       !
       0.175615257433204_wp, & ! Multiplicity 3
       0.479308067841923_wp, &
       0.260345966079038_wp, &
       0.260345966079038_wp, &
       !
       0.175615257433204_wp, & 
       0.260345966079038_wp, &
       0.479308067841923_wp, &
       0.260345966079038_wp, &
       !
       0.175615257433204_wp, & 
       0.260345966079038_wp, &
       0.260345966079038_wp, &
       0.479308067841923_wp, &
       !
       !
       0.053347235608839_wp, & ! Multiplicity 3
       0.869739794195568_wp, &
       0.065130102902216_wp, &
       0.065130102902216_wp, &
       !
       0.053347235608839_wp, & 
       0.065130102902216_wp, &
       0.869739794195568_wp, &
       0.065130102902216_wp, &
       !
       0.053347235608839_wp, & 
       0.065130102902216_wp, &
       0.065130102902216_wp, &
       0.869739794195568_wp, &
       !
       !
       0.077113760890257_wp, & ! Multiplicity 6
       0.638444188569809_wp, &
       0.312865496004875_wp, &
       0.048690315425316_wp, &
       !
       0.077113760890257_wp, & 
       0.638444188569809_wp, &
       0.048690315425316_wp, &
       0.312865496004875_wp, &
       !
       0.077113760890257_wp, & 
       0.312865496004875_wp, &
       0.638444188569809_wp, &
       0.048690315425316_wp, &
       !
       0.077113760890257_wp, & 
       0.312865496004875_wp, &
       0.048690315425316_wp, &
       0.638444188569809_wp, &
       !
       0.077113760890257_wp, & 
       0.048690315425316_wp, &
       0.638444188569809_wp, &
       0.312865496004875_wp, &
       !
       0.077113760890257_wp, & 
       0.048690315425316_wp, &
       0.312865496004875_wp, &
       0.638444188569809_wp  ], [4, 13]))

!!$        
  ! 1-dimensional Gauss Legendre formulas
  real(wp), parameter, dimension(1, 2), public :: GQF_Legendre_1pnt = &
       transpose(reshape([&
       2._wp, 0._wp], [2, 1]))
  real(wp), parameter, dimension(2, 2), public :: GQF_Legendre_2pnt = &
       transpose(reshape([&
       1._wp, -1._wp/sqrt(3._wp), &
       1._wp,  1._wp/sqrt(3._wp) ], [2, 2]))
  real(wp), parameter, dimension(3, 2), public :: GQF_Legendre_3pnt = &
       transpose(reshape([&
       5._wp/9._wp, -sqrt(3._wp/5._wp), &
       8._wp/9._wp,              0._wp, &
       5._wp/9._wp,  sqrt(3._wp/5._wp) ], [2, 3]))
  real(wp), parameter, dimension(4, 2), public :: GQF_Legendre_4pnt = &
       transpose(reshape([&
       0.3478548451374539_wp, -0.8611363115940526_wp, &
       0.6521451548625462_wp, -0.3399810435848563_wp, &
       0.6521451548625462_wp,  0.3399810435848563_wp, &
       0.3478548451374539_wp,  0.8611363115940526_wp  ], [2, 4]))
  real(wp), parameter, dimension(5, 2), public :: GQF_Legendre_5pnt = &
       transpose(reshape([&
       0.23692688505618908_wp, -0.906179845938664_wp, &
       0.47862867049936647_wp, -0.538469310105683_wp, &
       0.5688888888888889_wp ,  0._wp               , &
       0.47862867049936647_wp,  0.538469310105683_wp, &
       0.23692688505618908_wp,  0.906179845938664_wp  ], [2, 5]))
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  subroutine gen_oh ( code, num, a, b, v, x, y, z, w )

    !***************************************************************************
    !
    !! GEN_OH generates points under OH symmetry.
    !
    !  Discussion:
    !
    !    Given a point on a sphere, specified by A and B, this routine generates
    !    all the equivalent points under OH symmetry, making grid points with
    !    weight V.
    !
    !    The variable NUM is increased by the number of different points
    !    generated.
    !
    !    Depending on CODE, there are from 6 to 48 different but equivalent
    !    points that are generated:
    !
    !      CODE=1:   (0,0,1) etc                                (  6 points)
    !      CODE=2:   (0,A,A) etc, A=1/sqrt(2)                   ( 12 points)
    !      CODE=3:   (A,A,A) etc, A=1/sqrt(3)                   (  8 points)
    !      CODE=4:   (A,A,B) etc, B=sqrt(1-2 A^2)               ( 24 points)
    !      CODE=5:   (A,B,0) etc, B=sqrt(1-A^2), A input        ( 24 points)
    !      CODE=6:   (A,B,C) etc, C=sqrt(1-A^2-B^2), A, B input ( 48 points)
    !
    !  Modified:
    !
    !    11 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) CODE, selects the symmetry group.
    !
    !    Input/output, integer ( kind = 4 ) NUM, presumably a counter for the 
    !    total number of points.  It is incremented by the number of points 
    !    generated on this call.
    !
    !    Input, real ( kind = 8 ) A, B, information that may be needed to
    !    generate the coordinates of the points (for code = 5 or 6 only).
    !
    !    Input, real ( kind = 8 ) V, the weight to be assigned the points.
    !
    !    Output, real ( kind = 8 ) X(NUM), Y(NUM), Z(NUM), W(NUM), the coordinates
    !    and weights of the symmetric points generated on this call.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    real ( kind = 8 ) c
    integer ( kind = 4 ) code
    integer ( kind = 4 ) num
    real ( kind = 8 ) v
    real ( kind = 8 ) w(*)
    real ( kind = 8 ) x(*)
    real ( kind = 8 ) y(*)
    real ( kind = 8 ) z(*)
    
    if ( code == 1 ) then
       
       a = 1.0D+000
       x(1) =  a
       y(1) =  0.0D+000
       z(1) =  0.0D+000
       w(1) =  v
       x(2) = -a
       y(2) =  0.0D+000
       z(2) =  0.0D+000
       w(2) =  v
       x(3) =  0.0D+000
       y(3) =  a
       z(3) =  0.0D+000
       w(3) =  v
       x(4) =  0.0D+000
       y(4) = -a
       z(4) =  0.0D+000
       w(4) =  v
       x(5) =  0.0D+000
       y(5) =  0.0D+000
       z(5) =  a
       w(5) =  v
       x(6) =  0.0D+000
       y(6) =  0.0D+000
       z(6) = -a
       w(6) =  v
       num = num + 6
       
    else if ( code == 2 ) then
       
       a = sqrt ( 0.5D+000 )
       x( 1) =  0.0D+000
       y( 1) =  a
       z( 1) =  a
       w( 1) =  v
       x( 2) =  0.0D+000
       y( 2) = -a
       z( 2) =  a
       w( 2) =  v
       x( 3) =  0.0D+000
       y( 3) =  a
       z( 3) = -a
       w( 3) =  v
       x( 4) =  0.0D+000
       y( 4) = -a
       z( 4) = -a
       w( 4) =  v
       x( 5) =  a
       y( 5) =  0.0D+000
       z( 5) =  a
       w( 5) =  v
       x( 6) = -a
       y( 6) =  0.0D+000
       z( 6) =  a
       w( 6) =  v
       x( 7) =  a
       y( 7) =  0.0D+000
       z( 7) = -a
       w( 7) =  v
       x( 8) = -a
       y( 8) =  0.0D+000
       z( 8) = -a
       w( 8) =  v
       x( 9) =  a
       y( 9) =  a
       z( 9) =  0.0D+000
       w( 9) =  v
       x(10) = -a
       y(10) =  a
       z(10) =  0.0D+000
       w(10) =  v
       x(11) =  a
       y(11) = -a
       z(11) =  0.0D+000
       w(11) =  v
       x(12) = -a
       y(12) = -a
       z(12) =  0.0D+000
       w(12) =  v
       num = num + 12
       
    else if ( code == 3 ) then
       
       a = sqrt ( 1.0D+000 / 3.0D+000 )
       x(1) =  a
       y(1) =  a
       z(1) =  a
       w(1) =  v
       x(2) = -a
       y(2) =  a
       z(2) =  a
       w(2) =  v
       x(3) =  a
       y(3) = -a
       z(3) =  a
       w(3) =  v
       x(4) = -a
       y(4) = -a
       z(4) =  a
       w(4) =  v
       x(5) =  a
       y(5) =  a
       z(5) = -a
       w(5) =  v
       x(6) = -a
       y(6) =  a
       z(6) = -a
       w(6) =  v
       x(7) =  a
       y(7) = -a
       z(7) = -a
       w(7) =  v
       x(8) = -a
       y(8) = -a
       z(8) = -a
       w(8) =  v
       num = num + 8
       
    else if ( code == 4 ) then
       
       b = sqrt ( 1.0D+000 - 2.0D+000 * a * a )
       x( 1) =  a
       y( 1) =  a
       z( 1) =  b
       w( 1) =  v
       x( 2) = -a
       y( 2) =  a
       z( 2) =  b
       w( 2) =  v
       x( 3) =  a
       y( 3) = -a
       z( 3) =  b
       w( 3) =  v
       x( 4) = -a
       y( 4) = -a
       z( 4) =  b
       w( 4) =  v
       x( 5) =  a
       y( 5) =  a
       z( 5) = -b
       w( 5) =  v
       x( 6) = -a
       y( 6) =  a
       z( 6) = -b
       w( 6) =  v
       x( 7) =  a
       y( 7) = -a
       z( 7) = -b
       w( 7) =  v
       x( 8) = -a
       y( 8) = -a
       z( 8) = -b
       w( 8) =  v
       x( 9) =  a
       y( 9) =  b
       z( 9) =  a
       w( 9) =  v
       x(10) = -a
       y(10) =  b
       z(10) =  a
       w(10) =  v
       x(11) =  a
       y(11) = -b
       z(11) =  a
       w(11) =  v
       x(12) = -a
       y(12) = -b
       z(12) =  a
       w(12) =  v
       x(13) =  a
       y(13) =  b
       z(13) = -a
       w(13) =  v
       x(14) = -a
       y(14) =  b
       z(14) = -a
       w(14) =  v
       x(15) =  a
       y(15) = -b
       z(15) = -a
       w(15) =  v
       x(16) = -a
       y(16) = -b
       z(16) = -a
       w(16) =  v
       x(17) =  b
       y(17) =  a
       z(17) =  a
       w(17) =  v
       x(18) = -b
       y(18) =  a
       z(18) =  a
       w(18) =  v
       x(19) =  b
       y(19) = -a
       z(19) =  a
       w(19) =  v
       x(20) = -b
       y(20) = -a
       z(20) =  a
       w(20) =  v
       x(21) =  b
       y(21) =  a
       z(21) = -a
       w(21) =  v
       x(22) = -b
       y(22) =  a
       z(22) = -a
       w(22) =  v
       x(23) =  b
       y(23) = -a
       z(23) = -a
       w(23) =  v
       x(24) = -b
       y(24) = -a
       z(24) = -a
       w(24) =  v
       num = num + 24
       
    else if ( code == 5 ) then
       
       b = sqrt ( 1.0D+000 - a * a )
       x( 1) =  a
       y( 1) =  b
       z( 1) =  0.0D+000
       w( 1) =  v
       x( 2) = -a
       y( 2) =  b
       z( 2) =  0.0D+000
       w( 2) =  v
       x( 3) =  a
       y( 3) = -b
       z( 3) =  0.0D+000
       w( 3) =  v
       x( 4) = -a
       y( 4) = -b
       z( 4) =  0.0D+000
       w( 4) =  v
       x( 5) =  b
       y( 5) =  a
       z( 5) =  0.0D+000
       w( 5) =  v
       x( 6) = -b
       y( 6) =  a
       z( 6) =  0.0D+000
       w( 6) =  v
       x( 7) =  b
       y( 7) = -a
       z( 7) =  0.0D+000
       w( 7) =  v
       x( 8) = -b
       y( 8) = -a
       z( 8) =  0.0D+000
       w( 8) =  v
       x( 9) =  a
       y( 9) =  0.0D+000
       z( 9) =  b
       w( 9) =  v
       x(10) = -a
       y(10) =  0.0D+000
       z(10) =  b
       w(10) =  v
       x(11) =  a
       y(11) =  0.0D+000
       z(11) = -b
       w(11) =  v
       x(12) = -a
       y(12) =  0.0D+000
       z(12) = -b
       w(12) =  v
       x(13) =  b
       y(13) =  0.0D+000
       z(13) =  a
       w(13) =  v
       x(14) = -b
       y(14) =  0.0D+000
       z(14) =  a
       w(14) =  v
       x(15) =  b
       y(15) =  0.0D+000
       z(15) = -a
       w(15) =  v
       x(16) = -b
       y(16) =  0.0D+000
       z(16) = -a
       w(16) =  v
       x(17) =  0.0D+000
       y(17) =  a
       z(17) =  b
       w(17) =  v
       x(18) =  0.0D+000
       y(18) = -a
       z(18) =  b
       w(18) =  v
       x(19) =  0.0D+000
       y(19) =  a
       z(19) = -b
       w(19) =  v
       x(20) =  0.0D+000
       y(20) = -a
       z(20) = -b
       w(20) =  v
       x(21) =  0.0D+000
       y(21) =  b
       z(21) =  a
       w(21) =  v
       x(22) =  0.0D+000
       y(22) = -b
       z(22) =  a
       w(22) =  v
       x(23) =  0.0D+000
       y(23) =  b
       z(23) = -a
       w(23) =  v
       x(24) =  0.0D+000
       y(24) = -b
       z(24) = -a
       w(24) =  v
       num = num + 24
       
    else if ( code == 6 ) then
       
       c = sqrt ( 1.0D+000 - a * a - b * b )
       x( 1) =  a
       y( 1) =  b
       z( 1) =  c
       w( 1) =  v
       x( 2) = -a
       y( 2) =  b
       z( 2) =  c
       w( 2) =  v
       x( 3) =  a
       y( 3) = -b
       z( 3) =  c
       w( 3) =  v
       x( 4) = -a
       y( 4) = -b
       z( 4) =  c
       w( 4) =  v
       x( 5) =  a
       y( 5) =  b
       z( 5) = -c
       w( 5) =  v
       x( 6) = -a
       y( 6) =  b
       z( 6) = -c
       w( 6) =  v
       x( 7) =  a
       y( 7) = -b
       z( 7) = -c
       w( 7) =  v
       x( 8) = -a
       y( 8) = -b
       z( 8) = -c
       w( 8) =  v
       x( 9) =  a
       y( 9) =  c
       z( 9) =  b
       w( 9) =  v
       x(10) = -a
       y(10) =  c
       z(10) =  b
       w(10) =  v
       x(11) =  a
       y(11) = -c
       z(11) =  b
       w(11) =  v
       x(12) = -a
       y(12) = -c
       z(12) =  b
       w(12) =  v
       x(13) =  a
       y(13) =  c
       z(13) = -b
       w(13) =  v
       x(14) = -a
       y(14) =  c
       z(14) = -b
       w(14) =  v
       x(15) =  a
       y(15) = -c
       z(15) = -b
       w(15) =  v
       x(16) = -a
       y(16) = -c
       z(16) = -b
       w(16) =  v
       x(17) =  b
       y(17) =  a
       z(17) =  c
       w(17) =  v
       x(18) = -b
       y(18) =  a
       z(18) =  c
       w(18) =  v
       x(19) =  b
       y(19) = -a
       z(19) =  c
       w(19) =  v
       x(20) = -b
       y(20) = -a
       z(20) =  c
       w(20) =  v
       x(21) =  b
       y(21) =  a
       z(21) = -c
       w(21) =  v
       x(22) = -b
       y(22) =  a
       z(22) = -c
       w(22) =  v
       x(23) =  b
       y(23) = -a
       z(23) = -c
       w(23) =  v
       x(24) = -b
       y(24) = -a
       z(24) = -c
       w(24) =  v
       x(25) =  b
       y(25) =  c
       z(25) =  a
       w(25) =  v
       x(26) = -b
       y(26) =  c
       z(26) =  a
       w(26) =  v
       x(27) =  b
       y(27) = -c
       z(27) =  a
       w(27) =  v
       x(28) = -b
       y(28) = -c
       z(28) =  a
       w(28) =  v
       x(29) =  b
       y(29) =  c
       z(29) = -a
       w(29) =  v
       x(30) = -b
       y(30) =  c
       z(30) = -a
       w(30) =  v
       x(31) =  b
       y(31) = -c
       z(31) = -a
       w(31) =  v
       x(32) = -b
       y(32) = -c
       z(32) = -a
       w(32) =  v
       x(33) =  c
       y(33) =  a
       z(33) =  b
       w(33) =  v
       x(34) = -c
       y(34) =  a
       z(34) =  b
       w(34) =  v
       x(35) =  c
       y(35) = -a
       z(35) =  b
       w(35) =  v
       x(36) = -c
       y(36) = -a
       z(36) =  b
       w(36) =  v
       x(37) =  c
       y(37) =  a
       z(37) = -b
       w(37) =  v
       x(38) = -c
       y(38) =  a
       z(38) = -b
       w(38) =  v
       x(39) =  c
       y(39) = -a
       z(39) = -b
       w(39) =  v
       x(40) = -c
       y(40) = -a
       z(40) = -b
       w(40) =  v
       x(41) =  c
       y(41) =  b
       z(41) =  a
       w(41) =  v
       x(42) = -c
       y(42) =  b
       z(42) =  a
       w(42) =  v
       x(43) =  c
       y(43) = -b
       z(43) =  a
       w(43) =  v
       x(44) = -c
       y(44) = -b
       z(44) =  a
       w(44) =  v
       x(45) =  c
       y(45) =  b
       z(45) = -a
       w(45) =  v
       x(46) = -c
       y(46) =  b
       z(46) = -a
       w(46) =  v
       x(47) =  c
       y(47) = -b
       z(47) = -a
       w(47) =  v
       x(48) = -c
       y(48) = -b
       z(48) = -a
       w(48) =  v
       num = num + 48
       
    else
       
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'GEN_OH - Fatal error!'
       write ( *, '(a)' ) '  Illegal value of CODE.'
       stop
       
    end if
    
    return
  end subroutine gen_oh
  !-----------------------------------------------------------------------------
  
  subroutine ld_by_order ( order, x, y, z, w )
    
    !***************************************************************************
    !
    !! LD_BY_ORDER returns a Lebedev angular grid given its order.
    !
    !  Discussion:
    !
    !    Only a certain set of such rules are available through this function.
    !
    !  Modified:
    !
    !    13 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) ORDER, the order of the rule.
    !
    !    Output, real ( kind = 8 ) X(ORDER), Y(ORDER), Z(ORDER), W(ORDER), 
    !    the coordinates and weights of the points.
    !
    implicit none
    
    integer ( kind = 4 ) order
    
    real ( kind = 8 ) w(order)
    real ( kind = 8 ) x(order)
    real ( kind = 8 ) y(order)
    real ( kind = 8 ) z(order)
    
    if ( order == 6 ) then
       call ld0006 ( x, y, z, w )
    else if ( order == 14 ) then
       call ld0014 ( x, y, z, w )
    else if ( order == 26 ) then
       call ld0026 ( x, y, z, w )
    else if ( order == 38 ) then
       call ld0038 ( x, y, z, w )
    else if ( order == 50 ) then
       call ld0050 ( x, y, z, w )
    else if ( order == 74 ) then
       call ld0074 ( x, y, z, w )
    else if ( order == 86 ) then
       call ld0086 ( x, y, z, w )
    else if ( order == 110 ) then
       call ld0110 ( x, y, z, w )
    else if ( order == 146 ) then
       call ld0146 ( x, y, z, w )
    else if ( order == 170 ) then
       call ld0170 ( x, y, z, w )
    else if ( order == 194 ) then
       call ld0194 ( x, y, z, w )
    else if ( order == 230 ) then
       call ld0230 ( x, y, z, w )
    else if ( order == 266 ) then
       call ld0266 ( x, y, z, w )
    else if ( order == 302 ) then
       call ld0302 ( x, y, z, w )
    else if ( order == 350 ) then
       call ld0350 ( x, y, z, w )
    else if ( order == 434 ) then
       call ld0434 ( x, y, z, w )
    else if ( order == 590 ) then
       call ld0590 ( x, y, z, w )
    else if ( order == 770 ) then
       call ld0770 ( x, y, z, w )
    else if ( order == 974 ) then
       call ld0974 ( x, y, z, w )
    else if ( order == 5810 ) then
       call ld5810 ( x, y, z, w )
    else
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'LD_BY_ORDER - Fatal error!'
       write ( *, '(a)' ) '  Unexpected value of ORDER.'
       stop
    end if
    
    return
  end subroutine ld_by_order
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0006 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0006 computes the 6 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(6)
    real ( kind = 8 ) x(6)
    real ( kind = 8 ) y(6)
    real ( kind = 8 ) z(6)
    
    n = 1
    v = 0.1666666666666667D+00
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0006

  !-----------------------------------------------------------------------------
  
  subroutine ld0014 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0014 computes the 14 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(14)
    real ( kind = 8 ) x(14)
    real ( kind = 8 ) y(14)
    real ( kind = 8 ) z(14)
    
    n = 1
    v = 0.6666666666666667D-01
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.7500000000000000D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0014
  subroutine ld0026 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0026 computes the 26 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(26)
    real ( kind = 8 ) x(26)
    real ( kind = 8 ) y(26)
    real ( kind = 8 ) z(26)
    
    n = 1
    v = 0.4761904761904762D-01
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.3809523809523810D-01
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.3214285714285714D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0026
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0038 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0038 computes the 38 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(38)
    real ( kind = 8 ) x(38)
    real ( kind = 8 ) y(38)
    real ( kind = 8 ) z(38)
    
    n = 1
    v = 0.9523809523809524D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.3214285714285714D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4597008433809831D+00
    v = 0.2857142857142857D-01
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0038
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0050 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0050 computes the 50 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(50)
    real ( kind = 8 ) x(50)
    real ( kind = 8 ) y(50)
    real ( kind = 8 ) z(50)
    
    n = 1
    v = 0.1269841269841270D-01
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.2257495590828924D-01
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.2109375000000000D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3015113445777636D+00
    v = 0.2017333553791887D-01
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0050

  !-----------------------------------------------------------------------------
  
  subroutine ld0074 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0074 computes the 74 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(74)
    real ( kind = 8 ) x(74)
    real ( kind = 8 ) y(74)
    real ( kind = 8 ) z(74)
    
    n = 1
    v = 0.5130671797338464D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1660406956574204D-01
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = -0.2958603896103896D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4803844614152614D+00
    v = 0.2657620708215946D-01
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3207726489807764D+00
    v = 0.1652217099371571D-01
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0074
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0086 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0086 computes the 86 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(86)
    real ( kind = 8 ) x(86)
    real ( kind = 8 ) y(86)
    real ( kind = 8 ) z(86)
    
    n = 1
    v = 0.1154401154401154D-01
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1194390908585628D-01
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3696028464541502D+00
    v = 0.1111055571060340D-01
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6943540066026664D+00
    v = 0.1187650129453714D-01
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3742430390903412D+00
    v = 0.1181230374690448D-01
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0086
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0110 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0110 computes the 110 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(110)
    real ( kind = 8 ) x(110)
    real ( kind = 8 ) y(110)
    real ( kind = 8 ) z(110)
    
    n = 1
    v = 0.3828270494937162D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.9793737512487512D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1851156353447362D+00
    v = 0.8211737283191111D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6904210483822922D+00
    v = 0.9942814891178103D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3956894730559419D+00
    v = 0.9595471336070963D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4783690288121502D+00
    v = 0.9694996361663028D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0110
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0146 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0146 computes the 146 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(146)
    real ( kind = 8 ) x(146)
    real ( kind = 8 ) y(146)
    real ( kind = 8 ) z(146)
    
    n = 1
    v = 0.5996313688621381D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.7372999718620756D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.7210515360144488D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6764410400114264D+00
    v = 0.7116355493117555D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4174961227965453D+00
    v = 0.6753829486314477D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1574676672039082D+00
    v = 0.7574394159054034D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1403553811713183D+00
    b = 0.4493328323269557D+00
    v = 0.6991087353303262D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0146
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0170 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0170 computes the 170 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(170)
    real ( kind = 8 ) x(170)
    real ( kind = 8 ) y(170)
    real ( kind = 8 ) z(170)
    
    n = 1
    v = 0.5544842902037365D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.6071332770670752D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.6383674773515093D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2551252621114134D+00
    v = 0.5183387587747790D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6743601460362766D+00
    v = 0.6317929009813725D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4318910696719410D+00
    v = 0.6201670006589077D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2613931360335988D+00
    v = 0.5477143385137348D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4990453161796037D+00
    b = 0.1446630744325115D+00
    v = 0.5968383987681156D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0170
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0194 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0194 computes the 194 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(194)
    real ( kind = 8 ) x(194)
    real ( kind = 8 ) y(194)
    real ( kind = 8 ) z(194)
    
    n = 1
    v = 0.1782340447244611D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.5716905949977102D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.5573383178848738D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6712973442695226D+00
    v = 0.5608704082587997D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2892465627575439D+00
    v = 0.5158237711805383D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4446933178717437D+00
    v = 0.5518771467273614D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1299335447650067D+00
    v = 0.4106777028169394D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3457702197611283D+00
    v = 0.5051846064614808D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1590417105383530D+00
    b = 0.8360360154824589D+00
    v = 0.5530248916233094D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0194
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0230 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0230 computes the 230 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(230)
    real ( kind = 8 ) x(230)
    real ( kind = 8 ) y(230)
    real ( kind = 8 ) z(230)
    
    n = 1
    v = -0.5522639919727325D-01
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.4450274607445226D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4492044687397611D+00
    v = 0.4496841067921404D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2520419490210201D+00
    v = 0.5049153450478750D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6981906658447242D+00
    v = 0.3976408018051883D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6587405243460960D+00
    v = 0.4401400650381014D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4038544050097660D-01
    v = 0.1724544350544401D-01
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5823842309715585D+00
    v = 0.4231083095357343D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3545877390518688D+00
    v = 0.5198069864064399D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2272181808998187D+00
    b = 0.4864661535886647D+00
    v = 0.4695720972568883D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0230
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0266 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0266 computes the 266 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(266)
    real ( kind = 8 ) x(266)
    real ( kind = 8 ) y(266)
    real ( kind = 8 ) z(266)
    
    n = 1
    v = -0.1313769127326952D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = -0.2522728704859336D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.4186853881700583D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7039373391585475D+00
    v = 0.5315167977810885D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1012526248572414D+00
    v = 0.4047142377086219D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4647448726420539D+00
    v = 0.4112482394406990D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3277420654971629D+00
    v = 0.3595584899758782D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6620338663699974D+00
    v = 0.4256131351428158D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.8506508083520399D+00
    v = 0.4229582700647240D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3233484542692899D+00
    b = 0.1153112011009701D+00
    v = 0.4080914225780505D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2314790158712601D+00
    b = 0.5244939240922365D+00
    v = 0.4071467593830964D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0266
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0302 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0302 computes the 302 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(302)
    real ( kind = 8 ) x(302)
    real ( kind = 8 ) y(302)
    real ( kind = 8 ) z(302)
    
    n = 1
    v = 0.8545911725128148D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.3599119285025571D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3515640345570105D+00
    v = 0.3449788424305883D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6566329410219612D+00
    v = 0.3604822601419882D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4729054132581005D+00
    v = 0.3576729661743367D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.9618308522614784D-01
    v = 0.2352101413689164D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2219645236294178D+00
    v = 0.3108953122413675D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7011766416089545D+00
    v = 0.3650045807677255D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2644152887060663D+00
    v = 0.2982344963171804D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5718955891878961D+00
    v = 0.3600820932216460D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2510034751770465D+00
    b = 0.8000727494073952D+00
    v = 0.3571540554273387D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1233548532583327D+00
    b = 0.4127724083168531D+00
    v = 0.3392312205006170D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0302
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0350 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0350 computes the 350 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(350)
    real ( kind = 8 ) x(350)
    real ( kind = 8 ) y(350)
    real ( kind = 8 ) z(350)
    
    n = 1
    v = 0.3006796749453936D-02
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.3050627745650771D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7068965463912316D+00
    v = 0.1621104600288991D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4794682625712025D+00
    v = 0.3005701484901752D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1927533154878019D+00
    v = 0.2990992529653774D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6930357961327123D+00
    v = 0.2982170644107595D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3608302115520091D+00
    v = 0.2721564237310992D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6498486161496169D+00
    v = 0.3033513795811141D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1932945013230339D+00
    v = 0.3007949555218533D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3800494919899303D+00
    v = 0.2881964603055307D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2899558825499574D+00
    b = 0.7934537856582316D+00
    v = 0.2958357626535696D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.9684121455103957D-01
    b = 0.8280801506686862D+00
    v = 0.3036020026407088D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1833434647041659D+00
    b = 0.9074658265305127D+00
    v = 0.2832187403926303D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0350
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0434 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0434 computes the 434 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(434)
    real ( kind = 8 ) x(434)
    real ( kind = 8 ) y(434)
    real ( kind = 8 ) z(434)
    
    n = 1
    v = 0.5265897968224436D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.2548219972002607D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.2512317418927307D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6909346307509111D+00
    v = 0.2530403801186355D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1774836054609158D+00
    v = 0.2014279020918528D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4914342637784746D+00
    v = 0.2501725168402936D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6456664707424256D+00
    v = 0.2513267174597564D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2861289010307638D+00
    v = 0.2302694782227416D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7568084367178018D-01
    v = 0.1462495621594614D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3927259763368002D+00
    v = 0.2445373437312980D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.8818132877794288D+00
    v = 0.2417442375638981D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.9776428111182649D+00
    v = 0.1910951282179532D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2054823696403044D+00
    b = 0.8689460322872412D+00
    v = 0.2416930044324775D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5905157048925271D+00
    b = 0.7999278543857286D+00
    v = 0.2512236854563495D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5550152361076807D+00
    b = 0.7717462626915901D+00
    v = 0.2496644054553086D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.9371809858553722D+00
    b = 0.3344363145343455D+00
    v = 0.2236607760437849D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0434
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0590 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0590 computes the 590 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(590)
    real ( kind = 8 ) x(590)
    real ( kind = 8 ) y(590)
    real ( kind = 8 ) z(590)
    
    n = 1
    v = 0.3095121295306187D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1852379698597489D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7040954938227469D+00
    v = 0.1871790639277744D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6807744066455243D+00
    v = 0.1858812585438317D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6372546939258752D+00
    v = 0.1852028828296213D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5044419707800358D+00
    v = 0.1846715956151242D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4215761784010967D+00
    v = 0.1818471778162769D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3317920736472123D+00
    v = 0.1749564657281154D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2384736701421887D+00
    v = 0.1617210647254411D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1459036449157763D+00
    v = 0.1384737234851692D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6095034115507196D-01
    v = 0.9764331165051050D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6116843442009876D+00
    v = 0.1857161196774078D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3964755348199858D+00
    v = 0.1705153996395864D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1724782009907724D+00
    v = 0.1300321685886048D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5610263808622060D+00
    b = 0.3518280927733519D+00
    v = 0.1842866472905286D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4742392842551980D+00
    b = 0.2634716655937950D+00
    v = 0.1802658934377451D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5984126497885380D+00
    b = 0.1816640840360209D+00
    v = 0.1849830560443660D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3791035407695563D+00
    b = 0.1720795225656878D+00
    v = 0.1713904507106709D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2778673190586244D+00
    b = 0.8213021581932511D-01
    v = 0.1555213603396808D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5033564271075117D+00
    b = 0.8999205842074875D-01
    v = 0.1802239128008525D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0590
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0770 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0770 computes the 770 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(770)
    real ( kind = 8 ) x(770)
    real ( kind = 8 ) y(770)
    real ( kind = 8 ) z(770)
    
    n = 1
    v = 0.2192942088181184D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1436433617319080D-02
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1421940344335877D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5087204410502360D-01
    v = 0.6798123511050502D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1228198790178831D+00
    v = 0.9913184235294912D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2026890814408786D+00
    v = 0.1180207833238949D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2847745156464294D+00
    v = 0.1296599602080921D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3656719078978026D+00
    v = 0.1365871427428316D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4428264886713469D+00
    v = 0.1402988604775325D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5140619627249735D+00
    v = 0.1418645563595609D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6306401219166803D+00
    v = 0.1421376741851662D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6716883332022612D+00
    v = 0.1423996475490962D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6979792685336881D+00
    v = 0.1431554042178567D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1446865674195309D+00
    v = 0.9254401499865368D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3390263475411216D+00
    v = 0.1250239995053509D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5335804651263506D+00
    v = 0.1394365843329230D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6944024393349413D-01
    b = 0.2355187894242326D+00
    v = 0.1127089094671749D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2269004109529460D+00
    b = 0.4102182474045730D+00
    v = 0.1345753760910670D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.8025574607775339D-01
    b = 0.6214302417481605D+00
    v = 0.1424957283316783D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1467999527896572D+00
    b = 0.3245284345717394D+00
    v = 0.1261523341237750D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1571507769824727D+00
    b = 0.5224482189696630D+00
    v = 0.1392547106052696D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2365702993157246D+00
    b = 0.6017546634089558D+00
    v = 0.1418761677877656D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7714815866765732D-01
    b = 0.4346575516141163D+00
    v = 0.1338366684479554D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3062936666210730D+00
    b = 0.4908826589037616D+00
    v = 0.1393700862676131D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3822477379524787D+00
    b = 0.5648768149099500D+00
    v = 0.1415914757466932D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0770
  
  !-----------------------------------------------------------------------------
  
  subroutine ld0974 ( x, y, z, w )
    
    !***************************************************************************
    !
    !! LD0974 computes the 974 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(974)
    real ( kind = 8 ) x(974)
    real ( kind = 8 ) y(974)
    real ( kind = 8 ) z(974)
    
    n = 1
    v = 0.1438294190527431D-03
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1125772288287004D-02
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4292963545341347D-01
    v = 0.4948029341949241D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1051426854086404D+00
    v = 0.7357990109125470D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1750024867623087D+00
    v = 0.8889132771304384D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2477653379650257D+00
    v = 0.9888347838921435D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3206567123955957D+00
    v = 0.1053299681709471D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3916520749849983D+00
    v = 0.1092778807014578D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4590825874187624D+00
    v = 0.1114389394063227D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5214563888415861D+00
    v = 0.1123724788051555D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6253170244654199D+00
    v = 0.1125239325243814D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6637926744523170D+00
    v = 0.1126153271815905D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6910410398498301D+00
    v = 0.1130286931123841D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7052907007457760D+00
    v = 0.1134986534363955D-02
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1236686762657990D+00
    v = 0.6823367927109931D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2940777114468387D+00
    v = 0.9454158160447096D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4697753849207649D+00
    v = 0.1074429975385679D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6334563241139567D+00
    v = 0.1129300086569132D-02
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5974048614181342D-01
    b = 0.2029128752777523D+00
    v = 0.8436884500901954D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1375760408473636D+00
    b = 0.4602621942484054D+00
    v = 0.1075255720448885D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3391016526336286D+00
    b = 0.5030673999662036D+00
    v = 0.1108577236864462D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1271675191439820D+00
    b = 0.2817606422442134D+00
    v = 0.9566475323783357D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2693120740413512D+00
    b = 0.4331561291720157D+00
    v = 0.1080663250717391D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1419786452601918D+00
    b = 0.6256167358580814D+00
    v = 0.1126797131196295D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6709284600738255D-01
    b = 0.3798395216859157D+00
    v = 0.1022568715358061D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7057738183256172D-01
    b = 0.5517505421423520D+00
    v = 0.1108960267713108D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2783888477882155D+00
    b = 0.6029619156159187D+00
    v = 0.1122790653435766D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1979578938917407D+00
    b = 0.3589606329589096D+00
    v = 0.1032401847117460D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2087307061103274D+00
    b = 0.5348666438135476D+00
    v = 0.1107249382283854D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4055122137872836D+00
    b = 0.5674997546074373D+00
    v = 0.1121780048519972D-02
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1
    
    return
  end subroutine ld0974
  
  !-----------------------------------------------------------------------------
  
  subroutine ld5810 ( x, y, z, w )
    
    !*****************************************************************************80
    !
    !! LD5810 computes the 5810 point Lebedev angular grid.
    !
    !  Modified:
    !
    !    09 September 2010
    !
    !  Author:
    !
    !    Dmitri Laikov
    !
    !  Reference:
    !
    !    Vyacheslav Lebedev, Dmitri Laikov,
    !    A quadrature formula for the sphere of the 131st
    !    algebraic order of accuracy,
    !    Russian Academy of Sciences Doklady Mathematics,
    !    Volume 59, Number 3, 1999, pages 477-481.
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) X(N), Y(N), Z(N), W(N), the coordinates
    !    and weights of the points.
    !
    implicit none
    
    real ( kind = 8 ) a
    real ( kind = 8 ) b
    integer ( kind = 4 ) n
    real ( kind = 8 ) v
    real ( kind = 8 ) w(5810)
    real ( kind = 8 ) x(5810)
    real ( kind = 8 ) y(5810)
    real ( kind = 8 ) z(5810)
    
    n = 1
    v = 0.9735347946175486D-05
    call gen_oh ( 1, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1907581241803167D-03
    call gen_oh ( 2, n, a, b, v, x(n), y(n), z(n), w(n) )
    v = 0.1901059546737578D-03
    call gen_oh ( 3, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1182361662400277D-01
    v = 0.3926424538919212D-04
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3062145009138958D-01
    v = 0.6667905467294382D-04
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5329794036834243D-01
    v = 0.8868891315019135D-04
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7848165532862220D-01
    v = 0.1066306000958872D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1054038157636201D+00
    v = 0.1214506743336128D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1335577797766211D+00
    v = 0.1338054681640871D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1625769955502252D+00
    v = 0.1441677023628504D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1921787193412792D+00
    v = 0.1528880200826557D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2221340534690548D+00
    v = 0.1602330623773609D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2522504912791132D+00
    v = 0.1664102653445244D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2823610860679697D+00
    v = 0.1715845854011323D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3123173966267560D+00
    v = 0.1758901000133069D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3419847036953789D+00
    v = 0.1794382485256736D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3712386456999758D+00
    v = 0.1823238106757407D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3999627649876828D+00
    v = 0.1846293252959976D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4280466458648093D+00
    v = 0.1864284079323098D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4553844360185711D+00
    v = 0.1877882694626914D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4818736094437834D+00
    v = 0.1887716321852025D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5074138709260629D+00
    v = 0.1894381638175673D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5319061304570707D+00
    v = 0.1898454899533629D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5552514978677286D+00
    v = 0.1900497929577815D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5981009025246183D+00
    v = 0.1900671501924092D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6173990192228116D+00
    v = 0.1899837555533510D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6351365239411131D+00
    v = 0.1899014113156229D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6512010228227200D+00
    v = 0.1898581257705106D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6654758363948120D+00
    v = 0.1898804756095753D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6778410414853370D+00
    v = 0.1899793610426402D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6881760887484110D+00
    v = 0.1901464554844117D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6963645267094598D+00
    v = 0.1903533246259542D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7023010617153579D+00
    v = 0.1905556158463228D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.7059004636628753D+00
    v = 0.1907037155663528D-03
    call gen_oh ( 4, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3552470312472575D-01
    v = 0.5992997844249967D-04
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.9151176620841283D-01
    v = 0.9749059382456978D-04
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1566197930068980D+00
    v = 0.1241680804599158D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2265467599271907D+00
    v = 0.1437626154299360D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2988242318581361D+00
    v = 0.1584200054793902D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3717482419703886D+00
    v = 0.1694436550982744D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4440094491758889D+00
    v = 0.1776617014018108D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5145337096756642D+00
    v = 0.1836132434440077D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5824053672860230D+00
    v = 0.1876494727075983D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6468283961043370D+00
    v = 0.1899906535336482D-03
    call gen_oh ( 5, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6095964259104373D-01
    b = 0.1787828275342931D-01
    v = 0.8143252820767350D-04
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.8811962270959388D-01
    b = 0.3953888740792096D-01
    v = 0.9998859890887728D-04
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1165936722428831D+00
    b = 0.6378121797722990D-01
    v = 0.1156199403068359D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1460232857031785D+00
    b = 0.8985890813745037D-01
    v = 0.1287632092635513D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1761197110181755D+00
    b = 0.1172606510576162D+00
    v = 0.1398378643365139D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2066471190463718D+00
    b = 0.1456102876970995D+00
    v = 0.1491876468417391D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2374076026328152D+00
    b = 0.1746153823011775D+00
    v = 0.1570855679175456D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2682305474337051D+00
    b = 0.2040383070295584D+00
    v = 0.1637483948103775D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2989653312142369D+00
    b = 0.2336788634003698D+00
    v = 0.1693500566632843D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3294762752772209D+00
    b = 0.2633632752654219D+00
    v = 0.1740322769393633D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3596390887276086D+00
    b = 0.2929369098051601D+00
    v = 0.1779126637278296D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3893383046398812D+00
    b = 0.3222592785275512D+00
    v = 0.1810908108835412D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4184653789358347D+00
    b = 0.3512004791195743D+00
    v = 0.1836529132600190D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4469172319076166D+00
    b = 0.3796385677684537D+00
    v = 0.1856752841777379D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4745950813276976D+00
    b = 0.4074575378263879D+00
    v = 0.1872270566606832D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5014034601410262D+00
    b = 0.4345456906027828D+00
    v = 0.1883722645591307D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5272493404551239D+00
    b = 0.4607942515205134D+00
    v = 0.1891714324525297D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5520413051846366D+00
    b = 0.4860961284181720D+00
    v = 0.1896827480450146D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5756887237503077D+00
    b = 0.5103447395342790D+00
    v = 0.1899628417059528D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1225039430588352D+00
    b = 0.2136455922655793D-01
    v = 0.1123301829001669D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1539113217321372D+00
    b = 0.4520926166137188D-01
    v = 0.1253698826711277D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1856213098637712D+00
    b = 0.7086468177864818D-01
    v = 0.1366266117678531D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2174998728035131D+00
    b = 0.9785239488772918D-01
    v = 0.1462736856106918D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2494128336938330D+00
    b = 0.1258106396267210D+00
    v = 0.1545076466685412D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2812321562143480D+00
    b = 0.1544529125047001D+00
    v = 0.1615096280814007D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3128372276456111D+00
    b = 0.1835433512202753D+00
    v = 0.1674366639741759D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3441145160177973D+00
    b = 0.2128813258619585D+00
    v = 0.1724225002437900D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3749567714853510D+00
    b = 0.2422913734880829D+00
    v = 0.1765810822987288D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4052621732015610D+00
    b = 0.2716163748391453D+00
    v = 0.1800104126010751D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4349335453522385D+00
    b = 0.3007127671240280D+00
    v = 0.1827960437331284D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4638776641524965D+00
    b = 0.3294470677216479D+00
    v = 0.1850140300716308D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4920046410462687D+00
    b = 0.3576932543699155D+00
    v = 0.1867333507394938D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5192273554861704D+00
    b = 0.3853307059757764D+00
    v = 0.1880178688638289D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5454609081136522D+00
    b = 0.4122425044452694D+00
    v = 0.1889278925654758D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5706220661424140D+00
    b = 0.4383139587781027D+00
    v = 0.1895213832507346D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5946286755181518D+00
    b = 0.4634312536300553D+00
    v = 0.1898548277397420D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.1905370790924295D+00
    b = 0.2371311537781979D-01
    v = 0.1349105935937341D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2242518717748009D+00
    b = 0.4917878059254806D-01
    v = 0.1444060068369326D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2577190808025936D+00
    b = 0.7595498960495142D-01
    v = 0.1526797390930008D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2908724534927187D+00
    b = 0.1036991083191100D+00
    v = 0.1598208771406474D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3236354020056219D+00
    b = 0.1321348584450234D+00
    v = 0.1659354368615331D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3559267359304543D+00
    b = 0.1610316571314789D+00
    v = 0.1711279910946440D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3876637123676956D+00
    b = 0.1901912080395707D+00
    v = 0.1754952725601440D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4187636705218842D+00
    b = 0.2194384950137950D+00
    v = 0.1791247850802529D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4491449019883107D+00
    b = 0.2486155334763858D+00
    v = 0.1820954300877716D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4787270932425445D+00
    b = 0.2775768931812335D+00
    v = 0.1844788524548449D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5074315153055574D+00
    b = 0.3061863786591120D+00
    v = 0.1863409481706220D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5351810507738336D+00
    b = 0.3343144718152556D+00
    v = 0.1877433008795068D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5619001025975381D+00
    b = 0.3618362729028427D+00
    v = 0.1887444543705232D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5875144035268046D+00
    b = 0.3886297583620408D+00
    v = 0.1894009829375006D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6119507308734495D+00
    b = 0.4145742277792031D+00
    v = 0.1897683345035198D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2619733870119463D+00
    b = 0.2540047186389353D-01
    v = 0.1517327037467653D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.2968149743237949D+00
    b = 0.5208107018543989D-01
    v = 0.1587740557483543D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3310451504860488D+00
    b = 0.7971828470885599D-01
    v = 0.1649093382274097D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3646215567376676D+00
    b = 0.1080465999177927D+00
    v = 0.1701915216193265D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3974916785279360D+00
    b = 0.1368413849366629D+00
    v = 0.1746847753144065D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4295967403772029D+00
    b = 0.1659073184763559D+00
    v = 0.1784555512007570D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4608742854473447D+00
    b = 0.1950703730454614D+00
    v = 0.1815687562112174D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4912598858949903D+00
    b = 0.2241721144376724D+00
    v = 0.1840864370663302D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5206882758945558D+00
    b = 0.2530655255406489D+00
    v = 0.1860676785390006D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5490940914019819D+00
    b = 0.2816118409731066D+00
    v = 0.1875690583743703D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5764123302025542D+00
    b = 0.3096780504593238D+00
    v = 0.1886453236347225D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6025786004213506D+00
    b = 0.3371348366394987D+00
    v = 0.1893501123329645D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6275291964794956D+00
    b = 0.3638547827694396D+00
    v = 0.1897366184519868D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3348189479861771D+00
    b = 0.2664841935537443D-01
    v = 0.1643908815152736D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.3699515545855295D+00
    b = 0.5424000066843495D-01
    v = 0.1696300350907768D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4042003071474669D+00
    b = 0.8251992715430854D-01
    v = 0.1741553103844483D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4375320100182624D+00
    b = 0.1112695182483710D+00
    v = 0.1780015282386092D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4699054490335947D+00
    b = 0.1402964116467816D+00
    v = 0.1812116787077125D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5012739879431952D+00
    b = 0.1694275117584291D+00
    v = 0.1838323158085421D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5315874883754966D+00
    b = 0.1985038235312689D+00
    v = 0.1859113119837737D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5607937109622117D+00
    b = 0.2273765660020893D+00
    v = 0.1874969220221698D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5888393223495521D+00
    b = 0.2559041492849764D+00
    v = 0.1886375612681076D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6156705979160163D+00
    b = 0.2839497251976899D+00
    v = 0.1893819575809276D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6412338809078123D+00
    b = 0.3113791060500690D+00
    v = 0.1897794748256767D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4076051259257167D+00
    b = 0.2757792290858463D-01
    v = 0.1738963926584846D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4423788125791520D+00
    b = 0.5584136834984293D-01
    v = 0.1777442359873466D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4760480917328258D+00
    b = 0.8457772087727143D-01
    v = 0.1810010815068719D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5085838725946297D+00
    b = 0.1135975846359248D+00
    v = 0.1836920318248129D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5399513637391218D+00
    b = 0.1427286904765053D+00
    v = 0.1858489473214328D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5701118433636380D+00
    b = 0.1718112740057635D+00
    v = 0.1875079342496592D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5990240530606021D+00
    b = 0.2006944855985351D+00
    v = 0.1887080239102310D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6266452685139695D+00
    b = 0.2292335090598907D+00
    v = 0.1894905752176822D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6529320971415942D+00
    b = 0.2572871512353714D+00
    v = 0.1898991061200695D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.4791583834610126D+00
    b = 0.2826094197735932D-01
    v = 0.1809065016458791D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5130373952796940D+00
    b = 0.5699871359683649D-01
    v = 0.1836297121596799D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5456252429628476D+00
    b = 0.8602712528554394D-01
    v = 0.1858426916241869D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5768956329682385D+00
    b = 0.1151748137221281D+00
    v = 0.1875654101134641D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6068186944699046D+00
    b = 0.1442811654136362D+00
    v = 0.1888240751833503D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6353622248024907D+00
    b = 0.1731930321657680D+00
    v = 0.1896497383866979D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6624927035731797D+00
    b = 0.2017619958756061D+00
    v = 0.1900775530219121D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5484933508028488D+00
    b = 0.2874219755907391D-01
    v = 0.1858525041478814D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.5810207682142106D+00
    b = 0.5778312123713695D-01
    v = 0.1876248690077947D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6120955197181352D+00
    b = 0.8695262371439526D-01
    v = 0.1889404439064607D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6416944284294319D+00
    b = 0.1160893767057166D+00
    v = 0.1898168539265290D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6697926391731260D+00
    b = 0.1450378826743251D+00
    v = 0.1902779940661772D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6147594390585488D+00
    b = 0.2904957622341456D-01
    v = 0.1890125641731815D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6455390026356783D+00
    b = 0.5823809152617197D-01
    v = 0.1899434637795751D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6747258588365477D+00
    b = 0.8740384899884715D-01
    v = 0.1904520856831751D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    a = 0.6772135750395347D+00
    b = 0.2919946135808105D-01
    v = 0.1905534498734563D-03
    call gen_oh ( 6, n, a, b, v, x(n), y(n), z(n), w(n) )
    n = n - 1

    return
  end subroutine ld5810

  !-----------------------------------------------------------------------------
  
end module gauss_quad_formulas_mod
